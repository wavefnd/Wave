// This file is part of the Wave language project.
// SPDX-License-Identifier: MPL-2.0
//! Freestanding arithmetic legalization. Helpers contain no host calls and use
//! only operations the selected target can lower without a runtime SDK.
mod templates;
use super::target::CodegenTarget;
use inkwell::{
    context::Context,
    memory_buffer::MemoryBuffer,
    module::{Linkage, Module},
    values::{AnyValue, BasicValue, InstructionOpcode, InstructionValue},
};
use std::collections::HashMap;

/// Run after optimization so unused operations never pull in runtime support.
/// Functions are private and cached by operation, independently of user symbols.
pub(super) fn lower<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    target: CodegenTarget,
) -> Result<(), String> {
    let builder = context.create_builder();
    let mut helpers = HashMap::new();
    loop {
        let instructions: Vec<_> = module
            .get_functions()
            .flat_map(|f| f.get_basic_blocks())
            .flat_map(|b| b.get_instructions())
            .filter(|i| operation(*i, target).is_some())
            .collect();
        if instructions.is_empty() {
            break;
        }
        for instruction in instructions {
            let Some(key) = operation(instruction, target) else {
                continue;
            };
            let function = if let Some(function) = helpers.get(&key) {
                *function
            } else {
                let mut name = format!("__wave.runtime.{key}");
                while module.get_function(&name).is_some() || module.get_global(&name).is_some() {
                    name.push('.');
                }
                let text = templates::definition(&name, &key);
                let helper = context
                    .create_module_from_ir(MemoryBuffer::create_from_memory_range_copy(
                        text.as_bytes(),
                        "wave-runtime",
                    ))
                    .map_err(|e| format!("invalid compiler arithmetic runtime: {e}"))?;
                helper.set_triple(&module.get_triple());
                helper.set_data_layout(&module.get_data_layout());
                module.link_in_module(helper).map_err(|e| e.to_string())?;
                let function = module.get_function(&name).expect("linked arithmetic helper");
                function.set_linkage(Linkage::Private);
                helpers.insert(key.clone(), function);
                function
            };
            builder.position_before(&instruction);
            let arguments: Vec<_> =
                instruction.get_operands().map(|op| op.unwrap().unwrap_value().into()).collect();
            let call =
                builder.build_call(function, &arguments, "runtime").map_err(|e| e.to_string())?;
            instruction.replace_all_uses_with(
                &call.try_as_basic_value().basic().unwrap().as_instruction_value().unwrap(),
            );
            instruction.erase_from_basic_block();
        }
    }
    module.verify().map_err(|e| e.to_string())
}

fn operation(instruction: InstructionValue<'_>, target: CodegenTarget) -> Option<String> {
    use InstructionOpcode::*;
    if !matches!(
        instruction.get_opcode(),
        UDiv | SDiv | URem | SRem | Mul | Shl | LShr | AShr | UIToFP | SIToFP | FPToUI | FPToSI
    ) {
        return None;
    }
    // LLVM's 64-bit native targets expand i128 multiply and shifts into native
    // instructions. WebAssembly requires explicit limb legalization for these.
    let wasm = matches!(
        target,
        CodegenTarget::Wasm32Unknown | CodegenTarget::Wasm32WasiP1 | CodegenTarget::Wasm64Unknown
    );
    if !wasm && matches!(instruction.get_opcode(), Mul | Shl | LShr | AShr) {
        return None;
    }
    if !wasm && matches!(instruction.get_opcode(), UDiv | SDiv | URem | SRem) {
        // LLVM's native DAG lowering expands power-of-two divisors into
        // shifts/masks (with signed rounding adjustments), even at O0.
        if let Some(divisor) =
            instruction.get_operand(1).and_then(|v| v.value()).filter(|v| v.is_int_value())
        {
            let divisor = divisor.into_int_value();
            if divisor.is_const() {
                let text = divisor.print_to_string().to_string();
                if let Some(value) =
                    text.split_whitespace().last().and_then(|s| s.parse::<i128>().ok())
                {
                    let magnitude = if matches!(instruction.get_opcode(), SDiv | SRem) {
                        value.unsigned_abs()
                    } else {
                        value as u128
                    };
                    if magnitude.is_power_of_two() {
                        return None;
                    }
                }
            }
        }
    }
    let source = instruction.get_operand(0)?.value()?.get_type().print_to_string().to_string();
    let target = instruction.get_type().print_to_string().to_string();
    let opcode = match instruction.get_opcode() {
        UDiv if source == "i128" => "udiv",
        SDiv if source == "i128" => "sdiv",
        URem if source == "i128" => "urem",
        SRem if source == "i128" => "srem",
        Mul if source == "i128" => "mul",
        Shl | LShr | AShr
            if source == "i128"
                && !instruction.get_operand(1)?.value()?.into_int_value().is_const() =>
        {
            match instruction.get_opcode() {
                Shl => "shl",
                LShr => "lshr",
                AShr => "ashr",
                _ => unreachable!(),
            }
        },
        UIToFP if source == "i128" && matches!(target.as_str(), "float" | "double") => "uitofp",
        SIToFP if source == "i128" && matches!(target.as_str(), "float" | "double") => "sitofp",
        FPToUI if target == "i128" && matches!(source.as_str(), "float" | "double") => "fptoui",
        FPToSI if target == "i128" && matches!(source.as_str(), "float" | "double") => "fptosi",
        _ => return None,
    };
    Some(format!("{opcode}.{source}.{target}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn preserves_vector_division_created_by_the_optimizer() {
        let context = Context::create();
        let module = context.create_module_from_ir(MemoryBuffer::create_from_memory_range_copy(
            b"define <16 x i16> @vector(<16 x i16> %a) { %result = udiv <16 x i16> %a, splat (i16 251)\n ret <16 x i16> %result }",
            "vector-runtime-regression",
        )).unwrap();
        lower(&context, &module, CodegenTarget::FreeBsdX86_64).unwrap();
        let ir = module.print_to_string().to_string();
        assert!(ir.contains("udiv <16 x i16>"));
        assert!(!ir.contains("__wave.runtime."));
    }
}

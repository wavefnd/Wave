// SPDX-License-Identifier: MPL-2.0
//! Keep the raw str/pointer ABI while checking writes whose backing storage is
//! known only at runtime. All Wave imports are lowered into the same module.
use inkwell::{
    builder::BuilderError,
    context::Context,
    intrinsics::Intrinsic,
    module::{Linkage, Module},
    targets::TargetData,
    values::{BasicValue, FunctionValue, InstructionOpcode, PointerValue},
    AddressSpace, IntPredicate,
};
use std::collections::HashSet;

// This temporary classification never reaches an object file.
const LITERAL_SECTION: &str = "wave.literal.pending";

pub(crate) fn literal<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    bytes: &[u8],
) -> PointerValue<'ctx> {
    let value = context.const_string(bytes, true);
    let global = module.add_global(value.get_type(), None, "$wave.string");
    global.set_initializer(&value);
    global.set_constant(true);
    global.set_linkage(Linkage::Private);
    global.set_section(Some(LITERAL_SECTION));
    global.as_pointer_value()
}

fn known_writable<'ctx>(
    mut address: PointerValue<'ctx>,
    globals: &HashSet<PointerValue<'ctx>>,
) -> bool {
    while let Some(instruction) = address.as_instruction_value() {
        match instruction.get_opcode() {
            InstructionOpcode::Alloca => return true,
            InstructionOpcode::GetElementPtr | InstructionOpcode::BitCast => {
                address = instruction.get_operand(0).unwrap().unwrap_value().into_pointer_value();
            },
            _ => return false,
        }
    }
    globals.contains(&address)
}

pub(super) fn protect<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    target: &TargetData,
) -> Result<Option<FunctionValue<'ctx>>, BuilderError> {
    let literals: Vec<_> = module
        .get_globals()
        .filter(|global| {
            global.get_section().is_some_and(|s| s.to_bytes() == LITERAL_SECTION.as_bytes())
        })
        .collect();
    if literals.is_empty() {
        return Ok(None);
    }
    let writable = module
        .get_globals()
        .filter(|global| !global.is_constant())
        .map(|global| global.as_pointer_value())
        .collect();
    let mut writes = Vec::new();
    for instruction in
        module.get_functions().flat_map(|f| f.get_basic_blocks()).flat_map(|b| b.get_instructions())
    {
        if instruction.get_opcode() == InstructionOpcode::Store {
            let address = instruction.get_operand(1).unwrap().unwrap_value().into_pointer_value();
            let size = target
                .get_store_size(&instruction.get_operand(0).unwrap().unwrap_value().get_type());
            if size != 0 && !known_writable(address, &writable) {
                writes.push((instruction, size));
            }
        }
    }
    if writes.is_empty() {
        for global in literals {
            global.set_section(None);
        }
        return Ok(None);
    }

    // A packed pool preserves distinct addresses, embedded NULs and the final
    // terminator of each literal. One range check covers any interior alias.
    let initializers: Vec<_> = literals.iter().map(|g| g.get_initializer().unwrap()).collect();
    let value = context.const_struct(&initializers, true);
    let pool = module.add_global(value.get_type(), None, "$wave.literal.storage");
    pool.set_initializer(&value);
    pool.set_constant(true);
    pool.set_linkage(Linkage::Private);
    let index_type = context.i32_type();
    for (index, global) in literals.into_iter().enumerate() {
        // SAFETY: each index selects its corresponding field in the packed initializer.
        let field = unsafe {
            pool.as_pointer_value().const_in_bounds_gep(
                value.get_type(),
                &[index_type.const_zero(), index_type.const_int(index as u64, false)],
            )
        };
        global.as_pointer_value().replace_all_uses_with(field);
        // SAFETY: all uses now reference the pool and code generation for user functions is complete.
        unsafe { global.delete() };
    }
    let word = context.ptr_sized_int_type(target, None);
    let pointer = context.ptr_type(AddressSpace::default());
    let signature = context.void_type().fn_type(&[pointer.into(), word.into()], false);
    let guard = module.add_function("$wave.check.literal.write", signature, Some(Linkage::Private));
    let builder = context.create_builder();
    builder.position_at_end(context.append_basic_block(guard, "entry"));
    let base = pool.as_pointer_value().const_to_int(word);
    let dest = builder.build_ptr_to_int(
        guard.get_first_param().unwrap().into_pointer_value(),
        word,
        "dest",
    )?;
    let size = guard.get_nth_param(1).unwrap().into_int_value();
    let length = word.const_int(target.get_store_size(&value.get_type()), false);
    let from_base = builder.build_int_sub(dest, base, "from.base")?;
    let inside = builder.build_int_compare(IntPredicate::ULT, from_base, length, "inside")?;
    let to_base = builder.build_int_sub(base, dest, "to.base")?;
    let crosses = builder.build_int_compare(IntPredicate::ULT, to_base, size, "crosses")?;
    let overlap = builder.build_or(inside, crosses, "overlap")?;
    let fail = context.append_basic_block(guard, "literal.write");
    let ok = context.append_basic_block(guard, "writable");
    builder.build_conditional_branch(overlap, fail, ok)?;
    builder.position_at_end(fail);
    let trap = Intrinsic::find("llvm.trap").unwrap().get_declaration(module, &[]).unwrap();
    builder.build_call(trap, &[], "")?;
    builder.build_unreachable()?;
    builder.position_at_end(ok);
    builder.build_return(None)?;
    for (store, size) in writes {
        // Read the operand after replacing literal globals above.
        let address = store.get_operand(1).unwrap().unwrap_value();
        builder.position_before(&store);
        builder.build_call(guard, &[address.into(), word.const_int(size, false).into()], "")?;
    }
    Ok(Some(guard))
}

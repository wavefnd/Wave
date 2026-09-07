// This file is part of the Wave language project.
// Copyright (c) 2024–2026 Wave Foundation
// Copyright (c) 2024–2026 LunaStev and contributors
//
// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file,
// You can obtain one at https://mozilla.org/MPL/2.0/.
//
// SPDX-License-Identifier: MPL-2.0
// AI TRAINING NOTICE: Prohibited without prior written permission. No use for machine learning or generative AI training, fine-tuning, distillation, embedding, or dataset creation.

//! LLVM lowering for control-flow statements and Wave truthiness.
//!
//! Wave conditions intentionally accept integers, floats, and pointers. Every
//! such value is normalized to an LLVM `i1` here before branches are built.
//! Loop termination analysis distinguishes a break for the current loop from a
//! break nested inside another loop.

use crate::codegen::abi_c::ExternCInfo;
use crate::codegen::VariableInfo;
use crate::expression::rvalue::generate_expression_ir;
use crate::statement::variable::{coerce_basic_value, expression_is_unsigned, CoercionMode};
use inkwell::basic_block::BasicBlock;
use inkwell::module::Module;
use inkwell::targets::TargetData;
use inkwell::types::{BasicType, StructType};
use inkwell::values::{AnyValue, BasicValueEnum, FunctionValue, IntValue, PointerValue};
use inkwell::{FloatPredicate, IntPredicate};
use parser::ast::{
    ASTNode, Expression, Literal, MatchArm, MatchPattern, Mutability, StatementNode, WaveType,
};
use parser::hir::{HirExpressionType, TypedProgram};
use std::collections::{HashMap, HashSet};

fn truthy_to_i1<'ctx>(
    _context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    v: BasicValueEnum<'ctx>,
    name: &str,
) -> inkwell::values::IntValue<'ctx> {
    // Floating-point truthiness uses ordered comparison: NaN does not compare
    // as a non-zero value. Keep this choice explicit when changing semantics.
    match v {
        BasicValueEnum::IntValue(iv) => {
            if iv.get_type().get_bit_width() == 1 {
                iv
            } else {
                let zero = iv.get_type().const_zero();
                builder
                    .build_int_compare(IntPredicate::NE, iv, zero, name)
                    .unwrap()
            }
        }
        BasicValueEnum::FloatValue(fv) => {
            let zero = fv.get_type().const_float(0.0);
            builder
                .build_float_compare(FloatPredicate::ONE, fv, zero, name)
                .unwrap()
        }
        BasicValueEnum::PointerValue(pv) => builder.build_is_not_null(pv, name).unwrap(),
        _ => panic!("Unsupported condition type"),
    }
}

fn expression_is_true(expression: &Expression) -> bool {
    matches!(expression, Expression::Literal(Literal::Bool(true)))
}

fn block_breaks_current_loop(nodes: &[ASTNode]) -> bool {
    nodes.iter().any(node_breaks_current_loop)
}

fn node_breaks_current_loop(node: &ASTNode) -> bool {
    let ASTNode::Statement(statement) = node else {
        return false;
    };

    match statement {
        StatementNode::Break => true,
        StatementNode::If {
            body,
            else_if_blocks,
            else_block,
            ..
        } => {
            block_breaks_current_loop(body)
                || else_if_blocks.as_ref().is_some_and(|blocks| {
                    blocks
                        .iter()
                        .any(|(_, block)| block_breaks_current_loop(block))
                })
                || else_block
                    .as_ref()
                    .is_some_and(|block| block_breaks_current_loop(block))
        }
        StatementNode::Match { arms, .. } => {
            arms.iter().any(|arm| block_breaks_current_loop(&arm.body))
        }
        StatementNode::While { .. } | StatementNode::For { .. } => false,
        _ => false,
    }
}

fn eval_match_case_const<'ctx>(
    discr_ty: inkwell::types::IntType<'ctx>,
    pattern: &MatchPattern,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
) -> inkwell::values::IntValue<'ctx> {
    match pattern {
        MatchPattern::Located { .. } => unreachable!("typed HIR detaches source wrappers"),
        MatchPattern::Int(raw) => {
            let text = raw.as_str();
            let (neg, radix, digits) =
                crate::codegen::number::parse_integer(text).expect("validated match integer");

            let mut iv = discr_ty
                .const_int_from_string(&digits, radix)
                .unwrap_or_else(|| panic!("invalid integer literal in match case: {}", raw));
            if neg {
                iv = iv.const_neg();
            }
            iv
        }
        MatchPattern::Ident(name) => {
            let Some(v) = global_consts.get(name) else {
                panic!(
                    "match case identifier '{}' is not a known integer/enum constant",
                    name
                );
            };

            match *v {
                BasicValueEnum::IntValue(iv) => {
                    if iv.get_type().get_bit_width() != discr_ty.get_bit_width() {
                        panic!(
                            "match case '{}' type width mismatch: case i{}, match i{}",
                            name,
                            iv.get_type().get_bit_width(),
                            discr_ty.get_bit_width()
                        );
                    }
                    iv
                }
                other => panic!(
                    "match case identifier '{}' must resolve to integer/enum constant, got {:?}",
                    name,
                    other.get_type()
                ),
            }
        }
        MatchPattern::Wildcard => {
            panic!("internal error: wildcard cannot be lowered as a switch case constant");
        }
        MatchPattern::Binding(_) | MatchPattern::Variant { .. } => {
            panic!("variant pattern reached LLVM before variant lowering");
        }
    }
}

struct VariantBinding<'ctx> {
    name: String,
    ptr: PointerValue<'ctx>,
    ty: WaveType,
}

fn gen_variant_pattern_test<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    program: &TypedProgram,
    pattern: &MatchPattern,
    value_ptr: PointerValue<'ctx>,
    value_type: &WaveType,
    struct_types: &HashMap<String, StructType<'ctx>>,
) -> (IntValue<'ctx>, Vec<VariantBinding<'ctx>>) {
    match pattern {
        MatchPattern::Located { .. } => unreachable!("typed HIR detaches source wrappers"),
        MatchPattern::Wildcard => (context.bool_type().const_int(1, false), Vec::new()),
        MatchPattern::Binding(name) => (
            context.bool_type().const_int(1, false),
            vec![VariantBinding {
                name: name.clone(),
                ptr: value_ptr,
                ty: value_type.clone(),
            }],
        ),
        MatchPattern::Variant { payloads, .. } => {
            let metadata = program.variant_pattern_of(pattern).unwrap_or_else(|| {
                panic!("variant pattern reached LLVM without typed HIR metadata")
            });
            let WaveType::Variant(name) = &metadata.variant_type else {
                panic!("variant pattern metadata has a non-variant type");
            };
            let variant_ty = *struct_types
                .get(name)
                .unwrap_or_else(|| panic!("variant type '{}' not found", name));
            let tag_ptr = builder
                .build_struct_gep(variant_ty, value_ptr, 0, "variant.match.tag.ptr")
                .unwrap();
            let tag = builder
                .build_load(
                    context.i32_type().as_basic_type_enum(),
                    tag_ptr,
                    "variant.match.tag",
                )
                .unwrap()
                .into_int_value();
            let expected_tag = context
                .i32_type()
                .const_int(metadata.discriminant as u64, false);
            let mut condition = builder
                .build_int_compare(IntPredicate::EQ, tag, expected_tag, "variant.match.case")
                .unwrap();
            let case_index = metadata.discriminant + 1;
            let payload_ty = variant_ty
                .get_field_type_at_index(case_index)
                .unwrap_or_else(|| {
                    panic!(
                        "variant '{}' has no payload slot for case '{}'",
                        name, metadata.case_name
                    )
                })
                .into_struct_type();
            let payload_ptr = builder
                .build_struct_gep(
                    variant_ty,
                    value_ptr,
                    case_index,
                    "variant.match.payload.ptr",
                )
                .unwrap();
            let mut bindings = Vec::new();
            for (index, (payload_pattern, payload_wave_type)) in
                payloads.iter().zip(&metadata.payload_types).enumerate()
            {
                let field_ptr = builder
                    .build_struct_gep(
                        payload_ty,
                        payload_ptr,
                        index as u32,
                        "variant.match.field.ptr",
                    )
                    .unwrap();
                let (nested_condition, mut nested_bindings) = gen_variant_pattern_test(
                    context,
                    builder,
                    program,
                    payload_pattern,
                    field_ptr,
                    payload_wave_type,
                    struct_types,
                );
                condition = builder
                    .build_and(condition, nested_condition, "variant.match.and")
                    .unwrap();
                bindings.append(&mut nested_bindings);
            }
            (condition, bindings)
        }
        MatchPattern::Int(_) | MatchPattern::Ident(_) => {
            panic!("integer pattern reached variant LLVM lowering")
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn gen_variant_match_ir<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    module: &'ctx Module<'ctx>,
    string_counter: &mut usize,
    value: &Expression,
    value_type: &WaveType,
    arms: &[MatchArm],
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    loop_exit_stack: &mut Vec<BasicBlock<'ctx>>,
    loop_continue_stack: &mut Vec<BasicBlock<'ctx>>,
    current_function: FunctionValue<'ctx>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
    struct_field_types: &HashMap<String, HashMap<String, WaveType>>,
    target_data: &'ctx TargetData,
    extern_c_info: &HashMap<String, ExternCInfo<'ctx>>,
    program: &TypedProgram,
) {
    let WaveType::Variant(name) = value_type else {
        panic!("variant match lowering received a non-variant value type");
    };
    let variant_ty = *struct_types
        .get(name)
        .unwrap_or_else(|| panic!("variant type '{}' not found", name));
    let value = generate_expression_ir(
        program,
        context,
        builder,
        value,
        variables,
        module,
        Some(variant_ty.as_basic_type_enum()),
        global_consts,
        struct_types,
        struct_field_indices,
        target_data,
        extern_c_info,
    );
    let value_ptr = builder
        .build_alloca(variant_ty, "variant.match.value")
        .unwrap();
    builder.build_store(value_ptr, value).unwrap();

    let current_fn = builder.get_insert_block().unwrap().get_parent().unwrap();
    let merge_block = context.append_basic_block(current_fn, "variant.match.end");
    let fail_block = context.append_basic_block(current_fn, "variant.match.unreachable");
    let test_blocks = (0..arms.len())
        .map(|index| context.append_basic_block(current_fn, &format!("variant.match.test.{index}")))
        .collect::<Vec<_>>();
    if let Some(first) = test_blocks.first() {
        builder.build_unconditional_branch(*first).unwrap();
    } else {
        builder.build_unconditional_branch(fail_block).unwrap();
    }

    let outer_variables = variables.clone();
    let mut all_arms_terminate = true;
    for (index, arm) in arms.iter().enumerate() {
        builder.position_at_end(test_blocks[index]);
        let (condition, bindings) = gen_variant_pattern_test(
            context,
            builder,
            program,
            &arm.pattern,
            value_ptr,
            value_type,
            struct_types,
        );
        let body_block =
            context.append_basic_block(current_fn, &format!("variant.match.arm.{index}"));
        let next_block = test_blocks.get(index + 1).copied().unwrap_or(fail_block);
        builder
            .build_conditional_branch(condition, body_block, next_block)
            .unwrap();

        builder.position_at_end(body_block);
        *variables = outer_variables.clone();
        for binding in bindings {
            variables.insert(
                binding.name,
                VariableInfo {
                    ptr: binding.ptr,
                    mutability: Mutability::Var,
                    ty: binding.ty,
                },
            );
        }
        for statement in &arm.body {
            super::generate_statement_ir(
                context,
                builder,
                module,
                string_counter,
                statement,
                variables,
                loop_exit_stack,
                loop_continue_stack,
                current_function,
                global_consts,
                struct_types,
                struct_field_indices,
                struct_field_types,
                target_data,
                extern_c_info,
                program,
            );
        }
        if builder
            .get_insert_block()
            .is_some_and(|block| block.get_terminator().is_none())
        {
            all_arms_terminate = false;
            builder.build_unconditional_branch(merge_block).unwrap();
        }
    }
    *variables = outer_variables;
    builder.position_at_end(fail_block);
    builder.build_unreachable().unwrap();
    builder.position_at_end(merge_block);
    if all_arms_terminate {
        builder.build_unreachable().unwrap();
    }
}

pub(super) fn gen_if_ir<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    module: &'ctx Module<'ctx>,
    string_counter: &mut usize,
    condition: &Expression,
    body: &[ASTNode],
    else_if_blocks: &Option<Box<Vec<(Expression, Vec<ASTNode>)>>>,
    else_block: &Option<Box<Vec<ASTNode>>>,
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    loop_exit_stack: &mut Vec<BasicBlock<'ctx>>,
    loop_continue_stack: &mut Vec<BasicBlock<'ctx>>,
    current_function: FunctionValue<'ctx>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
    struct_field_types: &HashMap<String, HashMap<String, WaveType>>,
    target_data: &'ctx TargetData,
    extern_c_info: &HashMap<String, ExternCInfo<'ctx>>,
    program: &TypedProgram,
) {
    let current_fn = builder.get_insert_block().unwrap().get_parent().unwrap();

    let cond_any = generate_expression_ir(
        program,
        context,
        builder,
        condition,
        variables,
        module,
        None,
        global_consts,
        struct_types,
        struct_field_indices,
        target_data,
        extern_c_info,
    );

    let cond_i1 = truthy_to_i1(context, builder, cond_any, "if_cond");

    let then_block = context.append_basic_block(current_fn, "then");
    let else_block_bb = context.append_basic_block(current_fn, "else");
    let merge_block = context.append_basic_block(current_fn, "merge");
    let mut merge_reachable = false;

    builder
        .build_conditional_branch(cond_i1, then_block, else_block_bb)
        .unwrap();

    builder.position_at_end(then_block);
    for stmt in body {
        super::generate_statement_ir(
            context,
            builder,
            module,
            string_counter,
            stmt,
            variables,
            loop_exit_stack,
            loop_continue_stack,
            current_function,
            global_consts,
            struct_types,
            struct_field_indices,
            struct_field_types,
            target_data,
            extern_c_info,
            program,
        );
    }

    let then_end = builder.get_insert_block().unwrap();
    if then_end.get_terminator().is_none() {
        builder.build_unconditional_branch(merge_block).unwrap();
        merge_reachable = true;
    }

    builder.position_at_end(else_block_bb);
    let mut current_check_bb = else_block_bb;

    if let Some(else_ifs) = else_if_blocks {
        for (else_if_cond, else_if_body) in else_ifs.iter() {
            builder.position_at_end(current_check_bb);

            let c_any = generate_expression_ir(
                program,
                context,
                builder,
                else_if_cond,
                variables,
                module,
                None,
                global_consts,
                struct_types,
                struct_field_indices,
                target_data,
                extern_c_info,
            );
            let c_i1 = truthy_to_i1(context, builder, c_any, "elif_cond");

            let then_bb = context.append_basic_block(current_fn, "else_if_then");
            let next_check_bb = context.append_basic_block(current_fn, "next_else_if");

            builder
                .build_conditional_branch(c_i1, then_bb, next_check_bb)
                .unwrap();

            builder.position_at_end(then_bb);
            for stmt in else_if_body {
                super::generate_statement_ir(
                    context,
                    builder,
                    module,
                    string_counter,
                    stmt,
                    variables,
                    loop_exit_stack,
                    loop_continue_stack,
                    current_function,
                    global_consts,
                    struct_types,
                    struct_field_indices,
                    struct_field_types,
                    target_data,
                    extern_c_info,
                    program,
                );
            }

            let end_bb = builder.get_insert_block().unwrap();
            if end_bb.get_terminator().is_none() {
                builder.build_unconditional_branch(merge_block).unwrap();
                merge_reachable = true;
            }

            current_check_bb = next_check_bb;
        }

        builder.position_at_end(current_check_bb);

        if let Some(else_body) = else_block {
            for stmt in else_body.iter() {
                super::generate_statement_ir(
                    context,
                    builder,
                    module,
                    string_counter,
                    stmt,
                    variables,
                    loop_exit_stack,
                    loop_continue_stack,
                    current_function,
                    global_consts,
                    struct_types,
                    struct_field_indices,
                    struct_field_types,
                    target_data,
                    extern_c_info,
                    program,
                );
            }
        }

        let else_end = builder.get_insert_block().unwrap();
        if else_end.get_terminator().is_none() {
            builder.build_unconditional_branch(merge_block).unwrap();
            merge_reachable = true;
        }

        builder.position_at_end(merge_block);
        if !merge_reachable {
            builder.build_unreachable().unwrap();
        }
        return;
    }

    builder.position_at_end(current_check_bb);

    if let Some(else_body) = else_block.as_deref() {
        for stmt in else_body.iter() {
            super::generate_statement_ir(
                context,
                builder,
                module,
                string_counter,
                stmt,
                variables,
                loop_exit_stack,
                loop_continue_stack,
                current_function,
                global_consts,
                struct_types,
                struct_field_indices,
                struct_field_types,
                target_data,
                extern_c_info,
                program,
            );
        }
    }

    let else_end = builder.get_insert_block().unwrap();
    if else_end.get_terminator().is_none() {
        builder.build_unconditional_branch(merge_block).unwrap();
        merge_reachable = true;
    }

    builder.position_at_end(merge_block);
    if !merge_reachable {
        builder.build_unreachable().unwrap();
    }
}

pub(super) fn gen_while_ir<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    module: &'ctx Module<'ctx>,
    string_counter: &mut usize,
    condition: &Expression,
    body: &[ASTNode],
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    loop_exit_stack: &mut Vec<BasicBlock<'ctx>>,
    loop_continue_stack: &mut Vec<BasicBlock<'ctx>>,
    current_function: FunctionValue<'ctx>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
    struct_field_types: &HashMap<String, HashMap<String, WaveType>>,
    target_data: &'ctx TargetData,
    extern_c_info: &HashMap<String, ExternCInfo<'ctx>>,
    program: &TypedProgram,
) {
    let current_fn = builder.get_insert_block().unwrap().get_parent().unwrap();

    let cond_block = context.append_basic_block(current_fn, "while.cond");
    let body_block = context.append_basic_block(current_fn, "while.body");
    let merge_block = context.append_basic_block(current_fn, "while.end");

    loop_exit_stack.push(merge_block);
    loop_continue_stack.push(cond_block);

    builder.build_unconditional_branch(cond_block).unwrap();
    builder.position_at_end(cond_block);

    let cond_val = generate_expression_ir(
        program,
        context,
        builder,
        condition,
        variables,
        module,
        None,
        global_consts,
        struct_types,
        struct_field_indices,
        target_data,
        extern_c_info,
    );

    let cond_bool = truthy_to_i1(context, builder, cond_val, "while_cond");
    builder
        .build_conditional_branch(cond_bool, body_block, merge_block)
        .unwrap();

    builder.position_at_end(body_block);
    for stmt in body {
        super::generate_statement_ir(
            context,
            builder,
            module,
            string_counter,
            stmt,
            variables,
            loop_exit_stack,
            loop_continue_stack,
            current_function,
            global_consts,
            struct_types,
            struct_field_indices,
            struct_field_types,
            target_data,
            extern_c_info,
            program,
        );
    }

    let end_bb = builder.get_insert_block().unwrap();
    if end_bb.get_terminator().is_none() {
        builder.build_unconditional_branch(cond_block).unwrap();
    }

    loop_exit_stack.pop();
    loop_continue_stack.pop();

    builder.position_at_end(merge_block);
    if expression_is_true(condition) && !block_breaks_current_loop(body) {
        builder.build_unreachable().unwrap();
    }
}

pub(super) fn gen_match_ir<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    module: &'ctx Module<'ctx>,
    string_counter: &mut usize,
    value: &Expression,
    arms: &[MatchArm],
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    loop_exit_stack: &mut Vec<BasicBlock<'ctx>>,
    loop_continue_stack: &mut Vec<BasicBlock<'ctx>>,
    current_function: FunctionValue<'ctx>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
    struct_field_types: &HashMap<String, HashMap<String, WaveType>>,
    target_data: &'ctx TargetData,
    extern_c_info: &HashMap<String, ExternCInfo<'ctx>>,
    program: &TypedProgram,
) {
    if let Some(HirExpressionType::Resolved(value_type @ WaveType::Variant(_))) =
        program.type_of(value)
    {
        gen_variant_match_ir(
            context,
            builder,
            module,
            string_counter,
            value,
            value_type,
            arms,
            variables,
            loop_exit_stack,
            loop_continue_stack,
            current_function,
            global_consts,
            struct_types,
            struct_field_indices,
            struct_field_types,
            target_data,
            extern_c_info,
            program,
        );
        return;
    }

    let current_fn = builder.get_insert_block().unwrap().get_parent().unwrap();

    let discr_any = generate_expression_ir(
        program,
        context,
        builder,
        value,
        variables,
        module,
        None,
        global_consts,
        struct_types,
        struct_field_indices,
        target_data,
        extern_c_info,
    );

    let discr = match discr_any {
        BasicValueEnum::IntValue(iv) => iv,
        other => panic!(
            "match value must be integer/enum type, got {:?}",
            other.get_type()
        ),
    };
    let discr_ty = discr.get_type();

    let merge_block = context.append_basic_block(current_fn, "match.end");

    let mut default_arm: Option<&MatchArm> = None;
    let mut case_entries: Vec<(inkwell::values::IntValue<'ctx>, BasicBlock<'ctx>, &MatchArm)> =
        Vec::new();
    let mut seen_case_values: HashSet<String> = HashSet::new();

    for (idx, arm) in arms.iter().enumerate() {
        match &arm.pattern {
            MatchPattern::Located { .. } => unreachable!("typed HIR detaches source wrappers"),
            MatchPattern::Wildcard => {
                if default_arm.is_some() {
                    panic!("duplicate wildcard match arm (`_`)");
                }
                default_arm = Some(arm);
            }
            pat @ (MatchPattern::Int(_) | MatchPattern::Ident(_)) => {
                let case_value = eval_match_case_const(discr_ty, pat, global_consts);
                let case_key = case_value.print_to_string().to_string();
                if !seen_case_values.insert(case_key.clone()) {
                    panic!("duplicate match case value: {}", case_key);
                }

                let case_block =
                    context.append_basic_block(current_fn, &format!("match.case.{}", idx));
                case_entries.push((case_value, case_block, arm));
            }
            MatchPattern::Binding(_) | MatchPattern::Variant { .. } => {
                panic!("variant pattern reached LLVM before variant lowering");
            }
        }
    }

    let default_block = if default_arm.is_some() {
        context.append_basic_block(current_fn, "match.default")
    } else {
        merge_block
    };

    let switch_cases: Vec<(inkwell::values::IntValue<'ctx>, BasicBlock<'ctx>)> =
        case_entries.iter().map(|(v, bb, _)| (*v, *bb)).collect();

    builder
        .build_switch(discr, default_block, &switch_cases)
        .unwrap();

    for (_, case_block, arm) in case_entries {
        builder.position_at_end(case_block);
        for stmt in &arm.body {
            super::generate_statement_ir(
                context,
                builder,
                module,
                string_counter,
                stmt,
                variables,
                loop_exit_stack,
                loop_continue_stack,
                current_function,
                global_consts,
                struct_types,
                struct_field_indices,
                struct_field_types,
                target_data,
                extern_c_info,
                program,
            );
        }

        let end_bb = builder.get_insert_block().unwrap();
        if end_bb.get_terminator().is_none() {
            builder.build_unconditional_branch(merge_block).unwrap();
        }
    }

    if let Some(default_arm) = default_arm {
        builder.position_at_end(default_block);
        for stmt in &default_arm.body {
            super::generate_statement_ir(
                context,
                builder,
                module,
                string_counter,
                stmt,
                variables,
                loop_exit_stack,
                loop_continue_stack,
                current_function,
                global_consts,
                struct_types,
                struct_field_indices,
                struct_field_types,
                target_data,
                extern_c_info,
                program,
            );
        }

        let end_bb = builder.get_insert_block().unwrap();
        if end_bb.get_terminator().is_none() {
            builder.build_unconditional_branch(merge_block).unwrap();
        }
    }

    builder.position_at_end(merge_block);
}

pub(super) fn gen_for_ir<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    module: &'ctx Module<'ctx>,
    string_counter: &mut usize,
    initialization: &ASTNode,
    condition: &Expression,
    increment: &Expression,
    body: &[ASTNode],
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    loop_exit_stack: &mut Vec<BasicBlock<'ctx>>,
    loop_continue_stack: &mut Vec<BasicBlock<'ctx>>,
    current_function: FunctionValue<'ctx>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
    struct_field_types: &HashMap<String, HashMap<String, WaveType>>,
    target_data: &'ctx TargetData,
    extern_c_info: &HashMap<String, ExternCInfo<'ctx>>,
    program: &TypedProgram,
) {
    let current_fn = builder.get_insert_block().unwrap().get_parent().unwrap();
    let outer_scope_variables = variables.clone();

    super::generate_statement_ir(
        context,
        builder,
        module,
        string_counter,
        initialization,
        variables,
        loop_exit_stack,
        loop_continue_stack,
        current_function,
        global_consts,
        struct_types,
        struct_field_indices,
        struct_field_types,
        target_data,
        extern_c_info,
        program,
    );

    let cond_block = context.append_basic_block(current_fn, "for.cond");
    let body_block = context.append_basic_block(current_fn, "for.body");
    let inc_block = context.append_basic_block(current_fn, "for.inc");
    let merge_block = context.append_basic_block(current_fn, "for.end");

    loop_exit_stack.push(merge_block);
    loop_continue_stack.push(inc_block);

    builder.build_unconditional_branch(cond_block).unwrap();
    builder.position_at_end(cond_block);

    let cond_val = generate_expression_ir(
        program,
        context,
        builder,
        condition,
        variables,
        module,
        None,
        global_consts,
        struct_types,
        struct_field_indices,
        target_data,
        extern_c_info,
    );

    let cond_bool = truthy_to_i1(context, builder, cond_val, "for_cond");
    builder
        .build_conditional_branch(cond_bool, body_block, merge_block)
        .unwrap();

    builder.position_at_end(body_block);
    for stmt in body {
        super::generate_statement_ir(
            context,
            builder,
            module,
            string_counter,
            stmt,
            variables,
            loop_exit_stack,
            loop_continue_stack,
            current_function,
            global_consts,
            struct_types,
            struct_field_indices,
            struct_field_types,
            target_data,
            extern_c_info,
            program,
        );
    }

    let body_end = builder.get_insert_block().unwrap();
    if body_end.get_terminator().is_none() {
        builder.build_unconditional_branch(inc_block).unwrap();
    }

    builder.position_at_end(inc_block);
    let _ = generate_expression_ir(
        program,
        context,
        builder,
        increment,
        variables,
        module,
        None,
        global_consts,
        struct_types,
        struct_field_indices,
        target_data,
        extern_c_info,
    );

    let inc_end = builder.get_insert_block().unwrap();
    if inc_end.get_terminator().is_none() {
        builder.build_unconditional_branch(cond_block).unwrap();
    }

    loop_exit_stack.pop();
    loop_continue_stack.pop();

    builder.position_at_end(merge_block);
    if expression_is_true(condition) && !block_breaks_current_loop(body) {
        builder.build_unreachable().unwrap();
    }
    *variables = outer_scope_variables;
}

pub(super) fn gen_break_ir<'ctx>(
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    loop_exit_stack: &mut Vec<BasicBlock<'ctx>>,
) {
    if let Some(target_block) = loop_exit_stack.last() {
        builder.build_unconditional_branch(*target_block).unwrap();
    } else {
        panic!("break used outside of loop!");
    }
}

pub(super) fn gen_continue_ir<'ctx>(
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    loop_continue_stack: &mut Vec<BasicBlock<'ctx>>,
) {
    if let Some(target_block) = loop_continue_stack.last() {
        builder.build_unconditional_branch(*target_block).unwrap();
    } else {
        panic!("continue used outside of loop!");
    }
}

pub(super) fn gen_return_ir<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    module: &'ctx Module<'ctx>,
    expr_opt: Option<&Expression>,
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    current_function: FunctionValue<'ctx>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
    target_data: &'ctx TargetData,
    extern_c_info: &HashMap<String, ExternCInfo<'ctx>>,
    program: &TypedProgram,
) {
    let expected_ret = current_function.get_type().get_return_type(); // Option<BasicTypeEnum>

    match (expected_ret, expr_opt) {
        (None, None) => {
            builder.build_return(None).unwrap();
        }

        (None, Some(_)) => {
            panic!("Void function cannot return a value");
        }

        (Some(ret_ty), None) => {
            let is_i32_main = current_function.get_name().to_str().ok() == Some("main")
                && matches!(ret_ty, inkwell::types::BasicTypeEnum::IntType(it) if it.get_bit_width() == 32);

            if is_i32_main {
                let zero = context.i32_type().const_zero();
                builder.build_return(Some(&zero)).unwrap();
            } else {
                panic!("Non-void function must return a value");
            }
        }

        (Some(ret_ty), Some(expr)) => {
            let mut v = generate_expression_ir(
                program,
                context,
                builder,
                expr,
                variables,
                module,
                Some(ret_ty),
                global_consts,
                struct_types,
                struct_field_indices,
                target_data,
                extern_c_info,
            );

            if v.get_type() != ret_ty {
                v = coerce_basic_value(
                    context,
                    builder,
                    v,
                    ret_ty,
                    "ret_cast",
                    CoercionMode::Implicit,
                    expression_is_unsigned(program, expr),
                );
            }

            builder.build_return(Some(&v)).unwrap();
        }
    }
}

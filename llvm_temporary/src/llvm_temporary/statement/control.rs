use crate::llvm_temporary::llvm_codegen::VariableInfo;
use crate::llvm_temporary::expression::rvalue::generate_expression_ir;
use inkwell::basic_block::BasicBlock;
use inkwell::module::Module;
use inkwell::types::{AnyTypeEnum, BasicTypeEnum, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue};
use inkwell::{FloatPredicate, IntPredicate};
use parser::ast::{ASTNode, Expression};
use std::collections::HashMap;
use crate::llvm_temporary::statement::variable::{coerce_basic_value, CoercionMode};

fn truthy_to_i1<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    v: BasicValueEnum<'ctx>,
    name: &str,
) -> inkwell::values::IntValue<'ctx> {
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
) {
    let current_fn = builder.get_insert_block().unwrap().get_parent().unwrap();

    let cond_any = generate_expression_ir(
        context,
        builder,
        condition,
        variables,
        module,
        None,
        global_consts,
        struct_types,
        struct_field_indices,
    );

    let cond_i1 = truthy_to_i1(context, builder, cond_any, "if_cond");

    let then_block = context.append_basic_block(current_fn, "then");
    let else_block_bb = context.append_basic_block(current_fn, "else");
    let merge_block = context.append_basic_block(current_fn, "merge");

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
        );
    }

    let then_end = builder.get_insert_block().unwrap();
    if then_end.get_terminator().is_none() {
        builder.build_unconditional_branch(merge_block).unwrap();
    }

    builder.position_at_end(else_block_bb);
    let mut current_check_bb = else_block_bb;

    if let Some(else_ifs) = else_if_blocks {
        for (else_if_cond, else_if_body) in else_ifs.iter() {
            builder.position_at_end(current_check_bb);

            let c_any = generate_expression_ir(
                context,
                builder,
                else_if_cond,
                variables,
                module,
                None,
                global_consts,
                struct_types,
                struct_field_indices,
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
                );
            }

            let end_bb = builder.get_insert_block().unwrap();
            if end_bb.get_terminator().is_none() {
                builder.build_unconditional_branch(merge_block).unwrap();
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
                );
            }
        }

        let else_end = builder.get_insert_block().unwrap();
        if else_end.get_terminator().is_none() {
            builder.build_unconditional_branch(merge_block).unwrap();
        }

        builder.position_at_end(merge_block);
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
            );
        }
    }

    let else_end = builder.get_insert_block().unwrap();
    if else_end.get_terminator().is_none() {
        builder.build_unconditional_branch(merge_block).unwrap();
    }

    builder.position_at_end(merge_block);
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
        context,
        builder,
        condition,
        variables,
        module,
        None,
        global_consts,
        struct_types,
        struct_field_indices,
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
        );
    }

   let end_bb = builder.get_insert_block().unwrap();
    if end_bb.get_terminator().is_none() {
        builder.build_unconditional_branch(cond_block).unwrap();
    }

    loop_exit_stack.pop();
    loop_continue_stack.pop();

    builder.position_at_end(merge_block);
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
) {
    let expected_ret = current_function.get_type().get_return_type(); // Option<BasicTypeEnum>

    match (expected_ret, expr_opt) {
        (None, None) => {
            builder.build_return(None).unwrap();
        }

        (None, Some(_)) => {
            panic!("Void function cannot return a value");
        }

        (Some(_), None) => {
            panic!("Non-void function must return a value");
        }

        (Some(ret_ty), Some(expr)) => {
            let mut v = generate_expression_ir(
                context,
                builder,
                expr,
                variables,
                module,
                Some(ret_ty),
                global_consts,
                struct_types,
                struct_field_indices,
            );

            if v.get_type() != ret_ty {
                v = coerce_basic_value(
                    context,
                    builder,
                    v,
                    ret_ty,
                    "ret_cast",
                    CoercionMode::Implicit,
                );
            }

            builder.build_return(Some(&v)).unwrap();
        }
    }
}
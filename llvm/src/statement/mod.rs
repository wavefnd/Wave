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

//! Statement dispatcher for LLVM lowering.
//!
//! Specialized modules own each statement family; this module carries shared
//! function state and prevents emission after the current basic block has been
//! terminated. Adding a statement kind requires updating this dispatch as well
//! as semantic validation.

pub mod asm;
pub mod assign;
pub mod control;
pub mod expr_stmt;
pub mod io;
pub mod variable;

use crate::codegen::abi_c::ExternCInfo;
use crate::codegen::VariableInfo;
use inkwell::basic_block::BasicBlock;
use inkwell::context::Context;
use inkwell::targets::TargetData;
use inkwell::types::StructType;
use inkwell::values::{BasicValueEnum, FunctionValue};
use parser::ast::WaveType;
use parser::ast::{ASTNode, StatementNode};
use parser::hir::TypedProgram;
use std::collections::HashMap;

pub fn generate_statement_ir<'ctx>(
    context: &'ctx Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    module: &'ctx inkwell::module::Module<'ctx>,
    string_counter: &mut usize,
    stmt: &ASTNode,
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
    // A return, break, continue, or unconditional branch may have terminated the
    // block while walking a source-level list. LLVM rejects a second terminator.
    if builder
        .get_insert_block()
        .is_some_and(|block| block.get_terminator().is_some())
    {
        return;
    }

    match stmt {
        ASTNode::Variable(var_node) => {
            variable::gen_variable_ir(
                context,
                builder,
                module,
                var_node,
                variables,
                global_consts,
                struct_types,
                struct_field_indices,
                target_data,
                extern_c_info,
                program,
            );
        }

        ASTNode::Statement(StatementNode::Println(message))
        | ASTNode::Statement(StatementNode::Print(message)) => {
            io::gen_print_literal_ir(context, builder, module, string_counter, message);
        }

        ASTNode::Statement(StatementNode::PrintlnFormat { format, args })
        | ASTNode::Statement(StatementNode::PrintFormat { format, args }) => {
            io::gen_print_format_ir(
                context,
                builder,
                module,
                string_counter,
                format,
                args,
                variables,
                global_consts,
                struct_types,
                struct_field_indices,
                struct_field_types,
                target_data,
                extern_c_info,
                program,
            );
        }

        ASTNode::Statement(StatementNode::Input { format, args }) => {
            io::gen_input_ir(
                context,
                builder,
                module,
                string_counter,
                format,
                args,
                variables,
                global_consts,
                struct_types,
                struct_field_indices,
                struct_field_types,
                target_data,
                extern_c_info,
                program,
            );
        }

        ASTNode::Statement(StatementNode::If {
            condition,
            body,
            else_if_blocks,
            else_block,
        }) => {
            control::gen_if_ir(
                context,
                builder,
                module,
                string_counter,
                condition,
                body,
                else_if_blocks,
                else_block,
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

        ASTNode::Statement(StatementNode::While { condition, body }) => {
            control::gen_while_ir(
                context,
                builder,
                module,
                string_counter,
                condition,
                body,
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

        ASTNode::Statement(StatementNode::Match { value, arms }) => {
            control::gen_match_ir(
                context,
                builder,
                module,
                string_counter,
                value,
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
        }

        ASTNode::Statement(StatementNode::For {
            initialization,
            condition,
            increment,
            body,
        }) => {
            control::gen_for_ir(
                context,
                builder,
                module,
                string_counter,
                initialization,
                condition,
                increment,
                body,
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

        ASTNode::Statement(StatementNode::AsmBlock {
            instructions,
            inputs,
            outputs,
            clobbers,
        }) => {
            asm::gen_asm_stmt_ir(
                context,
                builder,
                module,
                instructions,
                inputs,
                outputs,
                clobbers,
                variables,
                global_consts,
                struct_types,
            );
        }

        ASTNode::Statement(StatementNode::Expression(expr)) => {
            expr_stmt::gen_expr_stmt_ir(
                context,
                builder,
                module,
                expr,
                variables,
                global_consts,
                struct_types,
                struct_field_indices,
                target_data,
                extern_c_info,
                program,
            );
            if matches!(
                program.type_of(expr),
                Some(parser::hir::HirExpressionType::Resolved(
                    parser::ast::WaveType::Never
                ))
            ) && builder
                .get_insert_block()
                .is_some_and(|block| block.get_terminator().is_none())
            {
                builder.build_unreachable().unwrap();
            }
        }

        ASTNode::Statement(StatementNode::Assign { variable, value }) => {
            assign::gen_assign_ir(
                context,
                builder,
                module,
                variable,
                value,
                variables,
                global_consts,
                struct_types,
                struct_field_indices,
                target_data,
                extern_c_info,
                program,
            );
        }

        ASTNode::Statement(StatementNode::Break) => {
            control::gen_break_ir(builder, loop_exit_stack);
        }

        ASTNode::Statement(StatementNode::Continue) => {
            control::gen_continue_ir(builder, loop_continue_stack);
        }

        ASTNode::Statement(StatementNode::Return(expr_opt)) => {
            control::gen_return_ir(
                context,
                builder,
                module,
                expr_opt.as_ref(),
                variables,
                current_function,
                global_consts,
                struct_types,
                struct_field_indices,
                target_data,
                extern_c_info,
                program,
            );
        }

        _ => {}
    }
}

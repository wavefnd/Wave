mod assign;
mod asm;
mod control;
mod expr_stmt;
mod io;
mod variable;

use crate::llvm_temporary::llvm_codegen::VariableInfo;
use inkwell::basic_block::BasicBlock;
use inkwell::context::Context;
use inkwell::types::StructType;
use inkwell::values::{BasicValueEnum, FunctionValue};
use parser::ast::{ASTNode, StatementNode};
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
) {
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
            );
        }

        ASTNode::Statement(StatementNode::AsmBlock {
                               instructions,
                               inputs,
                               outputs,
                           }) => {
            asm::gen_asm_stmt_ir(
                context,
                builder,
                module,
                instructions,
                inputs,
                outputs,
                variables,
                global_consts,
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
            );
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
            );
        }

        _ => {}
    }
}

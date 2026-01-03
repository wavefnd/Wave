use crate::llvm_temporary::expression::rvalue::generate_expression_ir;
use crate::llvm_temporary::llvm_codegen::VariableInfo;
use inkwell::module::Module;
use inkwell::types::StructType;
use inkwell::values::BasicValueEnum;
use parser::ast::Expression;
use std::collections::HashMap;

pub(super) fn gen_expr_stmt_ir<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    module: &'ctx Module<'ctx>,
    expr: &Expression,
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
) {
    let _ = generate_expression_ir(
        context,
        builder,
        expr,
        variables,
        module,
        None,
        global_consts,
        struct_types,
        struct_field_indices,
    );
}

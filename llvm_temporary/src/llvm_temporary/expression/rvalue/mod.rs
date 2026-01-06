use crate::llvm_temporary::llvm_codegen::VariableInfo;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicTypeEnum, StructType};
use inkwell::values::{BasicValueEnum};
use parser::ast::Expression;
use std::collections::HashMap;

pub mod dispatch;
pub mod utils;

pub mod literals;
pub mod variables;
pub mod pointers;
pub mod calls;
pub mod assign;
pub mod binary;
pub mod index;
pub mod asm;
pub mod structs;
pub mod unary;
pub mod incdec;

pub struct ProtoInfo<'ctx> {
    pub vtable_ty: StructType<'ctx>,
    pub fat_ty: StructType<'ctx>,
    pub methods: Vec<String>,
}

pub(crate) struct ExprGenEnv<'ctx, 'a> {
    pub context: &'ctx Context,
    pub builder: &'ctx Builder<'ctx>,
    pub variables: &'a mut HashMap<String, VariableInfo<'ctx>>,
    pub module: &'ctx Module<'ctx>,
    pub global_consts: &'a HashMap<String, BasicValueEnum<'ctx>>,
    pub struct_types: &'a HashMap<String, StructType<'ctx>>,
    pub struct_field_indices: &'a HashMap<String, HashMap<String, u32>>,
}

impl<'ctx, 'a> ExprGenEnv<'ctx, 'a> {
    #[inline]
    pub fn gen(
        &mut self,
        expr: &Expression,
        expected_type: Option<BasicTypeEnum<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        dispatch::gen_expr(self, expr, expected_type)
    }
}

pub fn generate_expression_ir<'ctx>(
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
    expr: &Expression,
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    module: &'ctx Module<'ctx>,
    expected_type: Option<BasicTypeEnum<'ctx>>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
) -> BasicValueEnum<'ctx> {
    let mut env = ExprGenEnv {
        context,
        builder,
        variables,
        module,
        global_consts,
        struct_types,
        struct_field_indices,
    };

    env.gen(expr, expected_type)
}

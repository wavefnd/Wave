use super::*;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum;
use parser::ast::Expression;

pub(crate) fn gen_expr<'ctx, 'a>(
    env: &mut ExprGenEnv<'ctx, 'a>,
    expr: &Expression,
    expected_type: Option<BasicTypeEnum<'ctx>>,
) -> BasicValueEnum<'ctx> {
    match expr {
        Expression::Literal(lit) => literals::gen(env, lit, expected_type),
        Expression::Variable(name) => variables::gen(env, name),

        Expression::Deref(inner) => pointers::gen_deref(env, inner),
        Expression::AddressOf(inner) => pointers::gen_addressof(env, inner, expected_type),

        Expression::MethodCall { object, name, args } => calls::gen_method_call(env, object, name, args),
        Expression::FunctionCall { name, args } => calls::gen_function_call(env, name, args, expected_type),

        Expression::AssignOperation { target, operator, value } => {
            assign::gen_assign_operation(env, target, operator, value)
        }
        Expression::Assignment { target, value } => assign::gen_assignment(env, target, value),

        Expression::BinaryExpression { left, operator, right } => {
            binary::gen(env, left, operator, right, expected_type)
        }

        Expression::IndexAccess { target, index } => index::gen(env, target, index),

        Expression::AsmBlock { instructions, inputs, outputs } => {
            asm::gen(env, instructions, inputs, outputs)
        }

        Expression::StructLiteral { name, fields } => structs::gen_struct_literal(env, name, fields),
        Expression::FieldAccess { object, field } => structs::gen_field_access(env, object, field),

        Expression::Unary { operator, expr } => unary::gen(env, operator, expr),
        Expression::IncDec { kind, target } => incdec::gen(env, kind, target),

        Expression::Grouped(inner) => env.gen(inner, expected_type),
        Expression::ArrayLiteral(elements) => arrays::gen_array_literal(env, elements, expected_type),

        _ => unimplemented!("Unsupported expression type"),
    }
}

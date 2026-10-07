// SPDX-License-Identifier: MPL-2.0
//! Keep specialization's recursive expression frames small, including source wrappers.
use super::*;

pub(super) fn rewrite_expression(
    expr: Expression,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Expression, String> {
    match expr {
        Expression::Located { .. } => rewrite_located(expr, subst, env),
        Expression::FunctionCall { .. } => rewrite_function_call(expr, subst, env),
        Expression::MethodCall { .. } => rewrite_method_call(expr, subst, env),
        Expression::StructLiteral { .. } => rewrite_struct_literal(expr, subst, env),
        Expression::Deref(..) => rewrite_deref(expr, subst, env),
        Expression::AddressOf(..) => rewrite_address_of(expr, subst, env),
        Expression::BinaryExpression { .. } => rewrite_binary_expression(expr, subst, env),
        Expression::IndexAccess { .. } => rewrite_index_access(expr, subst, env),
        Expression::ArrayLiteral(..) => rewrite_array_literal(expr, subst, env),
        Expression::Await(..) => rewrite_await(expr, subst, env),
        Expression::Grouped(..) => rewrite_grouped(expr, subst, env),
        Expression::AssignOperation { .. } => rewrite_assign_operation(expr, subst, env),
        Expression::Assignment { .. } => rewrite_assignment(expr, subst, env),
        Expression::AsmBlock { .. } => rewrite_asm_block(expr, subst, env),
        Expression::FieldAccess { .. } => rewrite_field_access(expr, subst, env),
        Expression::Unary { .. } => rewrite_unary(expr, subst, env),
        Expression::Cast { .. } => rewrite_cast(expr, subst, env),
        Expression::IncDec { .. } => rewrite_inc_dec(expr, subst, env),
        other => Ok(other),
    }
}

fn rewrite_located(
    expression: Expression,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Expression, String> {
    let Expression::Located { value, span } = expression else {
        unreachable!("expression dispatch")
    };

    let span = if subst.is_empty() { span } else { span.generated("generic specialization") };
    Ok(rewrite_expression(*value, subst, env)?.with_span(Some(span)))
}

fn rewrite_function_call(
    expression: Expression,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Expression, String> {
    let Expression::FunctionCall { name, type_args, args } = expression else {
        unreachable!("expression dispatch")
    };

    let mut args = rewrite_expr_list(args, subst, env)?;
    append_default_arguments(&name, &mut args, env)?;

    if type_args.is_empty() {
        if env.function_templates.contains_key(&name) {
            return Err(format!("generic function '{}' requires explicit type arguments", name));
        }
        return Ok(Expression::FunctionCall { name, type_args, args });
    }

    let concrete_args: Vec<WaveType> = type_args
        .iter()
        .map(|t| rewrite_wave_type(t, subst, env))
        .collect::<Result<Vec<_>, _>>()?;

    if crate::async_intrinsics::is_intrinsic(&name) || crate::layout_intrinsics::is_intrinsic(&name)
    {
        return Ok(Expression::FunctionCall { name, type_args: concrete_args, args });
    }
    if !env.function_templates.contains_key(&name) {
        return Err(format!("type arguments provided for non-generic function '{}'", name));
    }

    let instantiated = ensure_function_instance(&name, &concrete_args, env)?;
    Ok(Expression::FunctionCall { name: instantiated, type_args: Vec::new(), args })
}

fn rewrite_method_call(
    expression: Expression,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Expression, String> {
    let Expression::MethodCall { object, name, args, type_args } = expression else {
        unreachable!("expression dispatch")
    };
    Ok(Expression::MethodCall {
        object: Box::new(rewrite_expression(*object, subst, env)?),
        name,
        type_args: type_args
            .iter()
            .map(|t| rewrite_wave_type(t, subst, env))
            .collect::<Result<_, _>>()?,
        args: rewrite_expr_list(args, subst, env)?,
    })
}

fn rewrite_struct_literal(
    expression: Expression,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Expression, String> {
    let Expression::StructLiteral { name, fields } = expression else {
        unreachable!("expression dispatch")
    };

    let rewritten_name = rewrite_struct_name_usage(&name, subst, env)?;
    let mut rewritten_fields = Vec::with_capacity(fields.len());
    for (fname, value) in fields {
        rewritten_fields.push((fname, rewrite_expression(value, subst, env)?));
    }
    Ok(Expression::StructLiteral { name: rewritten_name, fields: rewritten_fields })
}

fn rewrite_deref(
    expression: Expression,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Expression, String> {
    let Expression::Deref(inner) = expression else { unreachable!("expression dispatch") };
    Ok(Expression::Deref(Box::new(rewrite_expression(*inner, subst, env)?)))
}

fn rewrite_address_of(
    expression: Expression,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Expression, String> {
    let Expression::AddressOf(inner) = expression else { unreachable!("expression dispatch") };
    Ok(Expression::AddressOf(Box::new(rewrite_expression(*inner, subst, env)?)))
}

fn rewrite_binary_expression(
    expression: Expression,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Expression, String> {
    let Expression::BinaryExpression { left, operator, right } = expression else {
        unreachable!("expression dispatch")
    };
    Ok(Expression::BinaryExpression {
        left: Box::new(rewrite_expression(*left, subst, env)?),
        operator,
        right: Box::new(rewrite_expression(*right, subst, env)?),
    })
}

fn rewrite_index_access(
    expression: Expression,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Expression, String> {
    let Expression::IndexAccess { target, index } = expression else {
        unreachable!("expression dispatch")
    };
    Ok(Expression::IndexAccess {
        target: Box::new(rewrite_expression(*target, subst, env)?),
        index: Box::new(rewrite_expression(*index, subst, env)?),
    })
}

fn rewrite_array_literal(
    expression: Expression,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Expression, String> {
    let Expression::ArrayLiteral(items) = expression else { unreachable!("expression dispatch") };
    Ok(Expression::ArrayLiteral(rewrite_expr_list(items, subst, env)?))
}

fn rewrite_await(
    expression: Expression,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Expression, String> {
    let Expression::Await(inner) = expression else { unreachable!("expression dispatch") };
    Ok(Expression::Await(Box::new(rewrite_expression(*inner, subst, env)?)))
}

fn rewrite_grouped(
    expression: Expression,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Expression, String> {
    let Expression::Grouped(inner) = expression else { unreachable!("expression dispatch") };
    Ok(Expression::Grouped(Box::new(rewrite_expression(*inner, subst, env)?)))
}

fn rewrite_assign_operation(
    expression: Expression,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Expression, String> {
    let Expression::AssignOperation { target, operator, value } = expression else {
        unreachable!("expression dispatch")
    };
    Ok(Expression::AssignOperation {
        target: Box::new(rewrite_expression(*target, subst, env)?),
        operator,
        value: Box::new(rewrite_expression(*value, subst, env)?),
    })
}

fn rewrite_assignment(
    expression: Expression,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Expression, String> {
    let Expression::Assignment { target, value } = expression else {
        unreachable!("expression dispatch")
    };
    Ok(Expression::Assignment {
        target: Box::new(rewrite_expression(*target, subst, env)?),
        value: Box::new(rewrite_expression(*value, subst, env)?),
    })
}

fn rewrite_asm_block(
    expression: Expression,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Expression, String> {
    let Expression::AsmBlock { instructions, inputs, outputs, clobbers } = expression else {
        unreachable!("expression dispatch")
    };
    Ok(Expression::AsmBlock {
        instructions,
        inputs: inputs
            .into_iter()
            .map(|(r, e)| Ok((r, rewrite_expression(e, subst, env)?)))
            .collect::<Result<Vec<_>, String>>()?,
        outputs: outputs
            .into_iter()
            .map(|(r, e)| Ok((r, rewrite_expression(e, subst, env)?)))
            .collect::<Result<Vec<_>, String>>()?,
        clobbers,
    })
}

fn rewrite_field_access(
    expression: Expression,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Expression, String> {
    let Expression::FieldAccess { object, field } = expression else {
        unreachable!("expression dispatch")
    };
    Ok(Expression::FieldAccess {
        object: Box::new(rewrite_expression(*object, subst, env)?),
        field,
    })
}

fn rewrite_unary(
    expression: Expression,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Expression, String> {
    let Expression::Unary { operator, expr } = expression else {
        unreachable!("expression dispatch")
    };
    Ok(Expression::Unary { operator, expr: Box::new(rewrite_expression(*expr, subst, env)?) })
}

fn rewrite_cast(
    expression: Expression,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Expression, String> {
    let Expression::Cast { expr, target_type } = expression else {
        unreachable!("expression dispatch")
    };
    Ok(Expression::Cast {
        expr: Box::new(rewrite_expression(*expr, subst, env)?),
        target_type: rewrite_wave_type(&target_type, subst, env)?,
    })
}

fn rewrite_inc_dec(
    expression: Expression,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Expression, String> {
    let Expression::IncDec { kind, target } = expression else {
        unreachable!("expression dispatch")
    };
    Ok(Expression::IncDec { kind, target: Box::new(rewrite_expression(*target, subst, env)?) })
}

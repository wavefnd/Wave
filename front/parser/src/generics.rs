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

use crate::ast::{
    ASTNode, EnumNode, Expression, ExternFunctionNode, FunctionNode, Literal, MatchArm,
    MatchPattern, ParameterNode, ProtoImplNode, StatementNode, StructNode, TypeAliasNode, Value,
    VariableNode, WaveType,
};
use crate::types::{parse_type, split_top_level_generic_args, token_type_to_wave_type};
use std::collections::{BTreeMap, HashMap, HashSet};

#[derive(Default)]
struct GenericEnv {
    function_templates: HashMap<String, FunctionNode>,
    function_parameters: HashMap<String, Vec<ParameterNode>>,
    struct_templates: HashMap<String, StructNode>,

    function_instances: BTreeMap<String, FunctionNode>,
    struct_instances: BTreeMap<String, StructNode>,

    function_in_progress: HashSet<String>,
    struct_in_progress: HashSet<String>,
}

pub fn monomorphize_generics(ast: Vec<ASTNode>) -> Result<Vec<ASTNode>, String> {
    let mut env = GenericEnv::default();

    for node in &ast {
        match node {
            ASTNode::Function(f) => {
                if env
                    .function_parameters
                    .insert(f.name.clone(), f.parameters.clone())
                    .is_some()
                {
                    return Err(format!("duplicate function '{}'", f.name));
                }

                if f.generic_params.is_empty() {
                    continue;
                }

                if env
                    .function_templates
                    .insert(f.name.clone(), f.clone())
                    .is_some()
                {
                    return Err(format!("duplicate generic function template '{}'", f.name));
                }
            }
            #[allow(clippy::collapsible_match)]
            ASTNode::Struct(s) if !s.generic_params.is_empty() => {
                if env
                    .struct_templates
                    .insert(s.name.clone(), s.clone())
                    .is_some()
                {
                    return Err(format!("duplicate generic struct template '{}'", s.name));
                }
            }
            _ => {}
        }
    }

    let mut out: Vec<ASTNode> = Vec::new();
    let empty_subst: HashMap<String, WaveType> = HashMap::new();

    for node in ast {
        match node {
            ASTNode::Function(f) => {
                if f.generic_params.is_empty() {
                    out.push(ASTNode::Function(rewrite_function(
                        f,
                        &empty_subst,
                        &mut env,
                    )?));
                }
            }
            ASTNode::Struct(s) if s.generic_params.is_empty() => {
                out.push(ASTNode::Struct(rewrite_struct(s, &empty_subst, &mut env)?));
            }

            ASTNode::Struct(_) => {}
            ASTNode::Variable(v) => {
                out.push(ASTNode::Variable(rewrite_variable(
                    v,
                    &empty_subst,
                    &mut env,
                )?));
            }
            ASTNode::ExternFunction(e) => {
                out.push(ASTNode::ExternFunction(rewrite_extern(
                    e,
                    &empty_subst,
                    &mut env,
                )?));
            }
            ASTNode::ProtoImpl(p) => {
                out.push(ASTNode::ProtoImpl(rewrite_proto(
                    p,
                    &empty_subst,
                    &mut env,
                )?));
            }
            ASTNode::TypeAlias(TypeAliasNode { name, target }) => {
                out.push(ASTNode::TypeAlias(TypeAliasNode {
                    name,
                    target: rewrite_wave_type(&target, &empty_subst, &mut env)?,
                }));
            }
            ASTNode::Enum(EnumNode {
                name,
                repr_type,
                variants,
            }) => {
                out.push(ASTNode::Enum(EnumNode {
                    name,
                    repr_type: rewrite_wave_type(&repr_type, &empty_subst, &mut env)?,
                    variants,
                }));
            }
            ASTNode::Statement(stmt) => {
                out.push(ASTNode::Statement(rewrite_statement(
                    stmt,
                    &empty_subst,
                    &mut env,
                )?));
            }
            ASTNode::Expression(expr) => {
                out.push(ASTNode::Expression(rewrite_expression(
                    expr,
                    &empty_subst,
                    &mut env,
                )?));
            }
            ASTNode::Program(p) => out.push(ASTNode::Program(p)),
        }
    }

    for (_, s) in env.struct_instances {
        out.push(ASTNode::Struct(s));
    }
    for (_, f) in env.function_instances {
        out.push(ASTNode::Function(f));
    }

    Ok(out)
}

fn rewrite_parameter(
    param: ParameterNode,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<ParameterNode, String> {
    Ok(ParameterNode {
        name: param.name,
        param_type: rewrite_wave_type(&param.param_type, subst, env)?,
        initial_value: param.initial_value,
    })
}

fn rewrite_function(
    mut f: FunctionNode,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<FunctionNode, String> {
    if !f.generic_params.is_empty() {
        return Err(format!(
            "internal error: unresolved generic params in function '{}': {:?}",
            f.name, f.generic_params
        ));
    }

    f.parameters = f
        .parameters
        .into_iter()
        .map(|p| rewrite_parameter(p, subst, env))
        .collect::<Result<Vec<_>, _>>()?;

    f.return_type = f
        .return_type
        .as_ref()
        .map(|t| rewrite_wave_type(t, subst, env))
        .transpose()?;

    f.body = f
        .body
        .into_iter()
        .map(|n| rewrite_node(n, subst, env))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(f)
}

fn rewrite_struct(
    mut s: StructNode,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<StructNode, String> {
    if !s.generic_params.is_empty() {
        return Err(format!(
            "internal error: unresolved generic params in struct '{}': {:?}",
            s.name, s.generic_params
        ));
    }

    s.fields = s
        .fields
        .into_iter()
        .map(|(n, t)| Ok((n, rewrite_wave_type(&t, subst, env)?)))
        .collect::<Result<Vec<_>, String>>()?;

    s.methods = s
        .methods
        .into_iter()
        .map(|m| {
            if !m.generic_params.is_empty() {
                return Err(format!(
                    "generic methods are not supported yet: '{}::{}'",
                    s.name, m.name
                ));
            }
            rewrite_function(m, subst, env)
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(s)
}

fn rewrite_proto(
    mut p: ProtoImplNode,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<ProtoImplNode, String> {
    p.methods = p
        .methods
        .into_iter()
        .map(|m| {
            if !m.generic_params.is_empty() {
                return Err(format!(
                    "generic methods are not supported yet: 'proto {}::{}'",
                    p.target, m.name
                ));
            }
            rewrite_function(m, subst, env)
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(p)
}

fn rewrite_extern(
    mut e: ExternFunctionNode,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<ExternFunctionNode, String> {
    e.params = e
        .params
        .into_iter()
        .map(|(n, t)| Ok((n, rewrite_wave_type(&t, subst, env)?)))
        .collect::<Result<Vec<_>, String>>()?;
    e.return_type = rewrite_wave_type(&e.return_type, subst, env)?;
    Ok(e)
}

fn rewrite_variable(
    mut v: VariableNode,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<VariableNode, String> {
    v.type_name = rewrite_wave_type(&v.type_name, subst, env)?;
    v.initial_value = v
        .initial_value
        .as_ref()
        .map(|e| rewrite_expression(e.clone(), subst, env))
        .transpose()?;
    Ok(v)
}

fn rewrite_node(
    node: ASTNode,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<ASTNode, String> {
    match node {
        ASTNode::Variable(v) => Ok(ASTNode::Variable(rewrite_variable(v, subst, env)?)),
        ASTNode::Statement(s) => Ok(ASTNode::Statement(rewrite_statement(s, subst, env)?)),
        ASTNode::Expression(e) => Ok(ASTNode::Expression(rewrite_expression(e, subst, env)?)),
        ASTNode::Function(f) => Ok(ASTNode::Function(rewrite_function(f, subst, env)?)),
        ASTNode::Struct(s) => Ok(ASTNode::Struct(rewrite_struct(s, subst, env)?)),
        ASTNode::ExternFunction(e) => Ok(ASTNode::ExternFunction(rewrite_extern(e, subst, env)?)),
        ASTNode::ProtoImpl(p) => Ok(ASTNode::ProtoImpl(rewrite_proto(p, subst, env)?)),
        ASTNode::TypeAlias(TypeAliasNode { name, target }) => {
            Ok(ASTNode::TypeAlias(TypeAliasNode {
                name,
                target: rewrite_wave_type(&target, subst, env)?,
            }))
        }
        ASTNode::Enum(EnumNode {
            name,
            repr_type,
            variants,
        }) => Ok(ASTNode::Enum(EnumNode {
            name,
            repr_type: rewrite_wave_type(&repr_type, subst, env)?,
            variants,
        })),
        ASTNode::Program(p) => Ok(ASTNode::Program(p)),
    }
}

fn rewrite_statement(
    stmt: StatementNode,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<StatementNode, String> {
    match stmt {
        StatementNode::PrintFormat { format, args } => Ok(StatementNode::PrintFormat {
            format,
            args: rewrite_expr_list(args, subst, env)?,
        }),
        StatementNode::PrintlnFormat { format, args } => Ok(StatementNode::PrintlnFormat {
            format,
            args: rewrite_expr_list(args, subst, env)?,
        }),
        StatementNode::Input { format, args } => Ok(StatementNode::Input {
            format,
            args: rewrite_expr_list(args, subst, env)?,
        }),
        StatementNode::If {
            condition,
            body,
            else_if_blocks,
            else_block,
        } => Ok(StatementNode::If {
            condition: rewrite_expression(condition, subst, env)?,
            body: rewrite_node_list(body, subst, env)?,
            else_if_blocks: else_if_blocks
                .map(|blocks| {
                    blocks
                        .into_iter()
                        .map(|(cond, body)| {
                            Ok((
                                rewrite_expression(cond, subst, env)?,
                                rewrite_node_list(body, subst, env)?,
                            ))
                        })
                        .collect::<Result<Vec<_>, String>>()
                })
                .transpose()?
                .map(Box::new),
            else_block: else_block
                .map(|body| rewrite_node_list(*body, subst, env).map(Box::new))
                .transpose()?,
        }),
        StatementNode::For {
            initialization,
            condition,
            increment,
            body,
        } => Ok(StatementNode::For {
            initialization: Box::new(rewrite_node(*initialization, subst, env)?),
            condition: rewrite_expression(condition, subst, env)?,
            increment: rewrite_expression(increment, subst, env)?,
            body: rewrite_node_list(body, subst, env)?,
        }),
        StatementNode::While { condition, body } => Ok(StatementNode::While {
            condition: rewrite_expression(condition, subst, env)?,
            body: rewrite_node_list(body, subst, env)?,
        }),
        StatementNode::Match { value, arms } => Ok(StatementNode::Match {
            value: rewrite_expression(value, subst, env)?,
            arms: arms
                .into_iter()
                .map(|arm| {
                    Ok(MatchArm {
                        pattern: rewrite_pattern(arm.pattern),
                        body: rewrite_node_list(arm.body, subst, env)?,
                    })
                })
                .collect::<Result<Vec<_>, String>>()?,
        }),
        StatementNode::Assign { variable, value } => Ok(StatementNode::Assign {
            variable,
            value: rewrite_expression(value, subst, env)?,
        }),
        StatementNode::AsmBlock {
            instructions,
            inputs,
            outputs,
            clobbers,
        } => Ok(StatementNode::AsmBlock {
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
        }),
        StatementNode::Expression(e) => Ok(StatementNode::Expression(rewrite_expression(
            e, subst, env,
        )?)),
        StatementNode::Return(v) => Ok(StatementNode::Return(
            v.map(|e| rewrite_expression(e, subst, env)).transpose()?,
        )),
        StatementNode::Print(s) => Ok(StatementNode::Print(s)),
        StatementNode::Println(s) => Ok(StatementNode::Println(s)),
        StatementNode::Variable(v) => Ok(StatementNode::Variable(v)),
        StatementNode::Import(s) => Ok(StatementNode::Import(s)),
        StatementNode::Break => Ok(StatementNode::Break),
        StatementNode::Continue => Ok(StatementNode::Continue),
    }
}

fn rewrite_pattern(pattern: MatchPattern) -> MatchPattern {
    pattern
}

fn rewrite_expr_list(
    exprs: Vec<Expression>,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Vec<Expression>, String> {
    exprs
        .into_iter()
        .map(|e| rewrite_expression(e, subst, env))
        .collect()
}

fn rewrite_node_list(
    nodes: Vec<ASTNode>,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Vec<ASTNode>, String> {
    nodes
        .into_iter()
        .map(|n| rewrite_node(n, subst, env))
        .collect()
}

fn rewrite_expression(
    expr: Expression,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<Expression, String> {
    match expr {
        Expression::FunctionCall {
            name,
            type_args,
            args,
        } => {
            let mut args = rewrite_expr_list(args, subst, env)?;
            append_default_arguments(&name, &mut args, env)?;

            if type_args.is_empty() {
                if env.function_templates.contains_key(&name) {
                    return Err(format!(
                        "generic function '{}' requires explicit type arguments",
                        name
                    ));
                }
                return Ok(Expression::FunctionCall {
                    name,
                    type_args,
                    args,
                });
            }

            let concrete_args: Vec<WaveType> = type_args
                .iter()
                .map(|t| rewrite_wave_type(t, subst, env))
                .collect::<Result<Vec<_>, _>>()?;

            if !env.function_templates.contains_key(&name) {
                return Err(format!(
                    "type arguments provided for non-generic function '{}'",
                    name
                ));
            }

            let instantiated = ensure_function_instance(&name, &concrete_args, env)?;
            Ok(Expression::FunctionCall {
                name: instantiated,
                type_args: Vec::new(),
                args,
            })
        }
        Expression::MethodCall { object, name, args } => Ok(Expression::MethodCall {
            object: Box::new(rewrite_expression(*object, subst, env)?),
            name,
            args: rewrite_expr_list(args, subst, env)?,
        }),
        Expression::StructLiteral { name, fields } => {
            let rewritten_name = rewrite_struct_name_usage(&name, subst, env)?;
            let mut rewritten_fields = Vec::with_capacity(fields.len());
            for (fname, value) in fields {
                rewritten_fields.push((fname, rewrite_expression(value, subst, env)?));
            }
            Ok(Expression::StructLiteral {
                name: rewritten_name,
                fields: rewritten_fields,
            })
        }
        Expression::Deref(inner) => Ok(Expression::Deref(Box::new(rewrite_expression(
            *inner, subst, env,
        )?))),
        Expression::AddressOf(inner) => Ok(Expression::AddressOf(Box::new(rewrite_expression(
            *inner, subst, env,
        )?))),
        Expression::BinaryExpression {
            left,
            operator,
            right,
        } => Ok(Expression::BinaryExpression {
            left: Box::new(rewrite_expression(*left, subst, env)?),
            operator,
            right: Box::new(rewrite_expression(*right, subst, env)?),
        }),
        Expression::IndexAccess { target, index } => Ok(Expression::IndexAccess {
            target: Box::new(rewrite_expression(*target, subst, env)?),
            index: Box::new(rewrite_expression(*index, subst, env)?),
        }),
        Expression::ArrayLiteral(items) => Ok(Expression::ArrayLiteral(rewrite_expr_list(
            items, subst, env,
        )?)),
        Expression::Grouped(inner) => Ok(Expression::Grouped(Box::new(rewrite_expression(
            *inner, subst, env,
        )?))),
        Expression::AssignOperation {
            target,
            operator,
            value,
        } => Ok(Expression::AssignOperation {
            target: Box::new(rewrite_expression(*target, subst, env)?),
            operator,
            value: Box::new(rewrite_expression(*value, subst, env)?),
        }),
        Expression::Assignment { target, value } => Ok(Expression::Assignment {
            target: Box::new(rewrite_expression(*target, subst, env)?),
            value: Box::new(rewrite_expression(*value, subst, env)?),
        }),
        Expression::AsmBlock {
            instructions,
            inputs,
            outputs,
            clobbers,
        } => Ok(Expression::AsmBlock {
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
        }),
        Expression::FieldAccess { object, field } => Ok(Expression::FieldAccess {
            object: Box::new(rewrite_expression(*object, subst, env)?),
            field,
        }),
        Expression::Unary { operator, expr } => Ok(Expression::Unary {
            operator,
            expr: Box::new(rewrite_expression(*expr, subst, env)?),
        }),
        Expression::Cast { expr, target_type } => Ok(Expression::Cast {
            expr: Box::new(rewrite_expression(*expr, subst, env)?),
            target_type: rewrite_wave_type(&target_type, subst, env)?,
        }),
        Expression::IncDec { kind, target } => Ok(Expression::IncDec {
            kind,
            target: Box::new(rewrite_expression(*target, subst, env)?),
        }),
        other => Ok(other),
    }
}

fn append_default_arguments(
    name: &str,
    args: &mut Vec<Expression>,
    env: &GenericEnv,
) -> Result<(), String> {
    let Some(parameters) = env.function_parameters.get(name) else {
        return Ok(());
    };

    if args.len() > parameters.len() {
        return Err(format!(
            "function '{}' expects at most {} arguments, got {}",
            name,
            parameters.len(),
            args.len()
        ));
    }

    for parameter in parameters.iter().skip(args.len()) {
        let default = parameter
            .initial_value
            .as_ref()
            .ok_or_else(|| format!("function '{}' requires argument '{}'", name, parameter.name))?;
        args.push(value_to_expression(default));
    }

    Ok(())
}

fn value_to_expression(value: &Value) -> Expression {
    match value {
        Value::Int(value) => Expression::Literal(Literal::Int(value.to_string())),
        Value::Float(value) => Expression::Literal(Literal::Float(*value)),
        Value::Text(value) => Expression::Literal(Literal::String(value.clone())),
    }
}

fn rewrite_wave_type(
    ty: &WaveType,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<WaveType, String> {
    match ty {
        WaveType::Pointer(inner) => Ok(WaveType::Pointer(Box::new(rewrite_wave_type(
            inner, subst, env,
        )?))),
        WaveType::Array(inner, n) => Ok(WaveType::Array(
            Box::new(rewrite_wave_type(inner, subst, env)?),
            *n,
        )),
        WaveType::Struct(name) => rewrite_struct_type(name, subst, env),
        _ => Ok(ty.clone()),
    }
}

fn rewrite_struct_type(
    name: &str,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<WaveType, String> {
    if let Some(mapped) = subst.get(name) {
        return Ok(mapped.clone());
    }

    if let Some((base, arg_strs)) = parse_type_application(name)? {
        if !env.struct_templates.contains_key(&base) {
            return Err(format!(
                "generic type '{}' is not declared as a generic struct",
                base
            ));
        }

        let mut concrete_args: Vec<WaveType> = Vec::with_capacity(arg_strs.len());
        for arg in arg_strs {
            let parsed = parse_wave_type_from_str(&arg)?;
            concrete_args.push(rewrite_wave_type(&parsed, subst, env)?);
        }

        let instantiated = ensure_struct_instance(&base, &concrete_args, env)?;
        return Ok(WaveType::Struct(instantiated));
    }

    if let Some(template) = env.struct_templates.get(name) {
        if !template.generic_params.is_empty() {
            return Err(format!(
                "generic struct '{}' requires explicit type arguments",
                name
            ));
        }
    }

    Ok(WaveType::Struct(name.to_string()))
}

fn rewrite_struct_name_usage(
    name: &str,
    subst: &HashMap<String, WaveType>,
    env: &mut GenericEnv,
) -> Result<String, String> {
    match rewrite_struct_type(name, subst, env)? {
        WaveType::Struct(n) => Ok(n),
        other => Err(format!(
            "invalid struct literal target '{}': expected struct type, got {:?}",
            name, other
        )),
    }
}

fn ensure_struct_instance(
    base: &str,
    args: &[WaveType],
    env: &mut GenericEnv,
) -> Result<String, String> {
    let template = env
        .struct_templates
        .get(base)
        .cloned()
        .ok_or_else(|| format!("unknown generic struct template '{}'", base))?;

    if template.generic_params.len() != args.len() {
        return Err(format!(
            "generic struct '{}' expects {} type arguments, got {}",
            base,
            template.generic_params.len(),
            args.len()
        ));
    }

    let inst_name = mangle_instance_name(base, args);
    if env.struct_instances.contains_key(&inst_name) {
        return Ok(inst_name);
    }
    if env.struct_in_progress.contains(&inst_name) {
        return Ok(inst_name);
    }

    let mut map: HashMap<String, WaveType> = HashMap::new();
    for (k, v) in template.generic_params.iter().zip(args.iter()) {
        map.insert(k.clone(), v.clone());
    }

    env.struct_in_progress.insert(inst_name.clone());

    let mut instantiated = template;
    instantiated.name = inst_name.clone();
    instantiated.generic_params.clear();
    instantiated = rewrite_struct(instantiated, &map, env)?;

    env.struct_in_progress.remove(&inst_name);
    env.struct_instances
        .insert(inst_name.clone(), instantiated)
        .map(|_| ())
        .unwrap_or(());

    Ok(inst_name)
}

fn ensure_function_instance(
    base: &str,
    args: &[WaveType],
    env: &mut GenericEnv,
) -> Result<String, String> {
    let template = env
        .function_templates
        .get(base)
        .cloned()
        .ok_or_else(|| format!("unknown generic function template '{}'", base))?;

    if template.generic_params.len() != args.len() {
        return Err(format!(
            "generic function '{}' expects {} type arguments, got {}",
            base,
            template.generic_params.len(),
            args.len()
        ));
    }

    let inst_name = mangle_instance_name(base, args);
    if env.function_instances.contains_key(&inst_name) {
        return Ok(inst_name);
    }
    if env.function_in_progress.contains(&inst_name) {
        return Ok(inst_name);
    }

    let mut map: HashMap<String, WaveType> = HashMap::new();
    for (k, v) in template.generic_params.iter().zip(args.iter()) {
        map.insert(k.clone(), v.clone());
    }

    env.function_in_progress.insert(inst_name.clone());

    let mut instantiated = template;
    instantiated.name = inst_name.clone();
    instantiated.generic_params.clear();
    instantiated = rewrite_function(instantiated, &map, env)?;

    env.function_in_progress.remove(&inst_name);
    env.function_instances
        .insert(inst_name.clone(), instantiated)
        .map(|_| ())
        .unwrap_or(());

    Ok(inst_name)
}

fn parse_type_application(name: &str) -> Result<Option<(String, Vec<String>)>, String> {
    let s = name.trim();
    let Some(lt) = s.find('<') else {
        return Ok(None);
    };
    if !s.ends_with('>') {
        return Err(format!(
            "malformed generic type '{}': missing closing '>'",
            s
        ));
    }

    let base = s[..lt].trim();
    if base.is_empty() {
        return Err(format!("malformed generic type '{}': missing base name", s));
    }
    let inner = &s[lt + 1..s.len() - 1];
    let args = split_top_level_generic_args(inner).ok_or_else(|| {
        format!(
            "malformed generic type '{}': invalid generic argument list",
            name
        )
    })?;
    Ok(Some((base.to_string(), args)))
}

fn parse_wave_type_from_str(raw: &str) -> Result<WaveType, String> {
    let tt = parse_type(raw).ok_or_else(|| format!("invalid type syntax '{}'", raw))?;
    token_type_to_wave_type(&tt)
        .ok_or_else(|| format!("unsupported type '{}' in generic argument", raw))
}

fn mangle_instance_name(base: &str, args: &[WaveType]) -> String {
    let mut out = String::with_capacity(base.len() + 16);
    out.push_str(base);
    out.push_str("$g");
    for arg in args {
        out.push('$');
        out.push_str(&mangle_type(arg));
    }
    out
}

fn mangle_type(ty: &WaveType) -> String {
    match ty {
        WaveType::Int(n) => format!("i{}", n),
        WaveType::Uint(n) => format!("u{}", n),
        WaveType::Float(n) => format!("f{}", n),
        WaveType::Bool => "bool".to_string(),
        WaveType::Char => "char".to_string(),
        WaveType::Byte => "byte".to_string(),
        WaveType::String => "str".to_string(),
        WaveType::Void => "void".to_string(),
        WaveType::Pointer(inner) => format!("p_{}", mangle_type(inner)),
        WaveType::Array(inner, n) => format!("a{}_{}", n, mangle_type(inner)),
        WaveType::Struct(name) => sanitize_ident(name),
    }
}

fn sanitize_ident(raw: &str) -> String {
    let mut out = String::with_capacity(raw.len());
    for c in raw.chars() {
        if c.is_ascii_alphanumeric() || c == '_' {
            out.push(c);
        } else {
            out.push('_');
        }
    }
    out
}

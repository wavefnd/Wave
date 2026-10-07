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

//! Whole-program semantic validation and expression type analysis.
//!
//! The verifier runs after imports and generics have been expanded. It first
//! collects declarations into a program-wide type environment, then validates
//! bodies with lexical scopes and expected types. Physical diagnostics use the
//! detached AST source map; legacy unlocated callers retain descriptive hints.

mod const_dependencies;
mod diagnostics;
mod expression;
mod facts;
pub use facts::{AnalyzedExpressionType, SemanticFacts, VariantConstruction, VariantPattern};
mod model;
mod numeric;
mod program;

use diagnostics::top_level_span_hint;
pub use diagnostics::{SemanticDiagnostic, SemanticSpanHint, SemanticSpanKind};
use model::*;
use numeric::*;
use program::*;

use crate::ast::{
    ASTNode, AssignOperator, Expression, FunctionNode, IncDecKind, Literal, MatchPattern,
    Mutability, Operator, StatementNode, WaveType,
};
use std::collections::{HashMap, HashSet};

fn infer_variant_substitution(
    program: &ProgramTypes,
    template: &WaveType,
    actual: &ExpressionType,
    generic_params: &[String],
    substitutions: &mut HashMap<String, WaveType>,
) -> Result<(), String> {
    let actual = match actual {
        ExpressionType::Known(ty) => program.canonical_type(ty),
        ExpressionType::IntLiteral(_) => WaveType::Int(32),
        ExpressionType::FloatLiteral => WaveType::Float(32),
        _ => return Ok(()),
    };
    infer_variant_type_pair(program, template, &actual, generic_params, substitutions)
}

fn infer_variant_type_pair(
    program: &ProgramTypes,
    template: &WaveType,
    actual: &WaveType,
    generic_params: &[String],
    substitutions: &mut HashMap<String, WaveType>,
) -> Result<(), String> {
    if let WaveType::Struct(name) = template {
        if generic_params.contains(name) {
            if let Some(previous) = substitutions.get(name) {
                if program.canonical_type(previous) != program.canonical_type(actual) {
                    return Err(format!(
                        "conflicting inferred types `{}` and `{}` for generic parameter `{}`",
                        display_wave_type(previous),
                        display_wave_type(actual),
                        name
                    ));
                }
            } else {
                substitutions.insert(name.clone(), actual.clone());
            }
            return Ok(());
        }
    }

    match (template, actual) {
        (WaveType::Future(template), WaveType::Future(actual))
        | (WaveType::Pointer(template), WaveType::Pointer(actual))
        | (WaveType::Array(template, _), WaveType::Array(actual, _)) => {
            infer_variant_type_pair(program, template, actual, generic_params, substitutions)
        },
        (WaveType::Struct(template_name), WaveType::Struct(actual_name))
        | (WaveType::Struct(template_name), WaveType::Variant(actual_name))
        | (WaveType::Variant(template_name), WaveType::Struct(actual_name))
        | (WaveType::Variant(template_name), WaveType::Variant(actual_name)) => {
            let Some((template_base, template_args)) = parse_named_type_application(template_name)
            else {
                return Ok(());
            };
            let Some((actual_base, actual_args)) = parse_named_type_application(actual_name) else {
                return Ok(());
            };
            if template_base != actual_base || template_args.len() != actual_args.len() {
                return Ok(());
            }
            for (template_arg, actual_arg) in template_args.iter().zip(&actual_args) {
                infer_variant_type_pair(
                    program,
                    template_arg,
                    actual_arg,
                    generic_params,
                    substitutions,
                )?;
            }
            Ok(())
        },
        _ => Ok(()),
    }
}

struct Validator<'a> {
    program: &'a ProgramTypes,
    scopes: Vec<HashMap<String, Binding>>,
    current_function: Option<String>,
    current_return_type: Option<WaveType>,
    current_async: bool,
    current_type_params: HashSet<String>,
    loop_depth: usize,
    top_level_index: usize,
    span_counts: HashMap<(SemanticSpanKind, String), usize>,
    primary_span: Option<SemanticSpanHint>,
    source_map: &'a crate::source::SourceMap,
    source_span: Option<error::SourceSpan>,
    diagnostic_help: Option<String>,
    expression_types: HashMap<usize, WaveType>,
    analyzed_expression_types: HashMap<usize, AnalyzedExpressionType>,
    expected_types: HashMap<usize, WaveType>,
    variant_constructions: HashMap<usize, VariantConstruction>,
    variant_patterns: HashMap<usize, VariantPattern>,
    integer_patterns: HashMap<usize, String>,
    generic_method_calls: HashMap<usize, crate::methods::GenericMethodCall>,
}

impl<'a> Validator<'a> {
    fn new(program: &'a ProgramTypes, source_map: &'a crate::source::SourceMap) -> Self {
        Self {
            program,
            scopes: vec![HashMap::new()],
            current_function: None,
            current_return_type: None,
            current_async: false,
            current_type_params: HashSet::new(),
            loop_depth: 0,
            top_level_index: 0,
            span_counts: HashMap::new(),
            primary_span: None,
            source_map,
            source_span: None,
            diagnostic_help: None,
            expression_types: HashMap::new(),
            analyzed_expression_types: HashMap::new(),
            expected_types: HashMap::new(),
            variant_constructions: HashMap::new(),
            variant_patterns: HashMap::new(),
            integer_patterns: HashMap::new(),
            generic_method_calls: HashMap::new(),
        }
    }

    fn begin_top_level(&mut self, index: usize, hint: SemanticSpanHint) {
        self.top_level_index = index;
        self.span_counts.clear();
        self.primary_span = Some(hint);
        self.diagnostic_help = None;
    }

    fn mark_span(&mut self, kind: SemanticSpanKind, text: impl Into<String>) {
        let text = text.into();
        let occurrence = self
            .span_counts
            .entry((kind.clone(), text.clone()))
            .and_modify(|count| *count += 1)
            .or_insert(1);
        self.primary_span = Some(SemanticSpanHint { kind, text, occurrence: *occurrence });
    }

    fn diagnostic(&self, message: String) -> SemanticDiagnostic {
        SemanticDiagnostic {
            code: "E3001".to_string(),
            label: message.clone(),
            message,
            top_level_index: self.top_level_index,
            primary: self.primary_span.clone(),
            span: self.source_span.clone(),
            note: None,
            help: self.diagnostic_help.clone().unwrap_or_else(|| {
                "fix type, mutability, scope, and control-flow errors".to_string()
            }),
        }
    }

    fn lookup_binding(&self, name: &str) -> Option<Binding> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).cloned())
            .or_else(|| self.program.globals.get(name).cloned())
    }

    fn with_scope<T>(
        &mut self,
        validate: impl FnOnce(&mut Self) -> Result<T, String>,
    ) -> Result<T, String> {
        self.scopes.push(HashMap::new());
        let result = validate(self);
        self.scopes.pop();
        result
    }

    fn validate_function(
        &mut self,
        function: &FunctionNode,
        display_name: &str,
        inherited_type_params: &[String],
    ) -> Result<(), String> {
        let mut declared_type_params: HashSet<&str> =
            inherited_type_params.iter().map(String::as_str).collect();
        for param in &function.generic_params {
            if !declared_type_params.insert(param) {
                return Err(format!(
                    "duplicate generic parameter `{}` in function `{}`",
                    param, display_name
                ));
            }
        }
        let previous_async = std::mem::replace(&mut self.current_async, function.is_async);
        let previous_function = self.current_function.replace(display_name.to_string());
        let previous_return = self
            .current_return_type
            .replace(function.return_type.clone().unwrap_or(WaveType::Void));
        let previous_loop_depth = std::mem::replace(&mut self.loop_depth, 0);
        let previous_type_params = std::mem::take(&mut self.current_type_params);
        self.current_type_params.extend(inherited_type_params.iter().cloned());
        self.current_type_params.extend(function.generic_params.iter().cloned());

        for parameter in &function.parameters {
            if let Some(span) = &parameter.span {
                self.source_span = Some(span.clone());
            }
            self.program.validate_type(
                &parameter.param_type,
                &self.current_type_params,
                false,
                &format!("parameter `{}` of function `{}`", parameter.name, display_name),
            )?;
        }
        let mut saw_default = false;
        for parameter in &function.parameters {
            if let Some(value) = &parameter.initial_value {
                saw_default = true;
                let actual = self.validate_expr_expected(value, Some(&parameter.param_type))?;
                self.require_assignable(
                    &actual,
                    &parameter.param_type,
                    &format!("default for `{}`", parameter.name),
                )?;
            } else if saw_default {
                return Err(format!(
                    "required parameter `{}` follows a default parameter",
                    parameter.name
                ));
            }
        }
        self.source_span = function.return_type_span.clone().or_else(|| function.span.clone());
        let return_type = function.return_type.clone().unwrap_or(WaveType::Void);
        self.program.validate_type(
            &return_type,
            &self.current_type_params,
            true,
            &format!("return type of function `{}`", display_name),
        )?;

        let result = self.with_scope(|validator| {
            for parameter in &function.parameters {
                validator.insert_current_binding(
                    parameter.name.clone(),
                    Binding { mutability: Mutability::Var, ty: parameter.param_type.clone() },
                    "parameter",
                )?;
            }

            let falls_through = validator.validate_block(&function.body)?;
            let return_type = function.return_type.clone().unwrap_or(WaveType::Void);
            if return_type != WaveType::Void && falls_through {
                validator.source_span = function.span.clone();
                return Err(format!(
                    "non-void function `{}` may exit without returning `{}`",
                    display_name,
                    display_wave_type(&return_type)
                ));
            }

            Ok(())
        });

        self.current_async = previous_async;
        self.current_function = previous_function;
        self.current_return_type = previous_return;
        self.loop_depth = previous_loop_depth;
        self.current_type_params = previous_type_params;
        result
    }

    fn insert_current_binding(
        &mut self,
        name: String,
        binding: Binding,
        kind: &str,
    ) -> Result<(), String> {
        let scope = self.scopes.last_mut().unwrap();
        if scope.contains_key(&name) {
            return Err(format!("duplicate {} declaration `{}` in the same scope", kind, name));
        }
        scope.insert(name, binding);
        Ok(())
    }

    fn validate_block(&mut self, nodes: &[ASTNode]) -> Result<bool, String> {
        let mut falls_through = true;
        for node in nodes {
            let node_falls_through = self.validate_node(node)?;
            if falls_through {
                falls_through = node_falls_through;
            }
        }
        Ok(falls_through)
    }

    fn validate_scoped_block(&mut self, nodes: &[ASTNode]) -> Result<bool, String> {
        self.with_scope(|validator| validator.validate_block(nodes))
    }

    fn validate_node(&mut self, node: &ASTNode) -> Result<bool, String> {
        if let Some(span) = self.source_map.nodes.get(&(node as *const _ as usize)) {
            self.source_span = Some(span.clone());
        }
        match node {
            ASTNode::Variable(variable) => {
                self.mark_span(SemanticSpanKind::Declaration, variable.name.clone());
                self.program.validate_type(
                    &variable.type_name,
                    &self.current_type_params,
                    false,
                    &format!("variable `{}`", variable.name),
                )?;
                if self.scopes.last().unwrap().contains_key(&variable.name) {
                    return Err(format!(
                        "duplicate variable declaration `{}` in the same scope",
                        variable.name
                    ));
                }
                let asm_initializer =
                    matches!(variable.initial_value, Some(Expression::AsmBlock { .. }));
                if asm_initializer {
                    self.insert_current_binding(
                        variable.name.clone(),
                        Binding { mutability: variable.mutability, ty: variable.type_name.clone() },
                        "variable",
                    )?;
                }

                if let Some(initial_value) = &variable.initial_value {
                    let actual =
                        self.validate_expr_expected(initial_value, Some(&variable.type_name))?;
                    self.require_assignable(
                        &actual,
                        &variable.type_name,
                        &format!("initializer for `{}`", variable.name),
                    )?;
                }

                if !asm_initializer {
                    self.insert_current_binding(
                        variable.name.clone(),
                        Binding { mutability: variable.mutability, ty: variable.type_name.clone() },
                        "variable",
                    )?;
                }
                Ok(true)
            },
            ASTNode::Statement(statement) => self.validate_statement(statement),
            ASTNode::Expression(expression) => {
                let ty = self.validate_expr(expression)?;
                Ok(!matches!(ty, ExpressionType::Known(WaveType::Never)))
            },
            _ => Ok(true),
        }
    }

    fn validate_statement(&mut self, statement: &StatementNode) -> Result<bool, String> {
        match statement {
            StatementNode::Expression(expression) => {
                let ty = self.validate_expr(expression)?;
                Ok(!matches!(ty, ExpressionType::Known(WaveType::Never)))
            },
            StatementNode::Assign { variable, value } => {
                self.mark_span(SemanticSpanKind::Identifier, variable.clone());
                let binding = self
                    .lookup_binding(variable)
                    .ok_or_else(|| format!("use of undeclared identifier `{}`", variable))?;
                self.ensure_mutable_binding(variable, &binding, "assign")?;
                let actual = self.validate_expr_expected(value, Some(&binding.ty))?;
                self.require_assignable(
                    &actual,
                    &binding.ty,
                    &format!("assignment to `{}`", variable),
                )?;
                Ok(true)
            },
            StatementNode::PrintFormat { args, format }
            | StatementNode::PrintlnFormat { args, format } => {
                self.mark_span(SemanticSpanKind::Keyword, "println|print");
                let specs = crate::format::format_fragments(format).map_err(str::to_string)?;
                let specs = specs.into_iter().filter_map(|p| match p {
                    crate::format::FormatFragment::Placeholder(s) => Some(s),
                    _ => None,
                });
                for (argument, spec) in args.iter().zip(specs) {
                    let ty = self.validate_expr(argument)?;
                    self.validate_format_argument(&ty)?;
                    if matches!(ty, ExpressionType::IntLiteral(_)) {
                        self.require_assignable(
                            &ty,
                            &WaveType::Int(32),
                            "default integer format argument",
                        )?;
                    }
                    let concrete = canonical_expression_type(self.program, &ty);
                    let generic =
                        concrete.as_ref().is_some_and(|t| self.program.is_generic_placeholder(t));
                    let valid = generic
                        || match spec {
                            "" => true,
                            "d" | "x" | "c" => {
                                self.is_integer_expression(&ty)
                                    || matches!(concrete, Some(WaveType::Bool))
                            },
                            "p" => {
                                matches!(concrete, Some(WaveType::Pointer(_) | WaveType::String))
                                    || matches!(ty, ExpressionType::Null)
                            },
                            "s" => {
                                matches!(concrete, Some(WaveType::String))
                                    || matches!(concrete, Some(WaveType::Pointer(ref t)) if matches!(t.as_ref(), WaveType::Byte | WaveType::Char))
                            },
                            _ => false,
                        };
                    if !valid {
                        return Err(format!(
                            "format placeholder `{{{spec}}}` is incompatible with `{}`",
                            display_expression_type(&ty)
                        ));
                    }
                }
                Ok(true)
            },
            StatementNode::Input { args, .. } => {
                self.mark_span(SemanticSpanKind::Keyword, "input");
                for argument in args {
                    if !is_lvalue_expression(argument) {
                        return Err("input argument must be a mutable lvalue".to_string());
                    }
                    self.ensure_mutable_write_target(argument, "write input into")?;
                    let ty = self.validate_expr(argument)?;
                    let supported = match &ty {
                        ExpressionType::Known(ty) => matches!(
                            self.program.canonical_type(ty),
                            WaveType::Bool
                                | WaveType::Int(_)
                                | WaveType::Uint(_)
                                | WaveType::Float(_)
                                | WaveType::Char
                                | WaveType::Byte
                        ),
                        _ => false,
                    };
                    if !supported {
                        return Err(format!(
                            "input argument must be a mutable scalar lvalue, found `{}`",
                            display_expression_type(&ty)
                        ));
                    }
                }
                Ok(true)
            },
            StatementNode::If { condition, body, else_if_blocks, else_block } => {
                self.mark_span(SemanticSpanKind::Keyword, "if");
                self.validate_condition(condition, "if condition")?;
                let mut any_branch_falls_through = self.validate_scoped_block(body)?;

                if let Some(blocks) = else_if_blocks {
                    for (condition, block) in blocks.iter() {
                        self.mark_span(SemanticSpanKind::Keyword, "if");
                        self.validate_condition(condition, "else-if condition")?;
                        any_branch_falls_through |= self.validate_scoped_block(block)?;
                    }
                }

                if let Some(block) = else_block {
                    any_branch_falls_through |= self.validate_scoped_block(block)?;
                } else {
                    any_branch_falls_through = true;
                }

                Ok(any_branch_falls_through)
            },
            StatementNode::While { condition, body } => {
                self.mark_span(SemanticSpanKind::Keyword, "while");
                self.validate_condition(condition, "while condition")?;
                self.loop_depth += 1;
                let body_result = self.validate_scoped_block(body);
                self.loop_depth -= 1;
                body_result?;

                Ok(!expression_is_true(condition) || block_breaks_current_loop(body))
            },
            StatementNode::For { initialization, condition, increment, body } => {
                self.with_scope(|validator| {
                    validator.mark_span(SemanticSpanKind::Keyword, "for");
                    validator.validate_node(initialization)?;
                    validator.validate_condition(condition, "for condition")?;
                    validator.validate_expr(increment)?;
                    validator.loop_depth += 1;
                    let body_result = validator.validate_block(body);
                    validator.loop_depth -= 1;
                    body_result?;

                    Ok(!expression_is_true(condition) || block_breaks_current_loop(body))
                })
            },
            StatementNode::Match { value, arms } => {
                self.mark_span(SemanticSpanKind::Keyword, "match");
                let value_type = self.validate_expr(value)?;
                match &value_type {
                    ExpressionType::Known(ty) => match self.program.canonical_type(ty) {
                        WaveType::Variant(name) => self.validate_variant_match(&name, arms),
                        _ if self.is_integer_expression(&value_type) => {
                            self.validate_integer_match(&value_type, arms)
                        },
                        _ => Err(format!(
                            "match value must be an integer, enum, or variant, found `{}`",
                            display_expression_type(&value_type)
                        )),
                    },
                    _ if self.is_integer_expression(&value_type) => {
                        self.validate_integer_match(&value_type, arms)
                    },
                    _ => Err(format!(
                        "match value must be an integer, enum, or variant, found `{}`",
                        display_expression_type(&value_type)
                    )),
                }
            },
            StatementNode::Break => {
                self.mark_span(SemanticSpanKind::Keyword, "break");
                if self.loop_depth == 0 {
                    return Err("`break` can only be used inside a loop".to_string());
                }
                Ok(false)
            },
            StatementNode::Continue => {
                self.mark_span(SemanticSpanKind::Keyword, "continue");
                if self.loop_depth == 0 {
                    return Err("`continue` can only be used inside a loop".to_string());
                }
                Ok(false)
            },
            StatementNode::Return(value) => {
                self.mark_span(SemanticSpanKind::Keyword, "return");
                self.validate_return(value.as_ref())?;
                Ok(false)
            },
            StatementNode::AsmBlock { inputs, outputs, clobbers, .. } => {
                for (_, expression) in inputs.iter().chain(outputs.iter()) {
                    self.validate_expr(expression)?;
                }
                Ok(!clobbers.iter().any(|clobber| clobber == "noreturn"))
            },
            _ => Ok(true),
        }
    }

    fn validate_integer_match(
        &mut self,
        value_type: &ExpressionType,
        arms: &[crate::ast::MatchArm],
    ) -> Result<bool, String> {
        let ty = canonical_expression_type(self.program, value_type).unwrap_or(WaveType::Int(32));
        let normalize = |raw: &str, ty: &WaveType| -> Result<String, String> {
            if !integer_literal_fits(raw, ty) {
                return Err(format!(
                    "integer match case `{raw}` is out of range for `{}`",
                    display_wave_type(ty)
                ));
            }
            let (bits, signed) = match ty {
                WaveType::Int(bits) => (*bits, true),
                WaveType::Uint(bits) => (*bits, false),
                WaveType::Byte | WaveType::Char => (8, false),
                _ => return Err("integer match requires a concrete integer type".into()),
            };
            lexer::number::IntegerLiteral::parse(raw)
                .and_then(|v| v.canonical_decimal(bits, signed))
                .ok_or_else(|| format!("invalid integer match case `{raw}`"))
        };
        let mut seen = HashSet::new();
        let mut has_wildcard = false;
        let mut all_arms_terminate = !arms.is_empty();

        for arm in arms {
            if let Some(span) = self.source_map.patterns.get(&(&arm.pattern as *const _ as usize)) {
                self.source_span = Some(span.clone());
            }
            let key = match &arm.pattern {
                MatchPattern::Located { .. } => {
                    unreachable!("source wrappers detached before analysis")
                },
                MatchPattern::Int(raw) => {
                    self.mark_span(SemanticSpanKind::Keyword, raw.clone());
                    let value = normalize(raw, &ty)?;
                    self.integer_patterns.insert(&arm.pattern as *const _ as usize, value.clone());
                    format!("value:{}", value)
                },
                MatchPattern::Ident(name) => {
                    self.mark_span(SemanticSpanKind::Identifier, name.clone());
                    let binding = self
                        .lookup_binding(name)
                        .ok_or_else(|| format!("unknown match case constant `{}`", name))?;
                    if !matches!(binding.mutability, Mutability::Const)
                        || !is_integer_type(&self.program.canonical_type(&binding.ty))
                    {
                        return Err(format!(
                            "match case `{}` must name an integer or enum constant",
                            name
                        ));
                    }
                    let value = self.program.constant_values.get(name).ok_or_else(|| {
                        format!("match case `{}` does not have a compile-time integer value", name)
                    })?;
                    let value = normalize(value, &self.program.canonical_type(&binding.ty))?;
                    let value = normalize(&value, &ty)?;
                    self.integer_patterns.insert(&arm.pattern as *const _ as usize, value.clone());
                    format!("value:{}", value)
                },
                MatchPattern::Wildcard => {
                    self.mark_span(SemanticSpanKind::Keyword, "_");
                    has_wildcard = true;
                    "wildcard:_".to_string()
                },
                MatchPattern::Binding(_) | MatchPattern::Variant { .. } => {
                    return Err("variant patterns require a variant match value".to_string())
                },
            };
            if !seen.insert(key.clone()) {
                return Err(format!("duplicate match case pattern `{}`", key));
            }
            all_arms_terminate &= !self.validate_scoped_block(&arm.body)?;
        }

        Ok(!(has_wildcard && all_arms_terminate))
    }

    fn validate_variant_match(
        &mut self,
        variant_name: &str,
        arms: &[crate::ast::MatchArm],
    ) -> Result<bool, String> {
        let definition = self.program.variant_type(variant_name).cloned().unwrap();
        let all_cases =
            definition.cases.iter().map(|(name, _)| name.clone()).collect::<HashSet<_>>();
        let expected_type = WaveType::Variant(variant_name.to_string());
        let mut seen_patterns = HashSet::new();
        let mut covered_patterns: Vec<&MatchPattern> = Vec::new();
        let mut all_arms_terminate = !arms.is_empty();

        for arm in arms {
            if let Some(span) = self.source_map.patterns.get(&(&arm.pattern as *const _ as usize)) {
                self.source_span = Some(span.clone());
            }
            if self.variant_patterns_cover(&covered_patterns, &expected_type) {
                return Err(
                    if covered_patterns
                        .iter()
                        .any(|pattern| matches!(pattern, MatchPattern::Wildcard))
                    {
                        "match pattern after wildcard is unreachable".to_string()
                    } else {
                        "match pattern is unreachable because previous variant patterns are exhaustive"
                        .to_string()
                    },
                );
            }
            let mut bindings = HashMap::new();
            match &arm.pattern {
                MatchPattern::Located { .. } => {
                    unreachable!("source wrappers detached before analysis")
                },
                MatchPattern::Wildcard => {},
                MatchPattern::Variant { variant_type, case_name, .. } => {
                    if self.program.named_type_base(variant_type)
                        != self.program.named_type_base(variant_name)
                    {
                        return Err(format!(
                            "case `{}::{}` does not belong to variant `{}`",
                            variant_type, case_name, variant_name
                        ));
                    }
                    let key = variant_pattern_key(&arm.pattern);
                    if !seen_patterns.insert(key) {
                        return Err(format!(
                            "duplicate variant case `{}::{}` in match",
                            variant_type, case_name
                        ));
                    }
                    self.collect_variant_pattern_bindings(
                        &arm.pattern,
                        &expected_type,
                        &mut bindings,
                    )?;
                },
                MatchPattern::Int(_) | MatchPattern::Ident(_) | MatchPattern::Binding(_) => {
                    return Err(format!(
                        "match on variant `{}` requires a qualified case pattern or `_`",
                        variant_name
                    ))
                },
            }

            let arm_falls_through = self.with_scope(|validator| {
                for (name, ty) in bindings {
                    validator.insert_current_binding(
                        name,
                        Binding { mutability: Mutability::Var, ty },
                        "pattern binding",
                    )?;
                }
                validator.validate_block(&arm.body)
            })?;
            all_arms_terminate &= !arm_falls_through;
            covered_patterns.push(&arm.pattern);
        }

        let exhaustive = self.variant_patterns_cover(&covered_patterns, &expected_type);
        if !exhaustive {
            let mut missing = all_cases
                .iter()
                .filter(|case| {
                    !covered_patterns.iter().any(|pattern| {
                        matches!(
                            pattern,
                            MatchPattern::Variant { case_name, .. } if case_name == *case
                        )
                    })
                })
                .cloned()
                .collect::<Vec<_>>();
            missing.sort();
            if missing.is_empty() {
                return Err(format!(
                    "non-exhaustive match on variant `{}`; patterns do not cover all payload shapes",
                    variant_name
                ));
            }
            return Err(format!(
                "non-exhaustive match on variant `{}`; missing case(s): {}",
                variant_name,
                missing.join(", ")
            ));
        }

        Ok(!(exhaustive && all_arms_terminate))
    }

    fn collect_variant_pattern_bindings(
        &mut self,
        pattern: &MatchPattern,
        expected: &WaveType,
        bindings: &mut HashMap<String, WaveType>,
    ) -> Result<(), String> {
        match pattern {
            MatchPattern::Located { .. } => {
                unreachable!("source wrappers detached before analysis")
            },
            MatchPattern::Binding(name) => {
                if bindings.insert(name.clone(), expected.clone()).is_some() {
                    return Err(format!("duplicate pattern binding `{}`", name));
                }
                Ok(())
            },
            MatchPattern::Wildcard => Ok(()),
            MatchPattern::Variant { variant_type, case_name, payloads } => {
                let WaveType::Variant(expected_name) = self.program.canonical_type(expected) else {
                    return Err(format!(
                        "nested variant pattern `{}::{}` cannot match payload type `{}`",
                        variant_type,
                        case_name,
                        display_wave_type(expected)
                    ));
                };
                if self.program.named_type_base(variant_type)
                    != self.program.named_type_base(&expected_name)
                {
                    return Err(format!(
                        "case `{}::{}` does not belong to payload variant `{}`",
                        variant_type, case_name, expected_name
                    ));
                }
                let (discriminant, payload_types) =
                    self.program.variant_case(&expected_name, case_name).ok_or_else(|| {
                        format!("unknown case `{}` in variant `{}`", case_name, expected_name)
                    })?;
                if payloads.len() != payload_types.len() {
                    return Err(format!(
                        "variant pattern `{}::{}` expects {} payload pattern(s), found {}",
                        variant_type,
                        case_name,
                        payload_types.len(),
                        payloads.len()
                    ));
                }
                self.variant_patterns.insert(
                    pattern as *const MatchPattern as usize,
                    VariantPattern {
                        variant_type: WaveType::Variant(expected_name.clone()),
                        case_name: case_name.clone(),
                        discriminant,
                        payload_types: payload_types.clone(),
                    },
                );
                for (payload, ty) in payloads.iter().zip(&payload_types) {
                    self.collect_variant_pattern_bindings(payload, ty, bindings)?;
                }
                Ok(())
            },
            MatchPattern::Int(_) | MatchPattern::Ident(_) => {
                Err("variant payload patterns support bindings, `_`, or nested cases".to_string())
            },
        }
    }

    fn variant_pattern_is_irrefutable(&self, pattern: &MatchPattern, expected: &WaveType) -> bool {
        match pattern {
            MatchPattern::Located { .. } => {
                unreachable!("source wrappers detached before analysis")
            },
            MatchPattern::Binding(_) | MatchPattern::Wildcard => true,
            MatchPattern::Variant { variant_type, case_name, payloads } => {
                let WaveType::Variant(expected_name) = self.program.canonical_type(expected) else {
                    return false;
                };
                if self.program.named_type_base(variant_type)
                    != self.program.named_type_base(&expected_name)
                {
                    return false;
                }
                let Some(definition) = self.program.variant_type(&expected_name) else {
                    return false;
                };
                if definition.cases.len() != 1 || definition.cases[0].0 != *case_name {
                    return false;
                }
                let Some((_, payload_types)) = self.program.variant_case(&expected_name, case_name)
                else {
                    return false;
                };
                payloads.len() == payload_types.len()
                    && payloads
                        .iter()
                        .zip(&payload_types)
                        .all(|(payload, ty)| self.variant_pattern_is_irrefutable(payload, ty))
            },
            MatchPattern::Int(_) | MatchPattern::Ident(_) => false,
        }
    }

    fn variant_patterns_cover(&self, patterns: &[&MatchPattern], expected: &WaveType) -> bool {
        if patterns
            .iter()
            .any(|pattern| matches!(pattern, MatchPattern::Binding(_) | MatchPattern::Wildcard))
        {
            return true;
        }
        let WaveType::Variant(expected_name) = self.program.canonical_type(expected) else {
            return false;
        };
        let Some(definition) = self.program.variant_type(&expected_name) else {
            return false;
        };
        definition.cases.iter().all(|(case_name, _)| {
            let case_patterns = patterns
                .iter()
                .filter_map(|pattern| match pattern {
                    MatchPattern::Variant { variant_type, case_name: pattern_case, payloads }
                        if self.program.named_type_base(variant_type)
                            == self.program.named_type_base(&expected_name)
                            && pattern_case == case_name =>
                    {
                        Some(payloads)
                    },
                    _ => None,
                })
                .collect::<Vec<_>>();
            if case_patterns.is_empty() {
                return false;
            }
            let Some((_, payload_types)) = self.program.variant_case(&expected_name, case_name)
            else {
                return false;
            };
            if payload_types.is_empty() {
                return true;
            }
            if payload_types.len() == 1 {
                let nested = case_patterns
                    .iter()
                    .filter_map(|payloads| payloads.first())
                    .collect::<Vec<_>>();
                return self.variant_patterns_cover(&nested, &payload_types[0]);
            }
            case_patterns.iter().any(|payloads| {
                payloads.len() == payload_types.len()
                    && payloads
                        .iter()
                        .zip(&payload_types)
                        .all(|(pattern, ty)| self.variant_pattern_is_irrefutable(pattern, ty))
            })
        })
    }

    fn validate_return(&mut self, value: Option<&Expression>) -> Result<(), String> {
        let function =
            self.current_function.clone().unwrap_or_else(|| "<unknown function>".to_string());
        let expected = self.current_return_type.clone().unwrap_or(WaveType::Void);

        match (expected, value) {
            (WaveType::Never, _) => Err(format!(
                "never-returning function `{function}` cannot contain a return statement"
            )),
            (WaveType::Void, None) => Ok(()),
            (WaveType::Void, Some(expression)) => {
                if matches!(self.validate_expr(expression)?, ExpressionType::Known(WaveType::Void))
                {
                    Ok(())
                } else {
                    Err(format!("void function `{function}` cannot return a value"))
                }
            },
            (expected, None) => Err(format!(
                "non-void function `{}` must return `{}`",
                function,
                display_wave_type(&expected)
            )),
            (expected, Some(expression)) => {
                let actual = self.validate_expr_expected(expression, Some(&expected))?;
                self.require_assignable(
                    &actual,
                    &expected,
                    &format!("return value of function `{}`", function),
                )
            },
        }
    }

    fn validate_condition(&mut self, expression: &Expression, context: &str) -> Result<(), String> {
        if let Some(mutation) = condition_mutation(expression) {
            self.diagnostic_help = Some(mutation.help().to_string());
            return Err(format!(
                "{} `{}` is not allowed in {}",
                mutation.description(),
                mutation.symbol(),
                context
            ));
        }

        let ty = self.validate_expr(expression)?;
        if self.is_truthy_expression(&ty) {
            return Ok(());
        }

        Err(format!(
            "{} must be bool, numeric, pointer, or string, found `{}`",
            context,
            display_expression_type(&ty)
        ))
    }

    fn validate_format_argument(&self, ty: &ExpressionType) -> Result<(), String> {
        let supported = match ty {
            ExpressionType::IntLiteral(_)
            | ExpressionType::FloatLiteral
            | ExpressionType::Null
            | ExpressionType::Unknown => true,
            ExpressionType::Known(ty) => {
                self.program.is_generic_placeholder(ty)
                    || matches!(
                        self.program.canonical_type(ty),
                        WaveType::Bool
                            | WaveType::Int(_)
                            | WaveType::Uint(_)
                            | WaveType::Float(_)
                            | WaveType::Char
                            | WaveType::Byte
                            | WaveType::String
                            | WaveType::Pointer(_)
                    )
            },
            ExpressionType::ArrayLiteral(_) | ExpressionType::AddressedArrayLiteral(_) => false,
        };
        if supported {
            Ok(())
        } else {
            Err(format!(
                "format argument must be a scalar, string, pointer, or null, found `{}`",
                display_expression_type(ty)
            ))
        }
    }

    fn is_truthy_expression(&self, ty: &ExpressionType) -> bool {
        match ty {
            ExpressionType::IntLiteral(_) | ExpressionType::FloatLiteral => true,
            ExpressionType::Known(ty) => matches!(
                self.program.canonical_type(ty),
                WaveType::Bool
                    | WaveType::Int(_)
                    | WaveType::Uint(_)
                    | WaveType::Float(_)
                    | WaveType::Char
                    | WaveType::Byte
                    | WaveType::String
                    | WaveType::Pointer(_)
            ),
            ExpressionType::Null
            | ExpressionType::ArrayLiteral(_)
            | ExpressionType::AddressedArrayLiteral(_)
            | ExpressionType::Unknown => false,
        }
    }

    fn is_integer_expression(&self, ty: &ExpressionType) -> bool {
        match ty {
            ExpressionType::IntLiteral(_) => true,
            ExpressionType::Known(ty) => is_integer_type(&self.program.canonical_type(ty)),
            _ => false,
        }
    }

    fn validate_expr(&mut self, expression: &Expression) -> Result<ExpressionType, String> {
        self.validate_expr_expected(expression, None)
    }

    fn validate_expr_expected(
        &mut self,
        expression: &Expression,
        expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        if let Some(span) = self.source_map.expressions.get(&(expression as *const _ as usize)) {
            self.source_span = Some(span.clone());
        }

        let result = self.validate_expr_inner(expression, expected);
        if let Ok(expression_type) = &result {
            if let Some(expected) = expected {
                self.expected_types.insert(
                    expression as *const Expression as usize,
                    self.program.canonical_type(expected),
                );
            }
            self.analyzed_expression_types.insert(
                expression as *const Expression as usize,
                analyzed_expression_type(self.program, expression_type),
            );
            if let Some(ty) = canonical_expression_type(self.program, expression_type) {
                self.expression_types.insert(expression as *const Expression as usize, ty);
            }
        }
        result
    }

    fn validate_integer_operand(
        &mut self,
        expression: &Expression,
        ty: &WaveType,
    ) -> Result<ExpressionType, String> {
        let actual = self.validate_expr_expected(expression, Some(ty))?;
        validate_contextual_integer_literal(
            self.program,
            &actual,
            &ExpressionType::Known(ty.clone()),
        )?;
        // Do not retain one operand's literal text as the type of an entire
        // arithmetic expression. Record the selected width and signedness.
        Ok(ExpressionType::Known(ty.clone()))
    }

    fn validate_contextual_operand(
        &mut self,
        expression: &Expression,
        other: &ExpressionType,
    ) -> Result<ExpressionType, String> {
        if let Some(ty) = canonical_expression_type(self.program, other) {
            if matches!(ty, WaveType::Int(_) | WaveType::Uint(_)) {
                return self.validate_integer_operand(expression, &ty);
            }
        }
        self.validate_expr(expression)
    }

    fn validate_function_call(
        &mut self,
        expression: &Expression,
        name: &str,
        type_args: &[WaveType],
        args: &[Expression],
    ) -> Result<ExpressionType, String> {
        if crate::layout_intrinsics::is_intrinsic(name)
            || crate::async_intrinsics::is_intrinsic(name)
        {
            return self.validate_intrinsic_call(name, type_args, args);
        }
        let signature = self.resolve_function_call(expression, name, type_args, args)?;
        self.validate_call_arguments(
            "function",
            name,
            args,
            &signature.params,
            signature.required_params,
            signature.variadic,
        )?;
        Ok(ExpressionType::Known(signature.return_type))
    }

    fn validate_intrinsic_call(
        &mut self,
        name: &str,
        type_args: &[WaveType],
        args: &[Expression],
    ) -> Result<ExpressionType, String> {
        if crate::layout_intrinsics::is_intrinsic(name) {
            if type_args.len() != 1 || !args.is_empty() {
                return Err(format!("{name} requires one type argument and no value arguments"));
            }
            self.program.validate_type(
                &type_args[0],
                &self.current_type_params,
                false,
                "layout query type",
            )?;
            return Ok(ExpressionType::Known(WaveType::Uint(64)));
        }
        if crate::async_intrinsics::is_intrinsic(name) {
            for (index, ty) in type_args.iter().enumerate() {
                self.program.validate_type(
                    ty,
                    &self.current_type_params,
                    name == "__wave_async_create" && index == 1,
                    "async intrinsic type argument",
                )?;
            }
            if name == "__wave_async_create" {
                let Some(Expression::Literal(Literal::String(symbol))) =
                    args.get(2).map(Expression::unspanned)
                else {
                    return Err("async frame creation requires a generated resume symbol".into());
                };
                let symbol = std::str::from_utf8(symbol)
                    .map_err(|_| "invalid generated resume symbol".to_string())?;
                if !symbol.starts_with("$async$poll$")
                    || !self.program.functions.contains_key(symbol)
                {
                    return Err("async frame creation requires a generated resume function".into());
                }
            }
            let actual =
                args.iter().map(|a| self.validate_expr(a)).collect::<Result<Vec<_>, _>>()?;
            let concrete = actual
                .iter()
                .map(|a| match a {
                    ExpressionType::Known(t) => t.clone(),
                    ExpressionType::IntLiteral(_) => WaveType::Int(64),
                    _ => WaveType::Void,
                })
                .collect::<Vec<_>>();
            let (params, result) = crate::async_intrinsics::signature(name, type_args, &concrete)?;
            if actual.len() != params.len() {
                return Err(format!(
                    "{name} expects {} arguments, found {}",
                    params.len(),
                    actual.len()
                ));
            }
            for (actual, expected) in actual.iter().zip(params.iter()) {
                self.require_assignable(actual, expected, name)?;
            }
            return Ok(ExpressionType::Known(result));
        }
        unreachable!("intrinsic name was checked")
    }

    fn resolve_function_call(
        &mut self,
        expression: &Expression,
        name: &str,
        type_args: &[WaveType],
        args: &[Expression],
    ) -> Result<FunctionType, String> {
        let mut inferred = Vec::new();
        if let Some(signature) = self.program.functions.get(name).cloned() {
            if type_args.is_empty()
                && !signature.generic_params.is_empty()
                && signature.params.iter().any(|t| matches!(t, WaveType::Future(_)))
            {
                let mut subst = HashMap::new();
                for (template, arg) in signature.params.iter().zip(args) {
                    let actual = self.validate_expr(arg)?;
                    infer_variant_substitution(
                        self.program,
                        template,
                        &actual,
                        &signature.generic_params,
                        &mut subst,
                    )?;
                }
                inferred = signature
                    .generic_params
                    .iter()
                    .map(|p| {
                        subst.get(p).cloned().ok_or_else(|| {
                            format!("cannot infer generic parameter `{p}` in `{name}`")
                        })
                    })
                    .collect::<Result<_, _>>()?;
                self.generic_method_calls.insert(
                    expression as *const _ as usize,
                    crate::methods::GenericMethodCall {
                        function: name.into(),
                        type_args: inferred.clone(),
                        defaults: Vec::new(),
                        rewrite: true,
                    },
                );
            }
        }
        let type_args = if inferred.is_empty() { type_args } else { &inferred };
        let signature = self.program.functions.get(name).map(|signature| {
            let substitutions: HashMap<String, WaveType> =
                signature.generic_params.iter().cloned().zip(type_args.iter().cloned()).collect();
            substitute_function_type(signature, &substitutions)
        });
        let Some(signature) = signature else {
            for argument in args {
                self.validate_expr(argument)?;
            }
            return Err(format!("call to unknown function `{}`", name));
        };

        let declared_generic_count =
            self.program.functions.get(name).map_or(0, |function| function.generic_params.len());
        if type_args.len() != declared_generic_count {
            return Err(format!(
                "function `{}` expects {} generic argument(s), found {}",
                name,
                declared_generic_count,
                type_args.len()
            ));
        }

        Ok(signature)
    }

    fn validate_variant_constructor(
        &mut self,
        expression: &Expression,
        name: &str,
        args: &[Expression],
        expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        let (owner, case_name) = self
            .program
            .variant_constructor(name)
            .ok_or_else(|| format!("unknown variant constructor `{}`", name))?;
        let definition = self.program.variant_type(owner).cloned().unwrap();
        let (_, (_, template_payloads)) = definition
            .cases
            .iter()
            .enumerate()
            .find(|(_, (case, _))| case == case_name)
            .ok_or_else(|| format!("unknown case `{}` in variant `{}`", case_name, owner))?;

        if args.len() != template_payloads.len() {
            return Err(format!(
                "variant constructor `{}` expects {} payload value(s), found {}",
                name,
                template_payloads.len(),
                args.len()
            ));
        }

        let expected_variant =
            expected.map(|ty| self.program.canonical_type(ty)).and_then(|ty| match ty {
                WaveType::Variant(name)
                    if self.program.named_type_base(&name)
                        == self.program.named_type_base(owner) =>
                {
                    Some(name)
                },
                _ => None,
            });

        let concrete_name = if let Some(name) = expected_variant {
            name
        } else if definition.generic_params.is_empty() {
            owner.to_string()
        } else {
            let mut substitutions = HashMap::new();
            for (argument, template) in args.iter().zip(template_payloads) {
                let actual = self.validate_expr(argument)?;
                infer_variant_substitution(
                    self.program,
                    template,
                    &actual,
                    &definition.generic_params,
                    &mut substitutions,
                )?;
            }
            let unresolved = definition
                .generic_params
                .iter()
                .filter(|param| !substitutions.contains_key(*param))
                .cloned()
                .collect::<Vec<_>>();
            if !unresolved.is_empty() {
                return Err(format!(
                    "cannot resolve generic argument(s) {} for variant constructor `{}`; add an explicit destination type",
                    unresolved.join(", "),
                    name
                ));
            }
            let arguments = definition
                .generic_params
                .iter()
                .map(|param| display_wave_type(&substitutions[param]))
                .collect::<Vec<_>>()
                .join(",");
            format!("{}<{}>", owner, arguments)
        };

        let (discriminant, payload_types) = self
            .program
            .variant_case(&concrete_name, case_name)
            .expect("validated variant case remains available");
        for (index, (argument, payload_type)) in args.iter().zip(&payload_types).enumerate() {
            let actual = self.validate_expr_expected(argument, Some(payload_type))?;
            self.require_assignable(
                &actual,
                payload_type,
                &format!("payload {} of variant constructor `{}`", index + 1, name),
            )?;
        }

        let variant_type = WaveType::Variant(concrete_name);
        self.variant_constructions.insert(
            expression as *const Expression as usize,
            VariantConstruction {
                variant_type: variant_type.clone(),
                case_name: case_name.to_string(),
                discriminant,
                payload_types,
            },
        );
        Ok(ExpressionType::Known(variant_type))
    }

    fn validate_method_call(
        &mut self,
        expression: &Expression,
        object: &Expression,
        name: &str,
        type_args: &[WaveType],
        args: &[Expression],
    ) -> Result<ExpressionType, String> {
        let object_type = self.validate_expr(object)?;
        let object_type = match object_type {
            ExpressionType::Known(ty) => ExpressionType::Known(self.program.canonical_type(&ty)),
            other => other,
        };
        self.source_span =
            self.source_map.expressions.get(&(expression as *const _ as usize)).cloned();
        self.mark_span(SemanticSpanKind::Identifier, name);
        let structure = match canonical_expression_type(self.program, &object_type) {
            Some(WaveType::Struct(name)) => Some(name),
            Some(WaveType::Pointer(inner)) => match *inner {
                WaveType::Struct(name) => Some(name),
                _ => None,
            },
            _ => None,
        };
        let method = structure.as_ref().and_then(|owner| self.program.method_type(owner, name));
        let inherent = method.is_some();
        let Some(mut signature) = method.or_else(|| self.program.functions.get(name).cloned())
        else {
            return Err(match structure {
                Some(owner) => format!("struct `{owner}` has no method `{name}`"),
                None => format!(
                    "method call `{name}` requires a struct receiver, found `{}`",
                    display_expression_type(&object_type)
                ),
            });
        };
        let generic_args = if type_args.is_empty() && !signature.generic_params.is_empty() {
            let mut substitutions = HashMap::new();
            if let Some(receiver) = signature.params.first() {
                infer_variant_substitution(
                    self.program,
                    receiver,
                    &object_type,
                    &signature.generic_params,
                    &mut substitutions,
                )?;
            }
            for (parameter, argument) in signature.params.iter().skip(1).zip(args) {
                let actual = self.validate_expr(argument)?;
                infer_variant_substitution(
                    self.program,
                    parameter,
                    &actual,
                    &signature.generic_params,
                    &mut substitutions,
                )?;
            }
            signature.generic_params.iter().map(|parameter| substitutions.get(parameter).cloned()
                .ok_or_else(|| format!("cannot infer generic parameter `{parameter}` for method `{name}`; provide explicit type arguments")))
                .collect::<Result<Vec<_>, _>>()?
        } else {
            type_args.to_vec()
        };
        if generic_args.len() != signature.generic_params.len() {
            return Err(format!(
                "method `{name}` expects {} generic argument(s), found {}",
                signature.generic_params.len(),
                generic_args.len()
            ));
        }
        for ty in &generic_args {
            self.program.validate_type(
                ty,
                &self.current_type_params,
                false,
                "method type argument",
            )?;
        }
        let mut lowered = None;
        if !signature.generic_params.is_empty() {
            let substitutions = signature
                .generic_params
                .iter()
                .cloned()
                .zip(generic_args.iter().cloned())
                .collect();
            signature = substitute_function_type(&signature, &substitutions);
            let (function, mut all_args) = if inherent {
                let owner = structure.as_ref().unwrap();
                let base = self.program.named_type_base(owner);
                let owner_args =
                    parse_named_type_application(owner).map(|(_, args)| args).unwrap_or_default();
                (crate::methods::method_symbol(base, name), owner_args)
            } else {
                (name.to_string(), Vec::new())
            };
            all_args.extend(generic_args);
            lowered = Some((function, all_args));
        }
        let Some(receiver) = signature.params.first() else {
            return Err(format!("method `{name}` requires a receiver parameter"));
        };
        self.require_assignable(&object_type, receiver, &format!("receiver of method `{name}`"))?;
        self.validate_call_arguments(
            "method",
            name,
            args,
            &signature.params[1..],
            signature.required_params.saturating_sub(1),
            false,
        )?;
        let rewrite = lowered.is_some();
        let (function, type_args) = lowered.unwrap_or_else(|| (name.to_string(), Vec::new()));
        let defaults = signature
            .defaults
            .iter()
            .skip(args.len() + 1)
            .flatten()
            .map(|value| {
                value.clone().with_span(
                    self.source_span
                        .clone()
                        .map(|s| s.generated(format!("default argument for {name}"))),
                )
            })
            .collect();
        self.generic_method_calls.insert(
            expression as *const _ as usize,
            crate::methods::GenericMethodCall { function, type_args, defaults, rewrite },
        );
        Ok(ExpressionType::Known(signature.return_type))
    }

    fn validate_unary(
        &self,
        operator: &Operator,
        ty: ExpressionType,
    ) -> Result<ExpressionType, String> {
        if matches!(&ty, ExpressionType::Known(inner) if self.program.is_generic_placeholder(inner))
        {
            return if matches!(operator, Operator::Not | Operator::LogicalNot) {
                Ok(ExpressionType::Known(WaveType::Bool))
            } else {
                Ok(ty)
            };
        }
        let canonical = match &ty {
            ExpressionType::Known(ty) => Some(self.program.canonical_type(ty)),
            ExpressionType::IntLiteral(_) => Some(WaveType::Int(32)),
            ExpressionType::FloatLiteral => Some(WaveType::Float(32)),
            _ => None,
        };

        let supported = match operator {
            Operator::Neg => canonical.as_ref().is_some_and(is_numeric_type),
            Operator::Not | Operator::LogicalNot => canonical.as_ref().is_some_and(|ty| {
                matches!(
                    ty,
                    WaveType::Bool
                        | WaveType::Int(_)
                        | WaveType::Uint(_)
                        | WaveType::Char
                        | WaveType::Byte
                )
            }),
            Operator::BitwiseNot => canonical.as_ref().is_some_and(is_integer_type),
            _ => false,
        };

        if !supported {
            return Err(format!(
                "unary operator `{:?}` is not supported for `{}`",
                operator,
                display_expression_type(&ty)
            ));
        }

        if matches!(operator, Operator::Not | Operator::LogicalNot) {
            Ok(ExpressionType::Known(WaveType::Bool))
        } else if matches!(operator, Operator::Neg) {
            match ty {
                ExpressionType::IntLiteral(raw) => {
                    let value = parse_integer_value(&raw)
                        .and_then(i128::checked_neg)
                        .ok_or_else(|| format!("integer literal `{}` overflows", raw))?;
                    Ok(ExpressionType::IntLiteral(value.to_string()))
                },
                other => Ok(other),
            }
        } else {
            Ok(ty)
        }
    }

    fn validate_call_arguments(
        &mut self,
        kind: &str,
        name: &str,
        args: &[Expression],
        params: &[WaveType],
        required_params: usize,
        variadic: bool,
    ) -> Result<(), String> {
        if args.len() < required_params || (!variadic && args.len() > params.len()) {
            let expectation = if required_params == params.len() {
                params.len().to_string()
            } else {
                format!("between {} and {}", required_params, params.len())
            };
            return Err(format!(
                "{} `{}` expects {} argument(s), found {}",
                kind,
                name,
                expectation,
                args.len()
            ));
        }

        for (index, (argument, expected)) in args.iter().zip(params).enumerate() {
            let actual = self.validate_expr_expected(argument, Some(expected))?;
            self.require_assignable(
                &actual,
                expected,
                &format!("argument {} of {} `{}`", index + 1, kind, name),
            )?;
        }

        if variadic {
            for (index, argument) in args.iter().enumerate().skip(params.len()) {
                let actual = self.validate_expr(argument)?;
                let actual = canonical_expression_type(self.program, &actual).ok_or_else(|| {
                    format!(
                        "variadic argument {} of function `{}` has no scalar type",
                        index + 1,
                        name
                    )
                })?;
                if !matches!(
                    actual,
                    WaveType::Int(_)
                        | WaveType::Uint(_)
                        | WaveType::Float(_)
                        | WaveType::Bool
                        | WaveType::Char
                        | WaveType::Byte
                        | WaveType::String
                        | WaveType::Pointer(_)
                ) {
                    return Err(format!(
                        "variadic argument {} of function `{}` must be a scalar value",
                        index + 1,
                        name
                    ));
                }
            }
        }
        Ok(())
    }

    fn require_assignable(
        &self,
        actual: &ExpressionType,
        expected: &WaveType,
        context: &str,
    ) -> Result<(), String> {
        require_assignable(self.program, actual, expected, context)
    }

    fn is_valid_cast(&self, source: &ExpressionType, target: &WaveType) -> bool {
        is_valid_cast(self.program, source, target)
    }

    fn ensure_mutable_write_target(
        &self,
        target: &Expression,
        operation: &str,
    ) -> Result<(), String> {
        let Some((base, saw_deref)) = find_base_var(target, false) else {
            return Ok(());
        };
        if saw_deref {
            return Ok(());
        }

        if let Some(binding) = self.lookup_binding(&base) {
            self.ensure_mutable_binding(&base, &binding, operation)?;
        }
        Ok(())
    }

    fn ensure_mutable_binding(
        &self,
        name: &str,
        binding: &Binding,
        operation: &str,
    ) -> Result<(), String> {
        if matches!(binding.mutability, Mutability::Const) {
            return Err(format!(
                "cannot {} immutable binding `{}` ({:?})",
                operation, name, binding.mutability
            ));
        }
        Ok(())
    }
}

fn operator_source_symbol(operator: &Operator) -> Option<&'static str> {
    match operator {
        Operator::Add => Some("+"),
        Operator::Subtract | Operator::Neg => Some("-"),
        Operator::Multiply => Some("*"),
        Operator::Divide => Some("/"),
        Operator::Remainder => Some("%"),
        Operator::GreaterEqual => Some(">="),
        Operator::LessEqual => Some("<="),
        Operator::Greater => Some(">"),
        Operator::Less => Some("<"),
        Operator::Equal => Some("=="),
        Operator::NotEqual => Some("!="),
        Operator::LogicalAnd => Some("&&"),
        Operator::BitwiseAnd => Some("&"),
        Operator::LogicalOr => Some("||"),
        Operator::BitwiseOr => Some("|"),
        Operator::ShiftLeft => Some("<<"),
        Operator::ShiftRight => Some(">>"),
        Operator::BitwiseXor => Some("^"),
        Operator::LogicalNot | Operator::Not => Some("!"),
        Operator::BitwiseNot => Some("~"),
        Operator::Assign => Some("="),
    }
}

fn assign_operator_source_symbol(operator: &AssignOperator) -> &'static str {
    match operator {
        AssignOperator::Assign => "=",
        AssignOperator::AddAssign => "+=",
        AssignOperator::SubAssign => "-=",
        AssignOperator::MulAssign => "*=",
        AssignOperator::DivAssign => "/=",
        AssignOperator::RemAssign => "%=",
    }
}

fn find_base_var(target: &Expression, saw_deref: bool) -> Option<(String, bool)> {
    match target {
        Expression::Variable(name) => Some((name.clone(), saw_deref)),
        Expression::Grouped(inner) => find_base_var(inner, saw_deref),
        Expression::FieldAccess { object, .. } => find_base_var(object, saw_deref),
        Expression::IndexAccess { target, .. } => find_base_var(target, saw_deref),
        Expression::Deref(inner) => find_base_var(inner, true),
        _ => None,
    }
}

fn is_lvalue_expression(expression: &Expression) -> bool {
    match expression {
        Expression::Located { .. } => unreachable!("source wrappers detached before analysis"),
        Expression::Variable(_)
        | Expression::FieldAccess { .. }
        | Expression::IndexAccess { .. }
        | Expression::Deref(_) => true,
        Expression::Grouped(inner) => is_lvalue_expression(inner),
        _ => false,
    }
}

#[derive(Clone, Copy)]
enum ConditionMutation {
    Assignment(&'static str),
    CompoundAssignment(&'static str),
    IncrementOrDecrement(&'static str),
}

impl ConditionMutation {
    fn symbol(self) -> &'static str {
        match self {
            Self::Assignment(symbol)
            | Self::CompoundAssignment(symbol)
            | Self::IncrementOrDecrement(symbol) => symbol,
        }
    }

    fn description(self) -> &'static str {
        match self {
            Self::Assignment(_) => "assignment",
            Self::CompoundAssignment(_) => "compound assignment",
            Self::IncrementOrDecrement(_) => "increment or decrement",
        }
    }

    fn help(self) -> &'static str {
        match self {
            Self::Assignment(_) => {
                "use `==` for comparison, or move the assignment before the condition"
            },
            Self::CompoundAssignment(_) | Self::IncrementOrDecrement(_) => {
                "move the mutation before the condition"
            },
        }
    }
}

fn condition_mutation(expression: &Expression) -> Option<ConditionMutation> {
    match expression {
        Expression::Located { value, .. } => condition_mutation(value),
        Expression::Assignment { .. } => Some(ConditionMutation::Assignment("=")),
        Expression::AssignOperation { operator, .. } => {
            let symbol = assign_operator_source_symbol(operator);
            if matches!(operator, AssignOperator::Assign) {
                Some(ConditionMutation::Assignment(symbol))
            } else {
                Some(ConditionMutation::CompoundAssignment(symbol))
            }
        },
        Expression::IncDec { kind, .. } => {
            Some(ConditionMutation::IncrementOrDecrement(match kind {
                IncDecKind::PreInc | IncDecKind::PostInc => "++",
                IncDecKind::PreDec | IncDecKind::PostDec => "--",
            }))
        },
        Expression::StructLiteral { fields, .. } => {
            fields.iter().find_map(|(_, value)| condition_mutation(value))
        },
        Expression::FunctionCall { args, .. } => args.iter().find_map(condition_mutation),
        Expression::MethodCall { object, args, .. } => {
            condition_mutation(object).or_else(|| args.iter().find_map(condition_mutation))
        },
        Expression::Deref(inner)
        | Expression::AddressOf(inner)
        | Expression::Await(inner)
        | Expression::Grouped(inner)
        | Expression::Unary { expr: inner, .. }
        | Expression::Cast { expr: inner, .. }
        | Expression::FieldAccess { object: inner, .. } => condition_mutation(inner),
        Expression::BinaryExpression { left, right, .. }
        | Expression::IndexAccess { target: left, index: right } => {
            condition_mutation(left).or_else(|| condition_mutation(right))
        },
        Expression::ArrayLiteral(values) => values.iter().find_map(condition_mutation),
        Expression::AsmBlock { inputs, outputs, .. } => {
            inputs.iter().chain(outputs.iter()).find_map(|(_, value)| condition_mutation(value))
        },
        Expression::Null | Expression::Literal(_) | Expression::Variable(_) => None,
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
        StatementNode::If { body, else_if_blocks, else_block, .. } => {
            block_breaks_current_loop(body)
                || else_if_blocks.as_ref().is_some_and(|blocks| {
                    blocks.iter().any(|(_, block)| block_breaks_current_loop(block))
                })
                || else_block.as_ref().is_some_and(|block| block_breaks_current_loop(block))
        },
        StatementNode::Match { arms, .. } => {
            arms.iter().any(|arm| block_breaks_current_loop(&arm.body))
        },
        StatementNode::While { .. } | StatementNode::For { .. } => false,
        _ => false,
    }
}

fn variant_pattern_key(pattern: &MatchPattern) -> String {
    match pattern {
        MatchPattern::Located { .. } => unreachable!("source wrappers detached before analysis"),
        MatchPattern::Int(raw) => format!("int:{}", raw),
        MatchPattern::Ident(name) => format!("ident:{}", name),
        MatchPattern::Binding(_) | MatchPattern::Wildcard => "*".to_string(),
        MatchPattern::Variant { variant_type, case_name, payloads } => format!(
            "{}::{}({})",
            variant_type,
            case_name,
            payloads.iter().map(variant_pattern_key).collect::<Vec<_>>().join(",")
        ),
    }
}

pub fn validate_program(nodes: &Vec<ASTNode>) -> Result<(), String> {
    validate_program_detailed(nodes).map_err(|diagnostic| diagnostic.message)
}

pub fn validate_program_detailed(nodes: &[ASTNode]) -> Result<(), SemanticDiagnostic> {
    crate::expression_depth::validate(nodes)?;
    let mut syntax = nodes.to_vec().into_boxed_slice();
    let sources = crate::source::SourceMap::detach(&mut syntax);
    analyze_program_types(&syntax, &sources).map(|_| ())
}

pub fn analyze_expression_types(
    nodes: &[ASTNode],
) -> Result<HashMap<usize, WaveType>, SemanticDiagnostic> {
    analyze_program_types(nodes, &crate::source::SourceMap::default())
        .map(|analysis| analysis.expression_types)
}

/// Analyze a final AST without changing its allocations. The source map must
/// refer to these same nodes; returned facts are valid only for this allocation.
pub fn analyze_semantic_facts(
    nodes: &[ASTNode],
    sources: &crate::source::SourceMap,
) -> Result<SemanticFacts, SemanticDiagnostic> {
    analyze_program_types(nodes, sources).map(|analysis| SemanticFacts {
        expression_types: analysis.analyzed_expression_types,
        variant_constructions: analysis.variant_constructions,
        variant_patterns: analysis.variant_patterns,
        expected_types: analysis.expected_types,
        integer_patterns: analysis.integer_patterns,
    })
}

pub(crate) fn analyze_generic_method_calls(
    nodes: &[ASTNode],
    sources: &crate::source::SourceMap,
) -> Result<HashMap<usize, crate::methods::GenericMethodCall>, SemanticDiagnostic> {
    analyze_program_types(nodes, sources).map(|analysis| analysis.generic_method_calls)
}

struct ProgramAnalysis {
    expression_types: HashMap<usize, WaveType>,
    analyzed_expression_types: HashMap<usize, AnalyzedExpressionType>,
    expected_types: HashMap<usize, WaveType>,
    variant_constructions: HashMap<usize, VariantConstruction>,
    variant_patterns: HashMap<usize, VariantPattern>,
    integer_patterns: HashMap<usize, String>,
    generic_method_calls: HashMap<usize, crate::methods::GenericMethodCall>,
}

fn is_supported_foreign_abi(abi: &str) -> bool {
    abi.eq_ignore_ascii_case("c") || abi.eq_ignore_ascii_case("system")
}

fn analyze_program_types(
    nodes: &[ASTNode],
    sources: &crate::source::SourceMap,
) -> Result<ProgramAnalysis, SemanticDiagnostic> {
    crate::expression_depth::validate(nodes)?;
    let program = ProgramTypes::collect(nodes).map_err(|(index, message, primary)| {
        let mut diagnostic = semantic_diagnostic_for_top_level(nodes, index, message, primary);
        diagnostic.span = nodes
            .get(index)
            .and_then(|node| sources.nodes.get(&(node as *const _ as usize)))
            .cloned();
        diagnostic
    })?;
    validate_declaration_types(nodes, &program).map_err(|mut diagnostic| {
        diagnostic.span = nodes
            .get(diagnostic.top_level_index)
            .and_then(|node| sources.nodes.get(&(node as *const _ as usize)))
            .cloned();
        diagnostic
    })?;
    const_dependencies::validate(nodes, sources)?;
    let mut validator = Validator::new(&program, sources);

    for (index, node) in nodes.iter().enumerate() {
        validator.begin_top_level(index, top_level_span_hint(node));
        validator.source_span = sources.nodes.get(&(node as *const _ as usize)).cloned();
        let result = match node {
            ASTNode::Function(function) => {
                if let Some(export) = &function.export {
                    if !is_supported_foreign_abi(&export.abi) {
                        validator.mark_span(SemanticSpanKind::Keyword, "export");
                        Err(format!(
                            "unsupported export ABI '{}' for function '{}': supported ABIs are export(c) and export(system)",
                            export.abi, function.name
                        ))
                    } else {
                        validator.validate_function(function, &function.name, &[])
                    }
                } else {
                    validator.validate_function(function, &function.name, &[])
                }
            },
            ASTNode::ProtoImpl(implementation) => {
                let mut result = Ok(());
                for method in &implementation.methods {
                    validator.mark_span(SemanticSpanKind::Declaration, method.name.clone());
                    result = validator.validate_function(
                        method,
                        &format!("{}.{}", implementation.target, method.name),
                        &[],
                    );
                    if result.is_err() {
                        break;
                    }
                }
                result
            },
            ASTNode::Struct(structure) => {
                let mut result = Ok(());
                for method in &structure.methods {
                    validator.mark_span(SemanticSpanKind::Declaration, method.name.clone());
                    result = validator.validate_function(
                        method,
                        &format!("{}.{}", structure.name, method.name),
                        &structure.generic_params,
                    );
                    if result.is_err() {
                        break;
                    }
                }
                result
            },
            ASTNode::ExternFunction(function) => {
                if !is_supported_foreign_abi(&function.abi) {
                    validator.mark_span(SemanticSpanKind::Keyword, "extern");
                    Err(format!(
                        "unsupported extern ABI '{}' for function '{}': supported ABIs are extern(c) and extern(system)",
                        function.abi, function.name
                    ))
                } else {
                    Ok(())
                }
            },
            ASTNode::Variable(_) | ASTNode::Statement(_) | ASTNode::Expression(_) => {
                validator.validate_node(node).map(|_| ())
            },
            _ => Ok(()),
        };
        if let Err(message) = result {
            return Err(validator.diagnostic(message));
        }
    }

    Ok(ProgramAnalysis {
        expression_types: validator.expression_types,
        analyzed_expression_types: validator.analyzed_expression_types,
        expected_types: validator.expected_types,
        variant_constructions: validator.variant_constructions,
        variant_patterns: validator.variant_patterns,
        integer_patterns: validator.integer_patterns,
        generic_method_calls: validator.generic_method_calls,
    })
}

fn semantic_diagnostic_for_top_level(
    nodes: &[ASTNode],
    index: usize,
    message: String,
    primary: Option<SemanticSpanHint>,
) -> SemanticDiagnostic {
    let primary = primary.or_else(|| nodes.get(index).map(top_level_span_hint));
    SemanticDiagnostic {
        code: "E3001".to_string(),
        label: message.clone(),
        message,
        top_level_index: index,
        primary,
        span: nodes.get(index).and_then(ASTNode::span).cloned(),
        note: None,
        help: "fix type, mutability, scope, and control-flow errors".to_string(),
    }
}

fn validate_declaration_types(
    nodes: &[ASTNode],
    program: &ProgramTypes,
) -> Result<(), SemanticDiagnostic> {
    let no_generics = HashSet::new();
    let mut checked_aliases = HashSet::new();

    for (index, node) in nodes.iter().enumerate() {
        let ffi_types = match node {
            ASTNode::Function(f) if f.export.is_some() => f
                .parameters
                .iter()
                .map(|p| &p.param_type)
                .chain(f.return_type.iter())
                .collect::<Vec<_>>(),
            ASTNode::ExternFunction(f) => {
                f.params.iter().map(|(_, t)| t).chain(std::iter::once(&f.return_type)).collect()
            },
            _ => Vec::new(),
        };
        if ffi_types.iter().any(|t| contains_future(program, t, &mut HashSet::new())) {
            return Err(semantic_diagnostic_for_top_level(
                nodes,
                index,
                "Future<T> cannot cross an FFI ABI".into(),
                None,
            ));
        }
        let result = match node {
            ASTNode::Function(function) => {
                let mut result =
                    validate_unique_generic_params(&function.generic_params, &function.name);
                if result.is_ok() && function.name == "main" {
                    if !function.generic_params.is_empty() {
                        result =
                            Err("entry function `main` cannot declare generic parameters"
                                .to_string());
                    } else if !function.parameters.is_empty() {
                        result = Err("entry function `main` must have zero parameters".to_string());
                    } else {
                        let return_type = function.return_type.clone().unwrap_or(WaveType::Void);
                        let return_type = program.canonical_type(&return_type);
                        if !matches!(return_type, WaveType::Void | WaveType::Int(32)) {
                            result = Err(format!(
                                "entry function `main` must return `i32` or omit its return type, found `{}`",
                                display_wave_type(&return_type)
                            ));
                        }
                    }
                }
                if result.is_ok() && function.export.is_some() {
                    for parameter in &function.parameters {
                        if is_direct_variant_type(program, &parameter.param_type) {
                            result = Err(format!(
                                "export(c) function `{}` cannot expose variant parameter `{}` directly",
                                function.name, parameter.name
                            ));
                            break;
                        }
                    }
                    if result.is_ok()
                        && function
                            .return_type
                            .as_ref()
                            .is_some_and(|ty| is_direct_variant_type(program, ty))
                    {
                        result = Err(format!(
                            "export(c) function `{}` cannot return a variant directly",
                            function.name
                        ));
                    }
                }
                result
            },
            ASTNode::Struct(structure) => {
                let mut result =
                    validate_unique_generic_params(&structure.generic_params, &structure.name);
                if result.is_ok() {
                    let generics: HashSet<String> =
                        structure.generic_params.iter().cloned().collect();
                    for (field, ty) in &structure.fields {
                        result = program.validate_type(
                            ty,
                            &generics,
                            false,
                            &format!("field `{}.{}`", structure.name, field),
                        );
                        if result.is_err() {
                            break;
                        }
                    }
                }
                result
            },
            ASTNode::ProtoImpl(implementation) => {
                if !program.is_known_named_type(&implementation.target) {
                    Err(format!(
                        "proto implementation targets unknown type `{}`",
                        implementation.target
                    ))
                } else {
                    Ok(())
                }
            },
            ASTNode::TypeAlias(alias) => program
                .validate_type(
                    &alias.target,
                    &no_generics,
                    false,
                    &format!("type alias `{}`", alias.name),
                )
                .and_then(|_| {
                    validate_alias_cycle(
                        &alias.name,
                        program,
                        &mut HashSet::new(),
                        &mut checked_aliases,
                    )
                }),
            ASTNode::Enum(enumeration) => {
                let result = program.validate_type(
                    &enumeration.repr_type,
                    &no_generics,
                    false,
                    &format!("representation of enum `{}`", enumeration.name),
                );
                if result.is_ok()
                    && !is_integer_type(&program.canonical_type(&enumeration.repr_type))
                {
                    Err(format!(
                        "enum `{}` representation must be an integer type, found `{}`",
                        enumeration.name,
                        display_wave_type(&enumeration.repr_type)
                    ))
                } else {
                    result
                }
            },
            ASTNode::Variant(variant) => {
                let mut result =
                    validate_unique_generic_params(&variant.generic_params, &variant.name);
                let generics: HashSet<String> = variant.generic_params.iter().cloned().collect();
                if result.is_ok() && variant.cases.is_empty() {
                    result =
                        Err(format!("variant `{}` must declare at least one case", variant.name));
                }
                if result.is_ok() {
                    for case in &variant.cases {
                        for payload in &case.payload_types {
                            result = program.validate_type(
                                payload,
                                &generics,
                                false,
                                &format!("payload of `{}::{}`", variant.name, case.name),
                            );
                            if result.is_err() {
                                break;
                            }
                        }
                        if result.is_err() {
                            break;
                        }
                    }
                }
                if result.is_ok() {
                    result = validate_finite_variant(&variant.name, program);
                }
                result
            },
            ASTNode::ExternFunction(function) => {
                let mut params = HashSet::new();
                let mut result = Ok(());
                for (name, ty) in &function.params {
                    if !params.insert(name) {
                        result = Err(format!(
                            "duplicate parameter declaration `{}` in extern function `{}`",
                            name, function.name
                        ));
                        break;
                    }
                    result = program.validate_type(
                        ty,
                        &no_generics,
                        false,
                        &format!("parameter `{}` of extern function `{}`", name, function.name),
                    );
                    if result.is_err() {
                        break;
                    }
                }
                if result.is_ok() {
                    result = program.validate_type(
                        &function.return_type,
                        &no_generics,
                        true,
                        &format!("return type of extern function `{}`", function.name),
                    );
                }
                if result.is_ok() {
                    for (name, ty) in &function.params {
                        if is_direct_variant_type(program, ty) {
                            result = Err(format!(
                                "extern(c) function `{}` cannot expose variant parameter `{}` directly",
                                function.name, name
                            ));
                            break;
                        }
                    }
                }
                if result.is_ok() && is_direct_variant_type(program, &function.return_type) {
                    result = Err(format!(
                        "extern(c) function `{}` cannot return a variant directly",
                        function.name
                    ));
                }
                result
            },
            ASTNode::Variable(variable) => program.validate_type(
                &variable.type_name,
                &no_generics,
                false,
                &format!("global variable `{}`", variable.name),
            ),
            _ => Ok(()),
        };
        if let Err(message) = result {
            return Err(semantic_diagnostic_for_top_level(nodes, index, message, None));
        }
    }

    Ok(())
}

fn is_direct_variant_type(program: &ProgramTypes, ty: &WaveType) -> bool {
    matches!(program.canonical_type(ty), WaveType::Variant(_))
}

fn validate_finite_variant(name: &str, program: &ProgramTypes) -> Result<(), String> {
    let mut active = HashSet::new();
    let mut checked = HashSet::new();
    validate_finite_named_type(name, name, program, &mut active, &mut checked)
}

fn validate_finite_named_type(
    root: &str,
    name: &str,
    program: &ProgramTypes,
    active: &mut HashSet<String>,
    checked: &mut HashSet<String>,
) -> Result<(), String> {
    let base = program.named_type_base(name).to_string();
    if checked.contains(&base) || program.generic_type_params.contains(&base) {
        return Ok(());
    }
    if !active.insert(base.clone()) {
        return Err(format!(
            "variant `{}` has infinite size through direct recursive payload `{}`; use `ptr<{}>` for indirection",
            root, base, base
        ));
    }

    if let Some(alias) = program.aliases.get(&base) {
        validate_finite_payload_type(root, alias, program, active, checked)?;
    } else if let Some(fields) = program.structs.get(&base) {
        for field in fields.values() {
            validate_finite_payload_type(root, field, program, active, checked)?;
        }
    } else if let Some(variant) = program.variants.get(&base) {
        for (_, payloads) in &variant.cases {
            for payload in payloads {
                validate_finite_payload_type(root, payload, program, active, checked)?;
            }
        }
    }

    active.remove(&base);
    checked.insert(base);
    Ok(())
}

fn validate_finite_payload_type(
    root: &str,
    ty: &WaveType,
    program: &ProgramTypes,
    active: &mut HashSet<String>,
    checked: &mut HashSet<String>,
) -> Result<(), String> {
    match ty {
        WaveType::Pointer(_) => Ok(()),
        WaveType::Array(inner, _) => {
            validate_finite_payload_type(root, inner, program, active, checked)
        },
        WaveType::Struct(name) | WaveType::Variant(name) => {
            validate_finite_named_type(root, name, program, active, checked)
        },
        _ => Ok(()),
    }
}

fn validate_alias_cycle(
    alias: &str,
    program: &ProgramTypes,
    active: &mut HashSet<String>,
    checked: &mut HashSet<String>,
) -> Result<(), String> {
    if checked.contains(alias) {
        return Ok(());
    }
    if !active.insert(alias.to_string()) {
        return Err(format!("cyclic type alias involving `{}`", alias));
    }
    if let Some(target) = program.aliases.get(alias) {
        validate_alias_type_cycle(target, program, active, checked)?;
    }
    active.remove(alias);
    checked.insert(alias.to_string());
    Ok(())
}

fn validate_alias_type_cycle(
    ty: &WaveType,
    program: &ProgramTypes,
    active: &mut HashSet<String>,
    checked: &mut HashSet<String>,
) -> Result<(), String> {
    match ty {
        WaveType::Struct(name) if program.aliases.contains_key(name) => {
            validate_alias_cycle(name, program, active, checked)
        },
        WaveType::Future(inner) | WaveType::Pointer(inner) | WaveType::Array(inner, _) => {
            validate_alias_type_cycle(inner, program, active, checked)
        },
        _ => Ok(()),
    }
}

fn validate_unique_generic_params(params: &[String], owner: &str) -> Result<(), String> {
    let mut seen = HashSet::new();
    for param in params {
        if !seen.insert(param) {
            return Err(format!("duplicate generic parameter `{}` in `{}`", param, owner));
        }
    }
    Ok(())
}

fn contains_future(program: &ProgramTypes, ty: &WaveType, seen: &mut HashSet<String>) -> bool {
    match ty {
        WaveType::Future(_) => true,
        WaveType::Pointer(inner) | WaveType::Array(inner, _) => {
            contains_future(program, inner, seen)
        },
        WaveType::Struct(name) | WaveType::Variant(name) if seen.insert(name.clone()) => {
            if let Some(alias) = program.aliases.get(name) {
                if contains_future(program, alias, seen) {
                    return true;
                }
            }
            if let Some(fields) = program.structs.get(name) {
                if fields.values().any(|t| contains_future(program, t, seen)) {
                    return true;
                }
            }
            if let Some(v) = program.variants.get(name) {
                if v.cases.iter().flat_map(|(_, p)| p).any(|t| contains_future(program, t, seen)) {
                    return true;
                }
            }
            false
        },
        _ => false,
    }
}

fn analyzed_expression_type(program: &ProgramTypes, ty: &ExpressionType) -> AnalyzedExpressionType {
    match ty {
        ExpressionType::Known(ty) => AnalyzedExpressionType::Resolved(program.canonical_type(ty)),
        ExpressionType::IntLiteral(_) => AnalyzedExpressionType::IntegerLiteral,
        ExpressionType::FloatLiteral => AnalyzedExpressionType::FloatLiteral,
        ExpressionType::Null => AnalyzedExpressionType::Null,
        ExpressionType::ArrayLiteral(_) => AnalyzedExpressionType::ArrayLiteral,
        ExpressionType::AddressedArrayLiteral(_) => AnalyzedExpressionType::AddressedArrayLiteral,
        ExpressionType::Unknown => AnalyzedExpressionType::Unknown,
    }
}

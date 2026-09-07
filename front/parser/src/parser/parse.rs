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

//! Parser entry points and top-level declaration dispatch.
//!
//! These routines construct a source-oriented AST only. Import expansion,
//! generic specialization, and semantic type validation are later phases and
//! must not be silently performed while consuming syntax.

use crate::ast::{ASTNode, Visibility};
use crate::parser::decl::*;
use crate::parser::functions::{parse_export, parse_function};
use crate::parser::items::*;
use crate::verification::*;
use lexer::token::TokenType;
use lexer::Token;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseDiagnostic {
    pub message: String,
    pub line: usize,
    pub column: usize,
    pub span: Option<error::SourceSpan>,
    pub expected: Vec<String>,
    pub found: Option<String>,
    pub context: Option<String>,
    pub help: Option<String>,
    pub note: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    Syntax(ParseDiagnostic),
    Semantic(ParseDiagnostic),
}

impl ParseError {
    pub fn syntax(message: impl Into<String>) -> Self {
        Self::Syntax(ParseDiagnostic {
            message: message.into(),
            line: 0,
            column: 0,
            span: None,
            expected: Vec::new(),
            found: None,
            context: None,
            help: None,
            note: None,
        })
    }

    pub fn syntax_at(token: Option<&Token>, message: impl Into<String>) -> Self {
        let mut err = Self::syntax(message);
        if let Some(tok) = token {
            err = err.with_line_col(tok.line, tok.span.as_ref().map_or(0, |s| s.column));
            err.diag_mut().span = tok.span.clone();
        }
        err
    }

    pub fn semantic(message: impl Into<String>) -> Self {
        Self::Semantic(ParseDiagnostic {
            message: message.into(),
            line: 0,
            column: 0,
            span: None,
            expected: Vec::new(),
            found: None,
            context: None,
            help: None,
            note: None,
        })
    }

    fn diag_mut(&mut self) -> &mut ParseDiagnostic {
        match self {
            ParseError::Syntax(d) | ParseError::Semantic(d) => d,
        }
    }

    fn token_desc(token: &Token) -> String {
        if token.lexeme.is_empty() {
            format!("{:?}", token.token_type)
        } else {
            format!("{:?} (`{}`)", token.token_type, token.lexeme)
        }
    }

    pub fn with_line_col(mut self, line: usize, column: usize) -> Self {
        let d = self.diag_mut();
        d.line = line;
        d.column = column;
        self
    }

    pub fn with_expected(mut self, expected: impl Into<String>) -> Self {
        self.diag_mut().expected.push(expected.into());
        self
    }

    pub fn with_expected_many<I, S>(mut self, expected: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.diag_mut().expected = expected.into_iter().map(|s| s.into()).collect();
        self
    }

    pub fn with_found(mut self, found: impl Into<String>) -> Self {
        self.diag_mut().found = Some(found.into());
        self
    }

    pub fn with_found_token(mut self, token: Option<&Token>) -> Self {
        if let Some(tok) = token {
            let d = self.diag_mut();
            d.line = tok.line;
            d.column = tok.span.as_ref().map_or(0, |s| s.column);
            d.span = tok.span.clone();
            d.found = Some(Self::token_desc(tok));
            if let Some(spelling) = tok.token_type.reserved_spelling() {
                d.message = format!("reserved syntax `{spelling}` is not implemented in Alpha");
            }
        }
        self
    }

    pub fn with_context(mut self, context: impl Into<String>) -> Self {
        self.diag_mut().context = Some(context.into());
        self
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.diag_mut().help = Some(help.into());
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.diag_mut().note = Some(note.into());
        self
    }

    pub fn message(&self) -> &str {
        match self {
            ParseError::Syntax(d) | ParseError::Semantic(d) => &d.message,
        }
    }

    pub fn span(&self) -> Option<&error::SourceSpan> {
        match self {
            Self::Syntax(d) | Self::Semantic(d) => d.span.as_ref(),
        }
    }

    pub fn line(&self) -> usize {
        match self {
            ParseError::Syntax(d) | ParseError::Semantic(d) => d.line,
        }
    }

    pub fn column(&self) -> usize {
        match self {
            ParseError::Syntax(d) | ParseError::Semantic(d) => d.column,
        }
    }

    pub fn expected(&self) -> &[String] {
        match self {
            ParseError::Syntax(d) | ParseError::Semantic(d) => &d.expected,
        }
    }

    pub fn found(&self) -> Option<&str> {
        match self {
            ParseError::Syntax(d) | ParseError::Semantic(d) => d.found.as_deref(),
        }
    }

    pub fn context(&self) -> Option<&str> {
        match self {
            ParseError::Syntax(d) | ParseError::Semantic(d) => d.context.as_deref(),
        }
    }

    pub fn help(&self) -> Option<&str> {
        match self {
            ParseError::Syntax(d) | ParseError::Semantic(d) => d.help.as_deref(),
        }
    }

    pub fn note(&self) -> Option<&str> {
        match self {
            ParseError::Syntax(d) | ParseError::Semantic(d) => d.note.as_deref(),
        }
    }
}

/// Compatibility entry point for consumers that do not retain source provenance.
pub fn parse_syntax_only(tokens: &[Token]) -> Result<Vec<ASTNode>, ParseError> {
    let mut tokens = tokens.to_vec();
    for token in &mut tokens {
        token.span = None;
    }
    parse_syntax_with_spans(&tokens)
}

/// Parse physical syntax with byte ranges preserved through frontend rewrites.
pub fn parse_syntax_with_spans(tokens: &[Token]) -> Result<Vec<ASTNode>, ParseError> {
    validate_explicit_variable_types(tokens)?;

    let mut iter = tokens.iter().peekable();
    let mut nodes = vec![];

    while let Some(token) = iter.peek().copied() {
        let before = iter.clone();
        let first_node = nodes.len();
        match token.token_type {
            TokenType::Whitespace | TokenType::Newline => {
                iter.next();
                continue;
            }
            TokenType::Import => {
                let anchor = (*token).clone();
                iter.next();
                if let Some(path) = parse_import(&mut iter) {
                    nodes.push(path);
                } else {
                    return Err(ParseError::syntax_at(
                        Some(&anchor),
                        "failed to parse import declaration",
                    )
                    .with_context("top-level import")
                    .with_expected("import(\"path\");")
                    .with_found_token(iter.peek().copied())
                    .with_help("imports must use parentheses and end with ';'"));
                }
            }
            TokenType::Pub => {
                let anchor = (*token).clone();
                iter.next();
                while matches!(
                    iter.peek().map(|token| &token.token_type),
                    Some(TokenType::Whitespace | TokenType::Newline)
                ) {
                    iter.next();
                }

                let declaration = match iter.peek().map(|token| &token.token_type) {
                    Some(TokenType::Import) => {
                        iter.next();
                        parse_import(&mut iter)
                    }
                    Some(TokenType::Export) => {
                        iter.next();
                        parse_export(&mut iter).and_then(|mut declarations| {
                            if declarations.len() == 1 {
                                declarations.pop()
                            } else {
                                None
                            }
                        })
                    }
                    Some(TokenType::Fun) => parse_function(&mut iter),
                    Some(TokenType::Struct) => {
                        iter.next();
                        parse_struct(&mut iter)
                    }
                    Some(TokenType::Type) => {
                        iter.next();
                        parse_type_alias(&mut iter)
                    }
                    Some(TokenType::Enum) => {
                        iter.next();
                        parse_enum(&mut iter)
                    }
                    Some(TokenType::Variant) => {
                        iter.next();
                        parse_variant(&mut iter)
                    }
                    Some(TokenType::Const) => {
                        iter.next();
                        parse_const(&mut iter)
                    }
                    Some(TokenType::Static) => {
                        iter.next();
                        parse_static(&mut iter)
                    }
                    _ => None,
                };

                let mut declaration = declaration.ok_or_else(|| {
                    ParseError::syntax_at(
                        Some(&anchor),
                        "`pub` must precede an importable declaration",
                    )
                    .with_context("public declaration")
                    .with_expected_many([
                        "pub fun",
                        "pub import",
                        "pub export(c) fun",
                        "pub struct",
                        "pub enum",
                        "pub variant",
                        "pub type",
                        "pub const",
                        "pub static",
                    ])
                    .with_found_token(iter.peek().copied())
                    .with_help("`pub` controls Wave module visibility; it is not an ABI export")
                })?;

                match &mut declaration {
                    ASTNode::Statement(crate::ast::StatementNode::Import(import)) => {
                        if import.selections.is_empty() {
                            return Err(ParseError::syntax_at(
                                Some(&anchor),
                                "public imports must select symbols to re-export",
                            )
                            .with_context("public import")
                            .with_expected("pub import(\"module\")::{symbol};")
                            .with_help("select the public symbols that this module re-exports"));
                        }
                        import.visibility = Visibility::Public;
                    }
                    ASTNode::Function(function) => {
                        if function.name == "main" {
                            return Err(ParseError::syntax_at(
                                Some(&anchor),
                                "entry function `main` cannot be public",
                            )
                            .with_context("public declaration")
                            .with_expected("fun main() { ... }")
                            .with_help("remove `pub`; `main` is a private program entry point"));
                        }
                        function.visibility = Visibility::Public;
                    }
                    ASTNode::Struct(structure) => structure.visibility = Visibility::Public,
                    ASTNode::TypeAlias(alias) => alias.visibility = Visibility::Public,
                    ASTNode::Enum(enumeration) => enumeration.visibility = Visibility::Public,
                    ASTNode::Variant(variant) => variant.visibility = Visibility::Public,
                    ASTNode::Variable(variable) => variable.visibility = Visibility::Public,
                    _ => unreachable!("public parser only constructs importable declarations"),
                }
                nodes.push(declaration);
            }
            TokenType::Extern => {
                let anchor = (*token).clone();
                iter.next();
                if let Some(extern_nodes) = parse_extern(&mut iter) {
                    nodes.extend(extern_nodes);
                } else {
                    return Err(ParseError::syntax_at(
                        Some(&anchor),
                        "failed to parse extern declaration",
                    )
                    .with_context("top-level extern block/declaration")
                    .with_expected_many([
                        "extern(c) fun name(...);",
                        "extern(c) { fun a(...); fun b(...); }",
                    ])
                    .with_found_token(iter.peek().copied())
                    .with_help("check ABI syntax, function signature, and separators"));
                }
            }
            TokenType::Export => {
                let anchor = (*token).clone();
                iter.next();
                if let Some(export_nodes) = parse_export(&mut iter) {
                    nodes.extend(export_nodes);
                } else {
                    return Err(ParseError::syntax_at(
                        Some(&anchor),
                        "failed to parse export declaration",
                    )
                    .with_context("top-level export block/declaration")
                    .with_expected_many([
                        "export(c) fun name(...) { ... }",
                        "export(c, \"symbol\") fun name(...) { ... }",
                        "export(c) { fun a(...) { ... } fun b(...) { ... } }",
                    ])
                    .with_found_token(iter.peek().copied())
                    .with_help("exports require a concrete non-generic function body"));
                }
            }
            TokenType::Const => {
                let anchor = (*token).clone();
                iter.next();
                if let Some(var) = parse_const(&mut iter) {
                    nodes.push(var);
                } else {
                    return Err(ParseError::syntax_at(
                        Some(&anchor),
                        "failed to parse const declaration",
                    )
                    .with_context("top-level constant declaration")
                    .with_expected("const name: type = value;")
                    .with_found_token(iter.peek().copied())
                    .with_help("const declarations require explicit type and initializer"));
                }
            }
            TokenType::Static => {
                let anchor = (*token).clone();
                iter.next();
                if let Some(var) = parse_static(&mut iter) {
                    nodes.push(var);
                } else {
                    return Err(ParseError::syntax_at(
                        Some(&anchor),
                        "failed to parse static declaration",
                    )
                    .with_context("top-level static declaration")
                    .with_expected("static name: type = value;")
                    .with_found_token(iter.peek().copied())
                    .with_help("static declarations require an explicit type"));
                }
            }
            TokenType::Proto => {
                let anchor = (*token).clone();
                iter.next();
                if let Some(proto_impl) = parse_proto(&mut iter) {
                    nodes.push(proto_impl);
                } else {
                    return Err(ParseError::syntax_at(
                        Some(&anchor),
                        "failed to parse proto implementation",
                    )
                    .with_context("top-level proto block")
                    .with_expected("proto Type { fun method(...); }")
                    .with_found_token(iter.peek().copied())
                    .with_help("check braces and method declarations inside proto"));
                }
            }
            TokenType::Type => {
                let anchor = (*token).clone();
                iter.next(); // consume 'type'
                if let Some(node) = parse_type_alias(&mut iter) {
                    nodes.push(node);
                } else {
                    return Err(
                        ParseError::syntax_at(Some(&anchor), "failed to parse type alias")
                            .with_context("top-level type alias")
                            .with_expected("type Name = ExistingType;")
                            .with_found_token(iter.peek().copied())
                            .with_help("type aliases must include `=` and end with ';'"),
                    );
                }
            }
            TokenType::Enum => {
                let anchor = (*token).clone();
                iter.next(); // consume 'enum'
                if let Some(node) = parse_enum(&mut iter) {
                    nodes.push(node);
                } else {
                    return Err(ParseError::syntax_at(
                        Some(&anchor),
                        "failed to parse enum declaration",
                    )
                    .with_context("top-level enum declaration")
                    .with_expected("enum Name -> i32 { A = 0, B = 1 }")
                    .with_found_token(iter.peek().copied())
                    .with_help("check enum repr type, braces, and variant values"));
                }
            }
            TokenType::Variant => {
                let anchor = (*token).clone();
                iter.next();
                if let Some(node) = parse_variant(&mut iter) {
                    nodes.push(node);
                } else {
                    return Err(ParseError::syntax_at(
                        Some(&anchor),
                        "failed to parse variant declaration",
                    )
                    .with_context("top-level variant declaration")
                    .with_expected("variant Result<T, E> { Ok(T), Err(E) }")
                    .with_found_token(iter.peek().copied())
                    .with_help("check generic parameters, payload types, commas, and braces"));
                }
            }
            TokenType::Struct => {
                let anchor = (*token).clone();
                iter.next();
                if let Some(struct_node) = parse_struct(&mut iter) {
                    nodes.push(struct_node);
                } else {
                    return Err(ParseError::syntax_at(
                        Some(&anchor),
                        "failed to parse struct declaration",
                    )
                    .with_context("top-level struct declaration")
                    .with_expected("struct Name { field: type; fun method(...) { ... } }")
                    .with_found_token(iter.peek().copied())
                    .with_help("check field separators (`;`) and method bodies"));
                }
            }
            TokenType::Fun => {
                let anchor = (*token).clone();
                if let Some(func) = parse_function(&mut iter) {
                    nodes.push(func);
                } else {
                    return Err(ParseError::syntax_at(
                        Some(&anchor),
                        "failed to parse function declaration",
                    )
                    .with_context("top-level function")
                    .with_expected_many([
                        "fun name(params) { ... }",
                        "fun name(params) -> return_type { ... }",
                    ])
                    .with_found_token(iter.peek().copied())
                    .with_help(
                        "check parameter syntax, return type arrow, and function body braces",
                    ));
                }
            }
            TokenType::Eof => break,
            _ => {
                return Err(
                    ParseError::syntax_at(Some(token), "unexpected token at top level")
                        .with_context("top-level items")
                        .with_expected_many([
                            "import", "extern", "pub", "const", "static", "type", "enum",
                            "variant", "struct", "proto", "fun", "export",
                        ])
                        .with_found_token(Some(token))
                        .with_help("only declarations are allowed at top level"),
                );
            }
        }
        for node in &mut nodes[first_node..] {
            let value = std::mem::replace(node, ASTNode::Expression(crate::ast::Expression::Null));
            let span = crate::source::node_span(before.clone(), &mut iter, &value);
            *node = value.with_span(span);
        }
    }

    Ok(nodes)
}

fn validate_explicit_variable_types(tokens: &[Token]) -> Result<(), ParseError> {
    let next_significant = |start: usize| {
        (start..tokens.len()).find(|index| {
            !matches!(
                tokens[*index].token_type,
                TokenType::Whitespace | TokenType::Newline
            )
        })
    };

    for (index, token) in tokens.iter().enumerate() {
        if !matches!(token.token_type, TokenType::Var | TokenType::Static) {
            continue;
        }
        let Some(name_index) = next_significant(index + 1) else {
            continue;
        };
        let TokenType::Identifier(name) = &tokens[name_index].token_type else {
            continue;
        };
        let Some(separator_index) = next_significant(name_index + 1) else {
            continue;
        };
        if matches!(tokens[separator_index].token_type, TokenType::Equal) {
            return Err(ParseError::syntax_at(
                Some(&tokens[separator_index]),
                format!("variable `{name}` requires an explicit type"),
            )
            .with_context("variable declaration")
            .with_expected("var name: Type = value;")
            .with_found_token(Some(&tokens[separator_index]))
            .with_help("Wave does not infer variable types; add a `: Type` annotation"));
        }
    }

    Ok(())
}

pub fn parse(tokens: &[Token]) -> Result<Vec<ASTNode>, ParseError> {
    let nodes = parse_syntax_only(tokens)?;

    if let Err(e) = validate_program(&nodes) {
        return Err(ParseError::semantic(e)
            .with_context("semantic validation")
            .with_help("fix mutability, scope, and expression validity issues"));
    }

    Ok(nodes)
}

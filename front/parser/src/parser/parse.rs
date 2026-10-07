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
    pub related: Vec<error::RelatedDiagnostic>,
    at_eof: bool,
    pub expected: Vec<String>,
    pub found: Option<String>,
    pub context: Option<String>,
    pub help: Option<String>,
    pub note: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    Syntax(Box<ParseDiagnostic>),
    Semantic(Box<ParseDiagnostic>),
}

impl ParseError {
    pub fn syntax(message: impl Into<String>) -> Self {
        Self::Syntax(Box::new(ParseDiagnostic {
            message: message.into(),
            line: 0,
            column: 0,
            span: None,
            related: Vec::new(),
            at_eof: false,
            expected: Vec::new(),
            found: None,
            context: None,
            help: None,
            note: None,
        }))
    }

    pub fn syntax_at(token: Option<&Token>, message: impl Into<String>) -> Self {
        let mut err = Self::syntax(message);
        if let Some(tok) = token {
            err = err.with_line_col(tok.line, tok.span.as_ref().map_or(0, |s| s.column));
            err.diag_mut().span = tok.span.clone();
        }
        err
    }

    pub(crate) fn expected_at(
        token: Option<&Token>,
        anchor: Option<&Token>,
        expected: &str,
        context: &str,
    ) -> Self {
        Self::syntax_at(token.or(anchor), format!("expected {expected} in {context}"))
            .with_expected(expected)
            .with_context(context)
            .with_found("end of file")
            .with_found_token(token)
    }

    pub fn semantic(message: impl Into<String>) -> Self {
        Self::Semantic(Box::new(ParseDiagnostic {
            message: message.into(),
            line: 0,
            column: 0,
            span: None,
            related: Vec::new(),
            at_eof: false,
            expected: Vec::new(),
            found: None,
            context: None,
            help: None,
            note: None,
        }))
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
            d.at_eof = tok.token_type == TokenType::Eof;
            if let Some(spelling) = tok.token_type.reserved_spelling() {
                d.message = format!("reserved syntax `{spelling}` is not implemented in Alpha");
            }
        } else {
            self.diag_mut().at_eof = true;
            self.diag_mut().found = Some("end of file".into());
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

    pub fn related(&self) -> &[error::RelatedDiagnostic] {
        match self {
            Self::Syntax(d) | Self::Semantic(d) => &d.related,
        }
    }

    fn with_unclosed_delimiter(mut self, tokens: &[Token]) -> Self {
        if !matches!(&self, Self::Syntax(d) if d.at_eof) {
            return self;
        }
        // Legacy leaf parsers may already have consumed the EOF sentinel.
        if let Some(eof) = tokens.last().filter(|token| token.token_type == TokenType::Eof) {
            self = self.with_found_token(Some(eof));
        }
        let mut openers: Vec<&Token> = Vec::new();
        for token in tokens {
            match token.token_type {
                TokenType::Lparen | TokenType::Lbrack | TokenType::Lbrace => openers.push(token),
                TokenType::Rparen | TokenType::Rbrack | TokenType::Rbrace => {
                    let Some(opener) = openers.pop() else {
                        return self;
                    };
                    if !matches!(
                        (&opener.token_type, &token.token_type),
                        (TokenType::Lparen, TokenType::Rparen)
                            | (TokenType::Lbrack, TokenType::Rbrack)
                            | (TokenType::Lbrace, TokenType::Rbrace)
                    ) {
                        return self;
                    }
                },
                _ => {},
            }
        }
        if let Some(opener) = openers.last() {
            let (open, close) = match opener.token_type {
                TokenType::Lparen => ("(", ")"),
                TokenType::Lbrack => ("[", "]"),
                TokenType::Lbrace => ("{", "}"),
                _ => unreachable!(),
            };
            let message = format!("unclosed '{open}' opened here; expected '{close}'");
            if let Some(span) = &opener.span {
                self.diag_mut()
                    .related
                    .push(error::RelatedDiagnostic { message, span: span.clone() });
            } else {
                self = self.with_note(format!("{message} (line {})", opener.line));
            }
        }
        self
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
    let nodes = parse_syntax_impl(tokens).map_err(|error| error.with_unclosed_delimiter(tokens))?;
    crate::expression_depth::validate(&nodes).map_err(|diagnostic| {
        let mut failure = ParseError::syntax(diagnostic.message).with_help(diagnostic.help);
        if let ParseError::Syntax(ref mut value) = failure {
            if let Some(span) = diagnostic.span {
                value.line = span.line;
                value.column = span.column;
                value.span = Some(span);
            }
        }
        failure
    })?;
    Ok(nodes)
}

fn parse_syntax_impl(tokens: &[Token]) -> Result<Vec<ASTNode>, ParseError> {
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
            },
            TokenType::Import => {
                iter.next();
                nodes.push(parse_import(&mut iter)?);
            },
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
                        Some(parse_import(&mut iter)?)
                    },
                    Some(TokenType::Export) => {
                        iter.next();
                        let mut declarations = parse_export(&mut iter)?;
                        if declarations.len() == 1 {
                            declarations.pop()
                        } else {
                            None
                        }
                    },
                    Some(TokenType::Fun | TokenType::Async) => Some(parse_function(&mut iter)?),
                    Some(TokenType::Struct) => {
                        iter.next();
                        Some(parse_struct(&mut iter)?)
                    },
                    Some(TokenType::Type) => {
                        iter.next();
                        Some(parse_type_alias(&mut iter)?)
                    },
                    Some(TokenType::Enum) => {
                        iter.next();
                        Some(parse_enum(&mut iter)?)
                    },
                    Some(TokenType::Variant) => {
                        iter.next();
                        Some(parse_variant(&mut iter)?)
                    },
                    Some(TokenType::Const) => {
                        iter.next();
                        Some(parse_const(&mut iter)?)
                    },
                    Some(TokenType::Static) => {
                        iter.next();
                        Some(parse_static(&mut iter)?)
                    },
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
                    },
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
                    },
                    ASTNode::Struct(structure) => structure.visibility = Visibility::Public,
                    ASTNode::TypeAlias(alias) => alias.visibility = Visibility::Public,
                    ASTNode::Enum(enumeration) => enumeration.visibility = Visibility::Public,
                    ASTNode::Variant(variant) => variant.visibility = Visibility::Public,
                    ASTNode::Variable(variable) => variable.visibility = Visibility::Public,
                    _ => unreachable!("public parser only constructs importable declarations"),
                }
                nodes.push(declaration);
            },
            TokenType::Extern => {
                iter.next();
                nodes.extend(parse_extern(&mut iter)?);
            },
            TokenType::Export => {
                iter.next();
                let export_nodes = parse_export(&mut iter)?;
                nodes.extend(export_nodes);
            },
            TokenType::Const => {
                iter.next();
                nodes.push(parse_const(&mut iter)?);
            },
            TokenType::Static => {
                iter.next();
                nodes.push(parse_static(&mut iter)?);
            },
            TokenType::Proto => {
                iter.next();
                nodes.push(parse_proto(&mut iter)?);
            },
            TokenType::Type => {
                iter.next();
                nodes.push(parse_type_alias(&mut iter)?);
            },
            TokenType::Enum => {
                iter.next();
                nodes.push(parse_enum(&mut iter)?);
            },
            TokenType::Variant => {
                iter.next();
                nodes.push(parse_variant(&mut iter)?);
            },
            TokenType::Struct => {
                iter.next();
                let struct_node = parse_struct(&mut iter)?;
                nodes.push(struct_node);
            },
            TokenType::Fun | TokenType::Async => {
                let func = parse_function(&mut iter)?;
                nodes.push(func);
            },
            TokenType::Eof => break,
            _ => {
                return Err(ParseError::syntax_at(Some(token), "unexpected token at top level")
                    .with_context("top-level items")
                    .with_expected_many([
                        "import", "extern", "pub", "const", "static", "type", "enum", "variant",
                        "struct", "proto", "fun", "export",
                    ])
                    .with_found_token(Some(token))
                    .with_help("only declarations are allowed at top level"));
            },
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
            !matches!(tokens[*index].token_type, TokenType::Whitespace | TokenType::Newline)
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

use std::iter::Peekable;
use std::slice::Iter;
use lexer::{Token, TokenType};
use crate::ast::{ASTNode, Expression, Mutability, VariableNode, WaveType};
use crate::format::parse_expression;
use crate::parser::types::{parse_type, token_type_to_wave_type};

pub fn parse_variable_decl(tokens: &mut Peekable<Iter<'_, Token>>, is_const: bool) -> Option<ASTNode> {
    let mut mutability = if is_const {
        Mutability::Const
    } else {
        Mutability::Let
    };

    if !is_const {
        if let Some(Token {
                        token_type: TokenType::Mut,
                        ..
                    }) = tokens.peek()
        {
            tokens.next(); // consume `mut`
            mutability = Mutability::LetMut;
        }
    }

    let name = match tokens.next() {
        Some(Token {
                 token_type: TokenType::Identifier(name),
                 ..
             }) => name.clone(),
        _ => {
            println!(
                "Expected identifier after `{}`",
                if is_const { "const" } else { "let" }
            );
            return None;
        }
    };

    if !matches!(tokens.next().map(|t| &t.token_type), Some(TokenType::Colon)) {
        println!("Expected ':' after identifier");
        return None;
    }

    let type_token = match tokens.next() {
        Some(token) => token.clone(),
        _ => {
            println!("Expected type after ':'");
            return None;
        }
    };

    let wave_type = if let TokenType::Identifier(ref name) = type_token.token_type {
        if let Some(Token {
                        token_type: TokenType::Lchevr,
                        ..
                    }) = tokens.peek()
        {
            tokens.next(); // consume '<'

            let mut inner = String::new();
            let mut depth = 1;

            while let Some(t) = tokens.next() {
                match &t.token_type {
                    TokenType::Lchevr => {
                        depth += 1;
                        inner.push('<');
                    }
                    TokenType::Rchevr => {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        } else {
                            inner.push('>');
                        }
                    }
                    _ => inner.push_str(&t.lexeme),
                }
            }

            let full_type_str = format!("{}<{}>", name, inner);
            let parsed_type = parse_type(&full_type_str);

            if parsed_type.is_none() {
                println!("Unknown generic type: {}", full_type_str);
                return None;
            }

            match token_type_to_wave_type(&parsed_type.unwrap()) {
                Some(wt) => wt,
                None => {
                    println!("Failed to convert to WaveType: {}", full_type_str);
                    return None;
                }
            }
        } else {
            match parse_type(&name).and_then(|tt| token_type_to_wave_type(&tt)) {
                Some(wt) => wt,
                None => {
                    println!("Unknown type: {}", name);
                    return None;
                }
            }
        }
    } else {
        match token_type_to_wave_type(&type_token.token_type) {
            Some(t) => t,
            None => {
                println!("Unknown or unsupported type: {}", type_token.lexeme);
                return None;
            }
        }
    };

    let initial_value = if let Some(Token {
                                        token_type: TokenType::Equal,
                                        ..
                                    }) = tokens.peek()
    {
        tokens.next(); // consume '='
        let expr = parse_expression(tokens)?;
        Some(expr)
    } else {
        None
    };

    if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::SemiColon) {
        println!("Expected ';' after expression");
        return None;
    }
    tokens.next();

    if let (WaveType::Array(_, expected_len), Some(Expression::ArrayLiteral(elements))) =
        (&wave_type, &initial_value)
    {
        if *expected_len != elements.len() as u32 {
            println!(
                "❌ Error: Array length mismatch. Expected {}, but got {} elements",
                expected_len,
                elements.len()
            );
            return None;
        }
    }

    Some(ASTNode::Variable(VariableNode {
        name,
        type_name: wave_type,
        initial_value,
        mutability,
    }))
}

pub fn parse_const(tokens: &mut Peekable<Iter<'_, Token>>) -> Option<ASTNode> {
    parse_variable_decl(tokens, true)
}

pub fn parse_let(tokens: &mut Peekable<Iter<'_, Token>>) -> Option<ASTNode> {
    parse_variable_decl(tokens, false)
}

// VAR parsing
pub fn parse_var(tokens: &mut Peekable<Iter<'_, Token>>) -> Option<ASTNode> {
    let mutability = Mutability::Var;

    let name = match tokens.next() {
        Some(Token {
                 token_type: TokenType::Identifier(name),
                 ..
             }) => name.clone(),
        _ => {
            println!("Expected identifier");
            return None;
        }
    };

    if !matches!(tokens.next().map(|t| &t.token_type), Some(TokenType::Colon)) {
        println!("Expected ':' after identifier");
        return None;
    }

    let type_token = match tokens.next() {
        Some(token) => token.clone(),
        _ => {
            println!("Expected type after ':'");
            return None;
        }
    };

    let wave_type = if let TokenType::Identifier(ref name) = type_token.token_type {
        if let Some(Token {
                        token_type: TokenType::Lchevr,
                        ..
                    }) = tokens.peek()
        {
            tokens.next(); // consume '<'

            let mut inner = String::new();
            let mut depth = 1;

            while let Some(t) = tokens.next() {
                match &t.token_type {
                    TokenType::Lchevr => {
                        depth += 1;
                        inner.push('<');
                    }
                    TokenType::Rchevr => {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        } else {
                            inner.push('>');
                        }
                    }
                    _ => inner.push_str(&t.lexeme),
                }
            }

            let full_type_str = format!("{}<{}>", name, inner);
            let parsed_type = parse_type(&full_type_str);

            if parsed_type.is_none() {
                println!("Unknown generic type: {}", full_type_str);
                return None;
            }

            match token_type_to_wave_type(&parsed_type.unwrap()) {
                Some(wt) => wt,
                None => {
                    println!("Failed to convert to WaveType: {}", full_type_str);
                    return None;
                }
            }
        } else {
            match parse_type(&name).and_then(|tt| token_type_to_wave_type(&tt)) {
                Some(wt) => wt,
                None => {
                    println!("Unknown type: {}", name);
                    return None;
                }
            }
        }
    } else {
        match token_type_to_wave_type(&type_token.token_type) {
            Some(t) => t,
            None => {
                println!("Unknown or unsupported type: {}", type_token.lexeme);
                return None;
            }
        }
    };

    let initial_value = if let Some(Token {
                                        token_type: TokenType::Equal,
                                        ..
                                    }) = tokens.peek()
    {
        tokens.next(); // consume '='
        let expr = parse_expression(tokens)?;
        Some(expr)
    } else {
        None
    };

    if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::SemiColon) {
        println!("Expected ';' after expression");
        return None;
    }
    tokens.next();

    if let (WaveType::Array(_, expected_len), Some(Expression::ArrayLiteral(elements))) =
        (&wave_type, &initial_value)
    {
        if *expected_len != elements.len() as u32 {
            println!(
                "❌ Error: Array length mismatch. Expected {}, but got {} elements",
                expected_len,
                elements.len()
            );
            return None;
        }
    }

    Some(ASTNode::Variable(VariableNode {
        name,
        type_name: wave_type,
        initial_value,
        mutability,
    }))
}
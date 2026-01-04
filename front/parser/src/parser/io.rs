use std::iter::Peekable;
use std::slice::Iter;
use regex::Regex;
use lexer::{Token, TokenType};
use crate::ast::{ASTNode, StatementNode};
use crate::format::parse_expression;

// PRINTLN parsing
pub fn parse_println(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    if tokens.peek()?.token_type != TokenType::Lparen {
        println!("Error: Expected '(' after 'println'");
        return None;
    }
    tokens.next(); // Consume '('

    let content = if let Some(Token {
                                  token_type: TokenType::String(content),
                                  ..
                              }) = tokens.next()
    {
        content.clone()
    } else {
        println!("Error: Expected string literal in 'println'");
        return None;
    };

    let placeholder_count = Regex::new(r"\{[^}]*\}")
        .unwrap()
        .find_iter(&content)
        .count();

    if placeholder_count == 0 {
        if tokens.peek()?.token_type != TokenType::Rparen {
            println!("Error: Expected closing ')'");
            return None;
        }
        tokens.next(); // Consume ')'

        if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::SemiColon) {
            println!("Expected ';' after expression");
            return None;
        }
        tokens.next();

        return Some(ASTNode::Statement(StatementNode::Println(format!(
            "{}\n",
            content
        ))));
    }

    let mut args = Vec::new();
    while let Some(Token {
                       token_type: TokenType::Comma,
                       ..
                   }) = tokens.peek()
    {
        tokens.next(); // Consume ','
        if let Some(expr) = parse_expression(tokens) {
            args.push(expr);
        } else {
            println!("Error: Failed to parse expression in 'println'");
            return None;
        }
    }

    if tokens.peek()?.token_type != TokenType::Rparen {
        println!("Error: Expected closing ')'");
        return None;
    }
    tokens.next(); // Consume ')'

    if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::SemiColon) {
        println!("Expected ';' after expression");
        return None;
    }
    tokens.next();

    if placeholder_count != args.len() {
        println!(
            "Error: Expected {} arguments, found {}",
            placeholder_count,
            args.len()
        );
        return None;
    }

    Some(ASTNode::Statement(StatementNode::PrintlnFormat {
        format: format!("{}\n", content),
        args,
    }))
}

// PRINT parsing
pub fn parse_print(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    if tokens.peek()?.token_type != TokenType::Lparen {
        println!("Error: Expected '(' after 'println'");
        return None;
    }
    tokens.next(); // Consume '('

    let content = if let Some(Token {
                                  token_type: TokenType::String(content),
                                  ..
                              }) = tokens.next()
    {
        content.clone() // Need clone() because it is String
    } else {
        println!("Error: Expected string literal in 'println'");
        return None;
    };

    let placeholder_count = Regex::new(r"\{[^}]*\}")
        .unwrap()
        .find_iter(&content)
        .count();

    if placeholder_count == 0 {
        // No format → Print just a string
        if tokens.peek()?.token_type != TokenType::Rparen {
            println!("Error: Expected closing ')'");
            return None;
        }
        tokens.next(); // Consume ')'

        if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::SemiColon) {
            println!("Expected ';' after expression");
            return None;
        }
        tokens.next();

        return Some(ASTNode::Statement(StatementNode::Print(format!(
            "{}",
            content
        ))));
    }

    let mut args = Vec::new();
    while let Some(Token {
                       token_type: TokenType::Comma,
                       ..
                   }) = tokens.peek()
    {
        tokens.next(); // Consume ','
        if let Some(expr) = parse_expression(tokens) {
            args.push(expr);
        } else {
            println!("Error: Failed to parse expression in 'println'");
            return None;
        }
    }

    if tokens.peek()?.token_type != TokenType::Rparen {
        println!("Error: Expected closing ')'");
        return None;
    }
    tokens.next(); // Consume ')'

    if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::SemiColon) {
        println!("Expected ';' after expression");
        return None;
    }
    tokens.next();

    if placeholder_count != args.len() {
        println!(
            "Error: Expected {} arguments, found {}",
            placeholder_count,
            args.len()
        );
        return None;
    }

    Some(ASTNode::Statement(StatementNode::PrintFormat {
        format: content,
        args,
    }))
}

pub fn parse_input(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    if tokens.peek()?.token_type != TokenType::Lparen {
        println!("Error: Expected '(' after 'println'");
        return None;
    }
    tokens.next(); // Consume '('

    let content = if let Some(Token {
                                  token_type: TokenType::String(content),
                                  ..
                              }) = tokens.next()
    {
        content.clone() // Need clone() because it is String
    } else {
        println!("Error: Expected string literal in 'input'");
        return None;
    };

    let placeholder_count = Regex::new(r"\{[^}]*\}")
        .unwrap()
        .find_iter(&content)
        .count();

    let mut args = Vec::new();
    while let Some(Token {
                       token_type: TokenType::Comma,
                       ..
                   }) = tokens.peek()
    {
        tokens.next(); // Consume ','
        if let Some(expr) = parse_expression(tokens) {
            args.push(expr);
        } else {
            println!("Error: Failed to parse expression in 'println'");
            return None;
        }
    }

    if tokens.peek()?.token_type != TokenType::Rparen {
        println!("Error: Expected closing ')'");
        return None;
    }
    tokens.next(); // Consume ')'

    if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::SemiColon) {
        println!("Expected ';' after expression");
        return None;
    }
    tokens.next();

    if placeholder_count != args.len() {
        println!(
            "Error: Expected {} arguments, found {}",
            placeholder_count,
            args.len()
        );
        return None;
    }

    Some(ASTNode::Statement(StatementNode::Input {
        format: content,
        args,
    }))
}
use std::iter::Peekable;
use std::slice::Iter;
use lexer::{Token, TokenType};
use crate::ast::{ASTNode, AssignOperator, Expression, Operator, StatementNode};
use crate::format::{parse_expression, parse_expression_from_token};
use crate::parser::control::{parse_for, parse_if, parse_while};
use crate::parser::decl::parse_var;
use crate::parser::io::*;
use crate::parser::types::is_expression_start;

pub fn parse_assignment(tokens: &mut Peekable<Iter<Token>>, first_token: &Token) -> Option<ASTNode> {
    let left_expr = match parse_expression_from_token(first_token, tokens) {
        Some(expr) => expr,
        None => {
            println!(
                "Error: Failed to parse left-hand side of assignment. Token: {:?}",
                first_token.token_type
            );
            return None;
        }
    };

    let assign_op = match tokens.peek()?.token_type {
        TokenType::PlusEq => {
            tokens.next();
            Some(AssignOperator::AddAssign)
        }
        TokenType::MinusEq => {
            tokens.next();
            Some(AssignOperator::SubAssign)
        }
        TokenType::StarEq => {
            tokens.next();
            Some(AssignOperator::MulAssign)
        }
        TokenType::DivEq => {
            tokens.next();
            Some(AssignOperator::DivAssign)
        }
        TokenType::RemainderEq => {
            tokens.next();
            Some(AssignOperator::RemAssign)
        }
        TokenType::Equal => {
            tokens.next();
            None
        }
        _ => return None,
    };

    let right_expr = parse_expression(tokens)?;

    if let Some(Token {
                    token_type: TokenType::SemiColon,
                    ..
                }) = tokens.peek()
    {
        tokens.next();
    }

    match (assign_op, &left_expr) {
        (Some(op), Expression::Variable(name)) => {
            Some(ASTNode::Expression(Expression::AssignOperation {
                target: Box::new(Expression::Variable(name.clone())),
                operator: op,
                value: Box::new(right_expr),
            }))
        }
        (None, Expression::Variable(name)) => Some(ASTNode::Statement(StatementNode::Assign {
            variable: name.clone(),
            value: right_expr,
        })),
        (None, Expression::Deref(_)) => Some(ASTNode::Statement(StatementNode::Assign {
            variable: "deref".to_string(),
            value: Expression::BinaryExpression {
                left: Box::new(left_expr),
                operator: Operator::Assign,
                right: Box::new(right_expr),
            },
        })),
        (_, _) => {
            println!(
                "Error: Unsupported assignment left expression: {:?}",
                left_expr
            );
            None
        }
    }
}

pub fn parse_block(tokens: &mut Peekable<Iter<Token>>) -> Option<Vec<ASTNode>> {
    let mut body = vec![];

    while let Some(token) = tokens.peek() {
        if token.token_type == TokenType::Rbrace {
            break;
        }

        if let Some(node) = parse_statement(tokens) {
            body.push(node);
        } else {
            println!("Error: Failed to parse statement inside block.");
            return None;
        }
    }

    if let Some(token) = tokens.next() {
        if token.token_type != TokenType::Rbrace {
            println!(
                "Error: Expected '}}' to close the block, but found {:?}",
                token.token_type
            );
            return None;
        }
    } else {
        println!("Error: Unexpected end of file, expected '}}'");
        return None;
    }

    Some(body)
}

pub fn parse_statement(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    let token = match tokens.peek() {
        Some(t) => (*t).clone(),
        None => return None,
    };

    let node = match token.token_type {
        TokenType::Var => {
            tokens.next();
            parse_var(tokens)
        }
        TokenType::Println => {
            tokens.next();
            parse_println(tokens)
        }
        TokenType::Print => {
            tokens.next();
            parse_print(tokens)
        }
        TokenType::If => {
            tokens.next();
            parse_if(tokens)
        }
        TokenType::For => {
            tokens.next();
            parse_for(tokens)
        }
        TokenType::While => {
            tokens.next();
            parse_while(tokens)
        }
        TokenType::Continue => {
            tokens.next();
            if let Some(Token {
                            token_type: TokenType::SemiColon,
                            ..
                        }) = tokens.peek()
            {
                tokens.next();
            }
            Some(ASTNode::Statement(StatementNode::Continue))
        }
        TokenType::Break => {
            tokens.next();
            if let Some(Token {
                            token_type: TokenType::SemiColon,
                            ..
                        }) = tokens.peek()
            {
                tokens.next();
            }
            Some(ASTNode::Statement(StatementNode::Break))
        }
        TokenType::Return => {
            tokens.next();
            let expr = if let Some(Token {
                                       token_type: TokenType::SemiColon,
                                       ..
                                   }) = tokens.peek()
            {
                tokens.next();
                None
            } else if tokens.peek().is_none() {
                None
            } else {
                let value = parse_expression(tokens)?;
                if let Some(Token {
                                token_type: TokenType::SemiColon,
                                ..
                            }) = tokens.peek()
                {
                    tokens.next();
                }
                Some(value)
            };
            Some(ASTNode::Statement(StatementNode::Return(expr)))
        }
        TokenType::Rbrace => None,

        _ => {
            if is_expression_start(&token.token_type) {
                if let Some(expr) = parse_expression(tokens) {
                    if let Some(Token {
                                    token_type: TokenType::SemiColon,
                                    ..
                                }) = tokens.peek()
                    {
                        tokens.next();
                    }
                    Some(ASTNode::Statement(StatementNode::Expression(expr)))
                } else {
                    println!("Error: Failed to parse expression statement.");
                    None
                }
            } else {
                println!(
                    "Error: Unexpected token, cannot start a statement with: {:?}",
                    token.token_type
                );
                tokens.next();
                None
            }
        }
    };

    node
}

fn skip_whitespace(tokens: &mut Peekable<Iter<Token>>) {
    while let Some(token) = tokens.peek() {
        if token.token_type == TokenType::Whitespace {
            tokens.next();
        } else {
            break;
        }
    }
}

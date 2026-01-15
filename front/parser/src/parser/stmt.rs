use std::iter::Peekable;
use std::slice::Iter;
use lexer::Token;
use lexer::token::TokenType;
use crate::ast::{ASTNode, AssignOperator, Expression, Operator, StatementNode};
use crate::expr::{is_assignable, parse_expression, parse_expression_from_token};
use crate::parser::control::{parse_for, parse_if, parse_while};
use crate::parser::decl::{parse_const, parse_let, parse_var};
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

    match assign_op {
        Some(op) => {
            if !is_assignable(&left_expr) {
                println!(
                    "Error: Unsupported assignment target for '{:?}': {:?}",
                    op, left_expr
                );
                return None;
            }

            Some(ASTNode::Statement(StatementNode::Expression(
                Expression::AssignOperation {
                    target: Box::new(left_expr),
                    operator: op,
                    value: Box::new(right_expr),
                },
            )))
        }

        None => match left_expr {
            Expression::Variable(name) => Some(ASTNode::Statement(StatementNode::Assign {
                variable: name,
                value: right_expr,
            })),

            other => {
                if !is_assignable(&other) {
                    println!("Error: Unsupported assignment left expression: {:?}", other);
                    return None;
                }

                Some(ASTNode::Statement(StatementNode::Assign {
                    variable: "deref".to_string(),
                    value: Expression::BinaryExpression {
                        left: Box::new(other),
                        operator: Operator::Assign,
                        right: Box::new(right_expr),
                    },
                }))
            }
        },
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

    if matches!(token.token_type, TokenType::Identifier(_) | TokenType::Deref) {
        let first = token.clone();

        let mut look = tokens.clone();
        look.next();

        if let Some(node) = parse_assignment(&mut look, &first) {
            *tokens = look;
            return Some(node);
        }
    }

    let node = match token.token_type {
        TokenType::Var => {
            tokens.next();
            parse_var(tokens)
        }
        TokenType::Let => {
            tokens.next();
            parse_let(tokens)
        }
        TokenType::Const => {
            tokens.next();
            parse_const(tokens)
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

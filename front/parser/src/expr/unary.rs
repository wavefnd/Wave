use lexer::Token;
use lexer::token::TokenType;
use crate::ast::{Expression, IncDecKind, Literal, Operator};
use crate::expr::is_assignable;
use crate::expr::primary::parse_primary_expression;

pub fn parse_unary_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    if let Some(token) = tokens.peek() {
        match token.token_type {
            TokenType::Not => {
                tokens.next();
                let inner = parse_unary_expression(tokens)?;
                return Some(Expression::Unary {
                    operator: Operator::Not,
                    expr: Box::new(inner),
                });
            }
            TokenType::BitwiseNot => {
                tokens.next();
                let inner = parse_unary_expression(tokens)?;
                return Some(Expression::Unary {
                    operator: Operator::BitwiseNot,
                    expr: Box::new(inner),
                });
            }
            TokenType::AddressOf => {
                tokens.next();
                let inner = parse_unary_expression(tokens)?;
                return Some(Expression::AddressOf(Box::new(inner)));
            }
            TokenType::Deref => {
                tokens.next();
                let inner = parse_unary_expression(tokens)?;
                return Some(Expression::Deref(Box::new(inner)));
            }
            TokenType::Increment => {
                let tok = tokens.next()?; // '++'
                let inner = parse_unary_expression(tokens)?;
                if !is_assignable(&inner) {
                    println!("Error: ++ target must be assignable (line {})", tok.line);
                    return None;
                }
                return Some(Expression::IncDec {
                    kind: IncDecKind::PreInc,
                    target: Box::new(inner),
                });
            }
            TokenType::Decrement => {
                let tok = tokens.next()?; // '--'
                let inner = parse_unary_expression(tokens)?;
                if !is_assignable(&inner) {
                    println!("Error: -- target must be assignable (line {})", tok.line);
                    return None;
                }
                return Some(Expression::IncDec {
                    kind: IncDecKind::PreDec,
                    target: Box::new(inner),
                });
            }
            TokenType::Minus => {
                let tok = tokens.next()?; // '-'
                let inner = parse_unary_expression(tokens)?;

                match inner {
                    Expression::Literal(Literal::Int(s)) => {
                        return Some(Expression::Literal(Literal::Int(format!("-{}", s))));
                    }
                    Expression::Literal(Literal::Float(f)) => {
                        return Some(Expression::Literal(Literal::Float(-f)));
                    }
                    _ => {
                        println!("Error: unary '-' only supports numeric literals (line {})", tok.line);
                        return None;
                    }
                }
            }

            TokenType::Plus => {
                tokens.next(); // consume '+'
                let inner = parse_unary_expression(tokens)?;
                return Some(inner);
            }
            _ => {}
        }
    }

    parse_primary_expression(tokens)
}
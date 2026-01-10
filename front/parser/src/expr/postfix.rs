use std::iter::Peekable;

use lexer::token::TokenType;
use lexer::Token;

use crate::ast::{Expression, IncDecKind};
use crate::expr::{is_assignable, parse_expression};

pub fn parse_postfix_expression<'a, T>(
    tokens: &mut Peekable<T>,
    mut expr: Expression,
) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    loop {
        match tokens.peek().map(|t| &t.token_type) {
            Some(TokenType::Dot) => {
                tokens.next(); // consume '.'

                let name = if let Some(Token {
                                           token_type: TokenType::Identifier(name),
                                           ..
                                       }) = tokens.next()
                {
                    name.clone()
                } else {
                    println!("Error: Expected identifier after '.'");
                    return None;
                };

                if let Some(Token {
                                token_type: TokenType::Lparen,
                                ..
                            }) = tokens.peek()
                {
                    // ----- MethodCall -----
                    tokens.next(); // consume '('

                    let mut args = Vec::new();
                    if tokens
                        .peek()
                        .map_or(false, |t| t.token_type != TokenType::Rparen)
                    {
                        loop {
                            let arg = parse_expression(tokens)?;
                            args.push(arg);

                            if let Some(Token {
                                            token_type: TokenType::Comma,
                                            ..
                                        }) = tokens.peek()
                            {
                                tokens.next(); // consume ','
                            } else {
                                break;
                            }
                        }
                    }

                    if tokens
                        .peek()
                        .map_or(true, |t| t.token_type != TokenType::Rparen)
                    {
                        println!("Error: Expected ')' after method call arguments");
                        return None;
                    }
                    tokens.next(); // consume ')'

                    let base_expr = expr;
                    expr = Expression::MethodCall {
                        object: Box::new(base_expr),
                        name,
                        args,
                    };
                } else {
                    // ----- FieldAccess -----
                    let base_expr = expr;
                    expr = Expression::FieldAccess {
                        object: Box::new(base_expr),
                        field: name,
                    };
                }
            }

            Some(TokenType::Lbrack) => {
                tokens.next(); // consume '['

                let index_expr = parse_expression(tokens)?;
                if tokens
                    .peek()
                    .map_or(true, |t| t.token_type != TokenType::Rbrack)
                {
                    println!("Error: Expected ']' after index");
                    return None;
                }
                tokens.next(); // consume ']'

                let base_expr = expr;
                expr = Expression::IndexAccess {
                    target: Box::new(base_expr),
                    index: Box::new(index_expr),
                };
            }

            Some(TokenType::Increment) => {
                let line = tokens.peek().unwrap().line;
                tokens.next(); // consume '++'

                if !is_assignable(&expr) {
                    println!("Error: postfix ++ target must be assignable (line {})", line);
                    return None;
                }

                let base = expr;
                expr = Expression::IncDec {
                    kind: IncDecKind::PostInc,
                    target: Box::new(base),
                };

                return Some(expr);
            }

            Some(TokenType::Decrement) => {
                let line = tokens.peek().unwrap().line;
                tokens.next(); // consume '--'

                if !is_assignable(&expr) {
                    println!("Error: postfix -- target must be assignable (line {})", line);
                    return None;
                }

                let base = expr;
                expr = Expression::IncDec {
                    kind: IncDecKind::PostDec,
                    target: Box::new(base),
                };

                return Some(expr);
            }

            _ => break,
        }
    }

    Some(expr)
}

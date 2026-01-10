use std::iter::Peekable;

use lexer::token::TokenType;
use lexer::Token;

use crate::asm::parse_asm_inout_clause;
use crate::ast::{Expression, Literal};
use crate::expr::parse_expression;
use crate::expr::postfix::parse_postfix_expression;

pub fn parse_primary_expression<'a, T>(tokens: &mut Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    let token = (*tokens.peek()?).clone();

    let expr = match &token.token_type {
        TokenType::IntLiteral(s) => {
            tokens.next();
            Some(Expression::Literal(Literal::Int(s.clone())))
        }
        TokenType::Float(value) => {
            tokens.next();
            Some(Expression::Literal(Literal::Float(*value)))
        }
        TokenType::CharLiteral(c) => {
            tokens.next();
            Some(Expression::Literal(Literal::Char(*c)))
        }
        TokenType::BoolLiteral(b) => {
            tokens.next();
            Some(Expression::Literal(Literal::Bool(*b)))
        }
        TokenType::Identifier(name) => {
            let name = name.clone();
            tokens.next();

            let expr = if let Some(peeked_token) = tokens.peek() {
                match &peeked_token.token_type {
                    TokenType::Lparen => {
                        tokens.next();

                        let mut args = vec![];
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
                                    tokens.next();
                                } else {
                                    break;
                                }
                            }
                        }

                        if tokens
                            .peek()
                            .map_or(true, |t| t.token_type != TokenType::Rparen)
                        {
                            println!("Error: Expected ')' after function call arguments");
                            return None;
                        }
                        tokens.next();

                        Expression::FunctionCall { name, args }
                    }
                    TokenType::Lbrace => {
                        tokens.next();
                        let mut fields = vec![];

                        while tokens
                            .peek()
                            .map_or(false, |t| t.token_type != TokenType::Rbrace)
                        {
                            let field_name = if let Some(Token {
                                                             token_type: TokenType::Identifier(n),
                                                             ..
                                                         }) = tokens.next()
                            {
                                n.clone()
                            } else {
                                println!("Error: Expected field name in struct literal.");
                                return None;
                            };

                            if tokens
                                .peek()
                                .map_or(true, |t| t.token_type != TokenType::Colon)
                            {
                                println!("Error: Expected ':' after field name '{}'", field_name);
                                return None;
                            }
                            tokens.next();

                            let value = parse_expression(tokens)?;
                            fields.push((field_name, value));

                            if let Some(Token {
                                            token_type: TokenType::Comma,
                                            ..
                                        }) = tokens.peek()
                            {
                                tokens.next();
                            } else {
                                break;
                            }
                        }

                        if tokens
                            .peek()
                            .map_or(true, |t| t.token_type != TokenType::Rbrace)
                        {
                            println!("Error: Expected '}}' to close struct literal");
                            return None;
                        }
                        tokens.next();

                        Expression::StructLiteral { name, fields }
                    }
                    _ => Expression::Variable(name),
                }
            } else {
                Expression::Variable(name)
            };

            Some(expr)
        }
        TokenType::Lparen => {
            tokens.next();
            let inner_expr = parse_expression(tokens)?;
            if tokens
                .peek()
                .map_or(true, |t| t.token_type != TokenType::Rparen)
            {
                println!("Error: Expected ')' to close grouped expression");
                return None;
            }
            tokens.next();
            Some(Expression::Grouped(Box::new(inner_expr)))
        }
        TokenType::String(value) => {
            tokens.next();
            Some(Expression::Literal(Literal::String(value.clone())))
        }
        TokenType::Lbrack => {
            tokens.next();
            let mut elements = vec![];
            if tokens
                .peek()
                .map_or(false, |t| t.token_type != TokenType::Rbrack)
            {
                loop {
                    elements.push(parse_expression(tokens)?);
                    if let Some(Token {
                                    token_type: TokenType::Comma,
                                    ..
                                }) = tokens.peek()
                    {
                        tokens.next();
                    } else {
                        break;
                    }
                }
            }
            if tokens
                .peek()
                .map_or(true, |t| t.token_type != TokenType::Rbrack)
            {
                println!("Error: Expected ']' to close array literal");
                return None;
            }
            tokens.next();
            Some(Expression::ArrayLiteral(elements))
        }
        TokenType::Asm => {
            tokens.next();
            if tokens.peek()?.token_type != TokenType::Lbrace {
                println!("Expected '{{' after 'asm'");
                return None;
            }
            tokens.next();

            let mut instructions = vec![];
            let mut inputs = vec![];
            let mut outputs = vec![];

            while let Some(token) = tokens.peek() {
                match &token.token_type {
                    TokenType::Rbrace => {
                        tokens.next();
                        break;
                    }

                    TokenType::In => {
                        tokens.next();
                        parse_asm_inout_clause(tokens, true, &mut inputs, &mut outputs)?;
                    }

                    TokenType::Out => {
                        tokens.next();
                        parse_asm_inout_clause(tokens, false, &mut inputs, &mut outputs)?;
                    }

                    TokenType::Identifier(s) if s == "in" => {
                        tokens.next();
                        parse_asm_inout_clause(tokens, true, &mut inputs, &mut outputs)?;
                    }

                    TokenType::Identifier(s) if s == "out" => {
                        tokens.next();
                        parse_asm_inout_clause(tokens, false, &mut inputs, &mut outputs)?;
                    }

                    TokenType::String(s) => {
                        instructions.push(s.clone());
                        tokens.next();
                    }

                    other => {
                        println!("Unexpected token in asm expression: {:?}", other);
                        tokens.next();
                    }
                }
            }

            Some(Expression::AsmBlock {
                instructions,
                inputs,
                outputs,
            })
        }
        _ => match token.token_type {
            TokenType::Continue | TokenType::Break | TokenType::Return | TokenType::SemiColon => None,
            _ => {
                println!("Error: Expected primary expression, found {:?}", token.token_type);
                println!("Error: Expected primary expression, found {:?}", token.lexeme);
                println!("Error: Expected primary expression, found {:?}", token.line);
                None
            }
        },
    };

    let base = expr?;

    parse_postfix_expression(tokens, base)
}

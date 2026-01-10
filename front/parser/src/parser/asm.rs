use std::iter::Peekable;
use std::slice::Iter;
use lexer::Token;
use lexer::token::TokenType;
use crate::ast::{ASTNode, Expression, Literal, StatementNode};
use crate::expr::is_assignable;

pub fn parse_asm_block(tokens: &mut Peekable<Iter<'_, Token>>) -> Option<ASTNode> {
    if tokens.peek()?.token_type != TokenType::Lbrace {
        println!("Expected '{{' after 'asm'");
        return None;
    }
    tokens.next(); // consume '{'

    let mut instructions = vec![];
    let mut inputs: Vec<(String, Expression)> = vec![];
    let mut outputs: Vec<(String, Expression)> = vec![];

    let mut closed = false;

    while let Some(tok) = tokens.peek() {
        match &tok.token_type {
            TokenType::Rbrace => {
                tokens.next(); // consume '}'
                closed = true;
                break;
            }

            TokenType::SemiColon | TokenType::Comma => {
                tokens.next();
            }

            TokenType::String(s) => {
                instructions.push(s.clone());
                tokens.next();
            }

            TokenType::In => {
                tokens.next(); // consume 'in'
                parse_asm_inout_clause(tokens, true, &mut inputs, &mut outputs)?;
            }

            TokenType::Out => {
                tokens.next(); // consume 'out'
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

            other => {
                println!("Unexpected token in asm block: {:?}", other);
                tokens.next();
            }
        }
    }

    if !closed {
        println!("Expected '}}' to close asm block");
        return None;
    }

    Some(ASTNode::Statement(StatementNode::AsmBlock {
        instructions,
        inputs,
        outputs,
    }))
}

pub fn parse_asm_inout_clause<'a, T>(
    tokens: &mut Peekable<T>,
    is_input: bool,
    inputs: &mut Vec<(String, Expression)>,
    outputs: &mut Vec<(String, Expression)>,
) -> Option<()>
where
    T: Iterator<Item = &'a Token>,
{
    if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::Lparen) {
        println!("Expected '(' after in/out");
        return None;
    }
    tokens.next(); // '('

    let reg = match tokens.next() {
        Some(Token { token_type: TokenType::String(s), .. }) => s.clone(),
        Some(Token { token_type: TokenType::Identifier(s), .. }) => s.clone(),
        Some(other) => {
            println!("Expected register string or identifier, got {:?}", other.token_type);
            return None;
        }
        None => {
            println!("Expected register in in/out(...)");
            return None;
        }
    };

    if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::Rparen) {
        println!("Expected ')' after in/out(...)");
        return None;
    }
    tokens.next(); // ')'

    let value_expr = parse_asm_operand(tokens)?;

    if is_input {
        inputs.push((reg, value_expr));
    } else {
        if !is_assignable(&value_expr) {
            println!("Error: out(...) target must be assignable");
            return None;
        }
        outputs.push((reg, value_expr));
    }

    Some(())
}

pub(crate) fn parse_asm_operand<'a, T>(tokens: &mut Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    let tok = tokens.next()?;
    match &tok.token_type {
        TokenType::Identifier(s) => Some(Expression::Variable(s.clone())),
        TokenType::IntLiteral(n) => Some(Expression::Literal(Literal::Int(n.clone()))),
        TokenType::String(s) => Some(Expression::Literal(Literal::String(s.clone()))),

        TokenType::AddressOf => {
            // &x
            let next = tokens.next()?;
            match &next.token_type {
                TokenType::Identifier(s) => Some(Expression::AddressOf(Box::new(Expression::Variable(s.clone())))),
                _ => {
                    println!("Expected identifier after '&' in in/out(...)");
                    None
                }
            }
        }

        TokenType::Deref => {
            let next = tokens.next()?;
            match &next.token_type {
                TokenType::Identifier(s) => Some(Expression::Deref(Box::new(Expression::Variable(s.clone())))),
                _ => {
                    println!("Expected identifier after 'deref' in in/out(...)");
                    None
                }
            }
        }

        TokenType::Minus => {
            match tokens.next()? {
                Token { token_type: TokenType::IntLiteral(n), .. } => {
                    Some(Expression::Literal(Literal::Int(format!("-{}", n))))
                }
                Token { token_type: TokenType::Float(f), .. } => {
                    Some(Expression::Literal(Literal::Float(-*f)))
                }
                other => {
                    println!(
                        "Expected int/float after '-' in asm operand, got {:?}",
                        other.token_type
                    );
                    None
                }
            }
        }

        other => {
            println!("Expected asm operand, got {:?}", other);
            None
        }
    }
}
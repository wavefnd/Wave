use std::iter::Peekable;
use std::slice::Iter;
use lexer::{Token, TokenType};
use crate::ast::Expression;
use crate::format::parse_expression;

pub fn parse_function_call(
    name: Option<String>,
    tokens: &mut Peekable<Iter<Token>>,
) -> Option<Expression> {
    let name = name?;

    if tokens.peek()?.token_type != TokenType::Lparen {
        println!("❌ Expected '(' after function name '{}'", name);
        return None;
    }
    tokens.next(); // consume '('

    let mut args = vec![];

    while let Some(token) = tokens.peek() {
        if token.token_type == TokenType::Rparen {
            tokens.next(); // consume ')'
            break;
        }

        let arg = parse_expression(tokens)?;
        args.push(arg);

        match tokens.peek().map(|t| &t.token_type) {
            Some(TokenType::Comma) => {
                tokens.next(); // consume ','
            }
            Some(TokenType::Rparen) => continue,
            _ => {
                println!(
                    "❌ Unexpected token in function arguments: {:?}",
                    tokens.peek()
                );
                return None;
            }
        }
    }

    Some(Expression::FunctionCall { name, args })
}

pub fn parse_parentheses(tokens: &mut Peekable<Iter<Token>>) -> Vec<Token> {
    let mut param_tokens = vec![];
    let mut paren_depth = 1;

    while let Some(token) = tokens.next() {
        match token.token_type {
            TokenType::Lparen => paren_depth += 1,
            TokenType::Rparen => {
                paren_depth -= 1;
                if paren_depth == 0 {
                    break;
                }
            }
            _ => {}
        }
        param_tokens.push(token.clone());
    }
    param_tokens
}
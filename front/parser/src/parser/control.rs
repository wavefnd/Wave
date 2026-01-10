use std::iter::Peekable;
use std::slice::Iter;
use lexer::Token;
use lexer::token::TokenType;
use crate::ast::{ASTNode, Expression, StatementNode};
use crate::expr::parse_expression;
use crate::parser::stmt::parse_block;

pub fn parse_if(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    if tokens.peek()?.token_type != TokenType::Lparen {
        println!("Error: Expected '(' after 'if'");
        return None;
    }
    tokens.next(); // Consume '('

    let condition = parse_expression(tokens)?;

    if tokens.peek()?.token_type != TokenType::Rparen {
        println!("Error: Expected ')' after 'if' condition");
        return None;
    }
    tokens.next(); // Consume ')'

    if tokens.peek()?.token_type != TokenType::Lbrace {
        println!("Error: Expected '{{' after 'if' condition");
        return None;
    }
    tokens.next(); // Consume '{'
    let body = parse_block(tokens)?;

    let mut else_if_blocks: Vec<(Expression, Vec<ASTNode>)> = Vec::new(); // Changed to store conditions and bodies
    let mut else_block = None;

    while let Some(token) = tokens.peek() {
        if token.token_type != TokenType::Else {
            break;
        }
        tokens.next(); // consume 'else'

        if let Some(Token {
                        token_type: TokenType::If,
                        ..
                    }) = tokens.peek()
        {
            tokens.next(); // consume 'if'

            if tokens.peek()?.token_type != TokenType::Lparen {
                println!("Error: Expected '(' after 'else if'");
                return None;
            }
            tokens.next();
            let else_if_condition = parse_expression(tokens)?;

            if tokens.peek()?.token_type != TokenType::Rparen {
                println!("Error: Expected ')' after 'else if' condition");
                return None;
            }
            tokens.next();

            if tokens.peek()?.token_type != TokenType::Lbrace {
                println!("Error: Expected '{{' after 'else if'");
                return None;
            }
            tokens.next();
            let else_if_body = parse_block(tokens)?;

            // Store condition and body directly instead of nested If node
            else_if_blocks.push((else_if_condition, else_if_body));
        } else {
            if tokens.peek()?.token_type != TokenType::Lbrace {
                println!("Error: Expected '{{' after 'else'");
                return None;
            }
            tokens.next();
            else_block = Some(Box::new(parse_block(tokens)?));
            break;
        }
    }

    Some(ASTNode::Statement(StatementNode::If {
        condition,
        body,
        else_if_blocks: if else_if_blocks.is_empty() {
            None
        } else {
            Some(Box::new(else_if_blocks))
        },
        else_block,
    }))
}

// FOR parsing
pub fn parse_for(_tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    // TODO: Implement proper for loop parsing
    /*
    // Check 'for' keyword and see if there is '()
    if tokens.peek()?.token_type != TokenType::Lparen {
        println!("Error: Expected '(' after 'if'");
        return None;
    }
    tokens.next(); // '(' Consumption

    // Conditional parsing (where condition must be made ASTNode)
    let initialization = parse_expression(tokens)?; // Parsing conditions with expressions
    let condition = parse_expression(tokens)?;
    let increment = parse_expression(tokens)?;
    let body = parse_expression(tokens)?;

    if tokens.peek()?.token_type != TokenType::Rparen {
        println!("Error: Expected ')' after condition");
        return None;
    }
    tokens.next(); // ')' Consumption

    Some(ASTNode::Statement(StatementNode::For {
        initialization,
        condition,
        increment,
        body,
    }))
     */
    None
}

// WHILE parsing
pub fn parse_while(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    if tokens.peek()?.token_type != TokenType::Lparen {
        println!("Error: Expected '(' after 'while'");
        return None;
    }
    tokens.next(); // Consume '('

    let condition = parse_expression(tokens)?;

    if tokens.peek()?.token_type != TokenType::Rparen {
        println!("Error: Expected ')' after 'while' condition");
        return None;
    }
    tokens.next(); // Consume ')'

    if tokens.peek()?.token_type != TokenType::Lbrace {
        println!("Error: Expected '{{' after 'while'");
        return None;
    }
    tokens.next(); // Consume '{'

    let body = parse_block(tokens)?;

    Some(ASTNode::Statement(StatementNode::While { condition, body }))
}
use std::iter::Peekable;
use std::slice::Iter;
use lexer::{Token, TokenType};
use crate::ast::{Operator, Expression, FormatPart, Literal, AssignOperator};

pub fn parse_format_string(s: &str) -> Vec<FormatPart> {
    let mut parts = Vec::new();
    let mut buffer = String::new();
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '{' {
            if let Some('}') = chars.peek() {
                chars.next();
                if !buffer.is_empty() {
                    parts.push(FormatPart::Literal(buffer.clone()));
                    buffer.clear();
                }
                parts.push(FormatPart::Placeholder);
            } else {
                buffer.push(c);
            }
        } else {
            buffer.push(c);
        }
    }

    if !buffer.is_empty() {
        parts.push(FormatPart::Literal(buffer));
    }

    parts
}

pub fn parse_expression<'a, T>(tokens: &mut Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    if let Some(Token { token_type: TokenType::AddressOf, .. }) = tokens.peek() {
        tokens.next(); // consume '&'
        let inner = parse_expression(tokens)?;
        return Some(Expression::AddressOf(Box::new(inner)));
    }

    if let Some(Token { token_type: TokenType::Deref, .. }) = tokens.peek() {
        tokens.next(); // consume 'deref'
        let inner = parse_expression(tokens)?;
        return Some(Expression::Deref(Box::new(inner)));
    }
    let expr = parse_assignment_expression(tokens)?;
    Some(expr)
}

pub fn parse_assignment_expression<'a, T>(tokens: &mut Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    let left = parse_logical_expression(tokens)?;

    if let Some(token) = tokens.peek() {
        let op = match token.token_type {
            TokenType::Equal => AssignOperator::Assign,
            TokenType::PlusEq => AssignOperator::AddAssign,
            TokenType::MinusEq => AssignOperator::SubAssign,
            TokenType::StarEq => AssignOperator::MulAssign,
            TokenType::DivEq => AssignOperator::DivAssign,
            TokenType::RemainderEq => AssignOperator::RemAssign,
            _ => return Some(left)
        };

        tokens.next(); // consume +=, -=

        let right = parse_logical_expression(tokens)?;
        return Some(Expression::AssignOperation {
            target: Box::new(left),
            operator: op,
            value: Box::new(right),
        });
    }

    Some(left)
}

pub fn parse_logical_expression<'a, T>(tokens: &mut Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    let mut left = parse_relational_expression(tokens)?;

    while let Some(token) = tokens.peek() {
        match token.token_type {
            TokenType::LogicalAnd | TokenType::LogicalOr => {
                let op = match token.token_type {
                    TokenType::LogicalAnd => Operator::LogicalAnd,
                    TokenType::LogicalOr => Operator::LogicalOr,
                    _ => unreachable!(),
                };
                tokens.next();

                let right = parse_relational_expression(tokens)?;
                left = Expression::BinaryExpression {
                    left: Box::new(left),
                    operator: op,
                    right: Box::new(right),
                };
            }
            _ => break,
        }
    }
    Some(left)
}

pub fn parse_relational_expression<'a, T>(tokens: &mut Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    let mut left = parse_additive_expression(tokens)?;

    while let Some(token) = tokens.peek() {
        match token.token_type {
            TokenType::EqualTwo |
            TokenType::NotEqual |
            TokenType::Rchevr |
            TokenType::RchevrEq |
            TokenType::Lchevr |
            TokenType::LchevrEq => {
                let op = match token.token_type {
                    TokenType::EqualTwo => Operator::Equal,
                    TokenType::NotEqual => Operator::NotEqual,
                    TokenType::Rchevr => Operator::Greater,
                    TokenType::RchevrEq => Operator::GreaterEqual,
                    TokenType::Lchevr => Operator::Less,
                    TokenType::LchevrEq => Operator::LessEqual,
                    _ => unreachable!(),
                };
                tokens.next();

                let right = parse_additive_expression(tokens)?;
                left = Expression::BinaryExpression {
                    left: Box::new(left),
                    operator: op,
                    right: Box::new(right),
                };
            }
            _ => break,
        }
    }
    Some(left)
}

pub fn parse_additive_expression<'a, T>(tokens: &mut Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    let mut left = parse_multiplicative_expression(tokens)?;

    while let Some(token) = tokens.peek() {
        match token.token_type {
            TokenType::Plus | TokenType::Minus => {
                let op = match token.token_type {
                    TokenType::Plus => Operator::Add,
                    TokenType::Minus => Operator::Subtract,
                    _ => unreachable!(),
                };
                tokens.next();

                let right = parse_multiplicative_expression(tokens)?;
                left = Expression::BinaryExpression {
                    left: Box::new(left),
                    operator: op,
                    right: Box::new(right),
                };
            }
            _ => break,
        }
    }
    Some(left)
}

pub fn parse_multiplicative_expression<'a, T>(tokens: &mut Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    let mut left = parse_primary_expression(tokens)?;

    while let Some(token) = tokens.peek() {
        match token.token_type {
            TokenType::Star | TokenType::Div | TokenType::Remainder => {
                let op = match token.token_type {
                    TokenType::Star => Operator::Multiply,
                    TokenType::Div => Operator::Divide,
                    TokenType::Remainder => Operator::Remainder,
                    _ => unreachable!(),
                };
                tokens.next();

                let right = parse_primary_expression(tokens)?;
                left = Expression::BinaryExpression {
                    left: Box::new(left),
                    operator: op,
                    right: Box::new(right),
                };
            }
            _ => break,
        }
    }
    Some(left)
}

pub fn parse_primary_expression<'a, T>(tokens: &mut Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    let token = tokens.peek()?.clone();

    let mut expr = match &token.token_type {
        TokenType::Number(value) => {
            tokens.next();
            Some(Expression::Literal(Literal::Number(*value)))
        }
        TokenType::Float(value) => {
            tokens.next();
            Some(Expression::Literal(Literal::Float(*value)))
        }
        TokenType::Identifier(name) => {
            let name = name.clone();
            tokens.next(); // consume identifier

            let mut expr = if let Some(Token { token_type: TokenType::Lparen, .. }) = tokens.peek() {
                tokens.next(); // consume '('

                let mut args = vec![];
                while let Some(token) = tokens.peek() {
                    if token.token_type == TokenType::Rparen {
                        tokens.next(); // consume ')'
                        break;
                    }

                    let arg = parse_expression(tokens)?;
                    args.push(arg);

                    if let Some(Token { token_type: TokenType::Comma, .. }) = tokens.peek() {
                        tokens.next(); // consume ','
                    }
                }

                Expression::FunctionCall { name, args }
            } else {
                Expression::Variable(name)
            };

            while let Some(Token { token_type: TokenType::Lbrack, .. }) = tokens.peek() {
                tokens.next(); // consume '['

                let index_expr = parse_expression(tokens)?;

                if tokens.peek()?.token_type != TokenType::Rbrack {
                    println!("Error: Expected ']' after index");
                    return None;
                }
                tokens.next(); // consume ']'

                expr = Expression::IndexAccess {
                    target: Box::new(expr),
                    index: Box::new(index_expr),
                };
            }

            Some(expr)
        }
        TokenType::Lparen => {
            parse_parenthesized_expression(tokens).map(|expr| Expression::Grouped(Box::new(expr)))
        }
        TokenType::String(value) => {
            tokens.next(); // consume the string token
            Some(Expression::Literal(Literal::String(value.clone())))
        }
        TokenType::Lbrack => {
            tokens.next(); // consume '['

            let mut elements = vec![];

            loop {
                if let Some(Token { token_type: TokenType::Rbrack, .. }) = tokens.peek() {
                    tokens.next(); // consume ']'
                    break;
                }

                let expr = parse_expression(tokens)?;
                elements.push(expr);

                match tokens.peek().map(|t| &t.token_type) {
                    Some(TokenType::Comma) => {
                        tokens.next(); // consume ','
                    }
                    Some(TokenType::Rbrack) => continue,
                    _ => {
                        println!("Error: Expected ',' or ']' in array literal");
                        return None;
                    }
                }
            }

            Some(Expression::ArrayLiteral(elements))
        }
        TokenType::Asm => {
            tokens.next(); // consume 'asm'
            if tokens.peek()?.token_type != TokenType::Lbrace {
                println!("Expected '{{' after 'asm'");
                return None;
            }
            tokens.next(); // consume '{'

            let mut instructions = vec![];
            let mut inputs = vec![];
            let mut outputs = vec![];

            while let Some(token) = tokens.peek() {
                match &token.token_type {
                    TokenType::Rbrace => {
                        tokens.next();
                        break;
                    }

                    TokenType::In | TokenType::Out => {
                        let is_input = matches!(token.token_type, TokenType::In);
                        tokens.next();

                        if tokens.peek().map(|t| t.token_type.clone()) != Some(TokenType::Lparen) {
                            println!("Expected '(' after in/out");
                            return None;
                        }
                        tokens.next();

                        let reg_token = tokens.next();
                        let reg = match reg_token {
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

                        if tokens.peek().map(|t| t.token_type.clone()) != Some(TokenType::Rparen) {
                            println!("Expected ')' after in/out");
                            return None;
                        }
                        tokens.next();

                        let value_token = tokens.next();
                        let value = match value_token {
                            Some(Token { token_type: TokenType::Identifier(s), .. }) => s.clone(),
                            Some(Token { token_type: TokenType::Number(n), .. }) => n.to_string(),
                            Some(Token { token_type: TokenType::String(n), .. }) => n.to_string(),
                            Some(other) => {
                                println!("Expected identifier or number after in/out(...), got {:?}", other.token_type);
                                return None;
                            }
                            None => {
                                println!("Expected value after in/out(...)");
                                return None;
                            }
                        };

                        if is_input {
                            inputs.push((reg, value));
                        } else {
                            outputs.push((reg, value));
                        }
                    }


                    TokenType::Identifier(s) if s == "in" || s == "out" => {
                        let is_input = s == "in";
                        tokens.next();

                        if tokens.peek().map(|t| t.token_type.clone()) != Some(TokenType::Lparen) {
                            println!("Expected '(' after in/out");
                            return None;
                        }
                        tokens.next();

                        let reg_token = tokens.next();
                        let reg = match reg_token {
                            Some(Token { token_type: TokenType::String(s), .. })    => s.clone(),
                            Some(Token { token_type: TokenType::Identifier(s), .. })=> s.clone(),
                            Some(other) => {
                                println!("Expected register string or identifier, got {:?}", other.token_type);
                                return None;
                            }
                            None => {
                                println!("Expected register in in/out(...)");
                                return None;
                            }
                        };

                        if tokens.peek().map(|t| t.token_type.clone()) != Some(TokenType::Rparen) {
                            println!("Expected ')' after in/out(...)");
                            return None;
                        }
                        tokens.next();

                        let value_token = tokens.next();
                        let value = match value_token {
                            Some(Token { token_type: TokenType::Identifier(s), .. }) => s.clone(),
                            Some(Token { token_type: TokenType::Number(n), .. })     => n.to_string(),
                            Some(other) => {
                                println!("Expected identifier or number after in/out(...), got {:?}", other.token_type);
                                return None;
                            }
                            None => {
                                println!("Expected value after in/out(...)");
                                return None;
                            }
                        };

                        if is_input {
                            inputs.push((reg, value));
                        } else {
                            outputs.push((reg, value));
                        }
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
        _ => {
            match token.token_type {
                TokenType::Continue | TokenType::Break | TokenType::Return | TokenType::SemiColon => None,
                _ => {
                    println!("Error: Expected primary expression, found {:?}", token.token_type);
                    println!("Error: Expected primary expression, found {:?}", token.lexeme);
                    println!("Error: Expected primary expression, found {:?}", token.line);
                    None
                }
            }
        }
    };

    loop {
        match tokens.peek().map(|t| &t.token_type) {
            Some(TokenType::Dot) => {
                tokens.next();

                let name = if let Some(Token { token_type: TokenType::Identifier(name), .. }) = tokens.next() {
                    name.clone()
                } else {
                    println!("Error: Expected identifier after Dot");
                    return None;
                };

                if tokens.peek().map_or(true, |t| t.token_type != TokenType::Lparen) {
                    println!("Error: Expected '(' for method call");
                    return None;
                }
                tokens.next();

                let mut args = vec![];
                if tokens.peek().map_or(false, |t| t.token_type != TokenType::Rparen) {
                    loop {
                        let arg = parse_expression(tokens)?;
                        args.push(arg);

                        if let Some(Token { token_type: TokenType::Comma, .. }) = tokens.peek() {
                            tokens.next();
                        } else {
                            break;
                        }
                    }
                }

                if tokens.peek().map_or(true, |t| t.token_type != TokenType::Rparen) {
                    println!("Error: Expected ')' after method call arguments");
                    return None;
                }
                tokens.next();

                expr = Some(Expression::MethodCall {
                    object: Box::new(expr?),
                    name,
                    args,
                });
            }
            Some(TokenType::Lbrack) => {
                tokens.next();
                let index_expr = parse_expression(tokens)?;
                if tokens.peek().map_or(true, |t| t.token_type != TokenType::Rbrack) {
                    println!("Error: Expected ']' after index");
                    return None;
                }
                tokens.next();
                expr = Some(Expression::IndexAccess {
                    target: Box::new(expr?),
                    index: Box::new(index_expr),
                });
            }
            _ => {
                break;
            }
        }
    }
    Some(expr?)
}

pub fn parse_parenthesized_expression<'a, T>(tokens: &mut Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    // Ensure the next token is '('
    if tokens.next()?.token_type != TokenType::Lparen {
        println!("Error: Expected '('");
        return None;
    }

    // Parse the inner expression
    let expr = parse_expression(tokens)?;

    // Ensure the next token is ')'
    if tokens.next()?.token_type != TokenType::Rparen {
        println!("Error: Expected ')'");
        return None;
    }

    Some(expr)
}

pub fn parse_expression_from_token(first_token: &Token, tokens: &mut Peekable<Iter<Token>>) -> Option<Expression> {
    match &first_token.token_type {
        TokenType::Identifier(name) => Some(Expression::Variable(name.clone())),

        TokenType::Deref => {
            if let Some(next_token) = tokens.next() {
                if let TokenType::Identifier(name) = &next_token.token_type {
                    return Some(Expression::Deref(Box::new(Expression::Variable(name.clone()))));
                }
            }
            None
        }

        _ => None,
    }
}
use lexer::Token;
use lexer::token::TokenType;
use crate::ast::{Expression, Operator};
use crate::expr::unary::parse_unary_expression;

pub fn parse_logical_or_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    let mut left = parse_logical_and_expression(tokens)?;

    while matches!(tokens.peek().map(|t| &t.token_type), Some(TokenType::LogicalOr)) {
        tokens.next();
        let right = parse_logical_and_expression(tokens)?;
        left = Expression::BinaryExpression {
            left: Box::new(left),
            operator: Operator::LogicalOr,
            right: Box::new(right),
        };
    }

    Some(left)
}

pub fn parse_logical_and_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    let mut left = parse_bitwise_or_expression(tokens)?;

    while matches!(tokens.peek().map(|t| &t.token_type), Some(TokenType::LogicalAnd)) {
        tokens.next();
        let right = parse_bitwise_or_expression(tokens)?;
        left = Expression::BinaryExpression {
            left: Box::new(left),
            operator: Operator::LogicalAnd,
            right: Box::new(right),
        };
    }

    Some(left)
}

pub fn parse_bitwise_or_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    let mut left = parse_bitwise_xor_expression(tokens)?;

    while matches!(tokens.peek().map(|t| &t.token_type), Some(TokenType::BitwiseOr)) {
        tokens.next();
        let right = parse_bitwise_xor_expression(tokens)?;
        left = Expression::BinaryExpression {
            left: Box::new(left),
            operator: Operator::BitwiseOr,
            right: Box::new(right),
        };
    }

    Some(left)
}

pub fn parse_bitwise_xor_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    let mut left = parse_bitwise_and_expression(tokens)?;

    while matches!(tokens.peek().map(|t| &t.token_type), Some(TokenType::Xor)) {
        tokens.next();
        let right = parse_bitwise_and_expression(tokens)?;
        left = Expression::BinaryExpression {
            left: Box::new(left),
            operator: Operator::BitwiseXor,
            right: Box::new(right),
        };
    }

    Some(left)
}

pub fn parse_bitwise_and_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    let mut left = parse_equality_expression(tokens)?;

    while matches!(tokens.peek().map(|t| &t.token_type), Some(TokenType::AddressOf)) {
        tokens.next();
        let right = parse_equality_expression(tokens)?;
        left = Expression::BinaryExpression {
            left: Box::new(left),
            operator: Operator::BitwiseAnd,
            right: Box::new(right),
        };
    }

    Some(left)
}

pub fn parse_equality_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    let mut left = parse_relational_expression(tokens)?;

    while let Some(token) = tokens.peek() {
        let op = match token.token_type {
            TokenType::EqualTwo => Operator::Equal,
            TokenType::NotEqual => Operator::NotEqual,
            _ => break,
        };
        tokens.next();
        let right = parse_relational_expression(tokens)?;
        left = Expression::BinaryExpression {
            left: Box::new(left),
            operator: op,
            right: Box::new(right),
        };
    }

    Some(left)
}

pub fn parse_relational_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    let mut left = parse_shift_expression(tokens)?;

    while let Some(token) = tokens.peek() {
        let op = match token.token_type {
            TokenType::Rchevr => Operator::Greater,
            TokenType::RchevrEq => Operator::GreaterEqual,
            TokenType::Lchevr => Operator::Less,
            TokenType::LchevrEq => Operator::LessEqual,
            _ => break,
        };
        tokens.next();
        let right = parse_shift_expression(tokens)?;
        left = Expression::BinaryExpression {
            left: Box::new(left),
            operator: op,
            right: Box::new(right),
        };
    }

    Some(left)
}

pub fn parse_shift_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    let mut left = parse_additive_expression(tokens)?;

    while let Some(token) = tokens.peek() {
        let op = match token.token_type {
            TokenType::Rol => Operator::ShiftLeft,
            TokenType::Ror => Operator::ShiftRight,
            _ => break,
        };

        tokens.next();
        let right = parse_additive_expression(tokens)?;
        left = Expression::BinaryExpression {
            left: Box::new(left),
            operator: op,
            right: Box::new(right),
        };
    }

    Some(left)
}

pub fn parse_additive_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    let mut left = parse_multiplicative_expression(tokens)?;

    while let Some(token) = tokens.peek() {
        let op = match token.token_type {
            TokenType::Plus => Operator::Add,
            TokenType::Minus => Operator::Subtract,
            _ => break,
        };
        tokens.next();
        let right = parse_multiplicative_expression(tokens)?;
        left = Expression::BinaryExpression {
            left: Box::new(left),
            operator: op,
            right: Box::new(right),
        };
    }

    Some(left)
}

pub fn parse_multiplicative_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    let mut left = parse_unary_expression(tokens)?;

    while let Some(token) = tokens.peek() {
        let op = match token.token_type {
            TokenType::Star => Operator::Multiply,
            TokenType::Div => Operator::Divide,
            TokenType::Remainder => Operator::Remainder,
            _ => break,
        };
        tokens.next();
        let right = parse_unary_expression(tokens)?;
        left = Expression::BinaryExpression {
            left: Box::new(left),
            operator: op,
            right: Box::new(right),
        };
    }

    Some(left)
}
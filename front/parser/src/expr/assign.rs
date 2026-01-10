use lexer::Token;
use lexer::token::TokenType;
use crate::ast::{AssignOperator, Expression};
use crate::expr::binary::parse_logical_or_expression;

pub fn parse_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    parse_assignment_expression(tokens)
}

pub fn parse_assignment_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    let left = parse_logical_or_expression(tokens)?;

    if let Some(token) = tokens.peek() {
        let op = match token.token_type {
            TokenType::Equal => AssignOperator::Assign,
            TokenType::PlusEq => AssignOperator::AddAssign,
            TokenType::MinusEq => AssignOperator::SubAssign,
            TokenType::StarEq => AssignOperator::MulAssign,
            TokenType::DivEq => AssignOperator::DivAssign,
            TokenType::RemainderEq => AssignOperator::RemAssign,
            _ => return Some(left),
        };

        tokens.next(); // consume op

        let right = parse_assignment_expression(tokens)?;
        return Some(Expression::AssignOperation {
            target: Box::new(left),
            operator: op,
            value: Box::new(right),
        });
    }

    Some(left)
}
use std::iter::Peekable;
use std::slice::Iter;
use lexer::Token;
use lexer::token::TokenType;
use crate::ast::Expression;
pub fn is_assignable(expr: &Expression) -> bool {
    match expr {
        Expression::Variable(_) => true,
        Expression::Deref(_) => true,
        Expression::FieldAccess { .. } => true,
        Expression::IndexAccess { .. } => true,

        Expression::Grouped(inner) => is_assignable(inner),

        _ => false,
    }
}

pub fn parse_expression_from_token(
    first_token: &Token,
    tokens: &mut Peekable<Iter<Token>>,
) -> Option<Expression> {
    match &first_token.token_type {
        TokenType::Identifier(name) => Some(Expression::Variable(name.clone())),

        TokenType::Deref => {
            if let Some(next_token) = tokens.next() {
                if let TokenType::Identifier(name) = &next_token.token_type {
                    return Some(Expression::Deref(Box::new(Expression::Variable(
                        name.clone(),
                    ))));
                }
            }
            None
        }

        _ => None,
    }
}

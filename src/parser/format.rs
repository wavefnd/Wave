use std::iter::Peekable;
use crate::lexer::{Token, TokenType};
use crate::parser::ast::{Operator, Expression, FormatPart, Literal};

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
    parse_logical_expression(tokens)
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
            TokenType::Lchevr => {
                let op = match token.token_type {
                    TokenType::EqualTwo => Operator::Equal,
                    TokenType::NotEqual => Operator::NotEqual,
                    TokenType::Rchevr => Operator::Greater,
                    TokenType::Lchevr => Operator::Less,
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
            TokenType::Star | TokenType::Div => {
                let op = match token.token_type {
                    TokenType::Star => Operator::Multiply,
                    TokenType::Div => Operator::Divide,
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

/// Parses a primary expression from the token stream.
///
/// This function examines the next token in the provided iterator and attempts to parse a primary
/// expression. It handles numeric literals, variable identifiers, and parenthesized expressions:
/// - A number token is converted into a literal numeric expression.
/// - An identifier token is parsed into a variable expression.
/// - A left parenthesis triggers the parsing of a grouped (parenthesized) expression,
///   which is handled by `parse_parenthesized_expression`.
///
/// If the token does not match any expected primary expression type, an error is printed and `None` is returned.
///
/// # Examples
///
/// ```
/// // The example assumes that `Token`, `TokenType`, `Expression`, and `Literal` are defined in your module.
/// use std::iter::Peekable;
///
/// // Stub definitions for demonstration purposes.
/// #[derive(Debug, PartialEq)]
/// enum TokenType {
///     Number(i32),
///     Identifier(String),
///     Lparen,
///     // Other token types...
/// }
///
/// #[derive(Debug)]
/// struct Token {
///     token_type: TokenType,
/// }
///
/// #[derive(Debug, PartialEq)]
/// enum Literal {
///     Number(f64),
/// }
///
/// #[derive(Debug, PartialEq)]
/// enum Expression {
///     Literal(Literal),
///     Variable(String),
///     Grouped(Box<Expression>),
/// }
///
/// // Dummy implementation for `parse_parenthesized_expression` for this example.
/// fn parse_parenthesized_expression<'a, T>(_tokens: &mut Peekable<T>) -> Option<Expression>
/// where
///     T: Iterator<Item = &'a Token>,
/// {
///     // This dummy returns a simple literal for demonstration.
///     Some(Expression::Literal(Literal::Number(1.0)))
/// }
///
/// // Example token stream containing a numeric literal.
/// let token = Token { token_type: TokenType::Number(42) };
/// let mut tokens = vec![token].iter().peekable();
///
/// // Parse the primary expression and verify that it is a numeric literal.
/// let expr = parse_primary_expression(&mut tokens).unwrap();
/// assert_eq!(expr, Expression::Literal(Literal::Number(42.0)));
/// ```
pub fn parse_primary_expression<'a, T>(tokens: &mut Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    let token = tokens.peek()?; // Use talkens.peek() first to see if it is the expected value

    match &token.token_type {
        TokenType::Number(value) => Some(Expression::Literal(Literal::Number(*value as f64))),
        TokenType::Identifier(name) => Some(Expression::Variable(name.clone())),
        TokenType::Lparen => {
            let expr = parse_parenthesized_expression(tokens)?;
            Some(Expression::Grouped(Box::new(expr)))
        }
        _ => {
            println!("Error: Expected primary expression, found {:?}", token.token_type);
            None
        }
    }
}

/// Parses an expression enclosed in parentheses from a token stream.
/// 
/// This function consumes tokens from a Peekable iterator by first ensuring that an opening
/// parenthesis `(` is present, then parsing an inner expression using `parse_expression`, and finally
/// verifying that a closing parenthesis `)` follows. It prints an error message and returns `None` if
/// either the opening or closing parenthesis is missing.
/// 
/// # Examples
/// 
/// ```
/// use std::iter::Peekable;
///
/// // Example definitions for demonstration purposes.
/// #[derive(Debug, PartialEq)]
/// enum TokenType { Lparen, Rparen, Number }
///
/// #[derive(Debug)]
/// struct Token {
///     token_type: TokenType,
/// }
///
/// #[derive(Debug)]
/// struct Expression; // Placeholder for the actual Expression type.
///
/// // Dummy implementation of parse_expression for illustration.
/// fn parse_expression<'a, T>(_tokens: &mut Peekable<T>) -> Option<Expression>
/// where
///     T: Iterator<Item = &'a Token>,
/// {
///     Some(Expression)
/// }
///
/// // Example implementation of parse_parenthesized_expression.
/// fn parse_parenthesized_expression<'a, T>(tokens: &mut Peekable<T>) -> Option<Expression>
/// where
///     T: Iterator<Item = &'a Token>,
/// {
///     if tokens.next()?.token_type != TokenType::Lparen {
///         println!("Error: Expected '('");
///         return None;
///     }
///
///     let expr = parse_expression(tokens)?;
///
///     if tokens.next()?.token_type != TokenType::Rparen {
///         println!("Error: Expected ')'");
///         return None;
///     }
///
///     Some(expr)
/// }
///
/// // Construct a token stream representing a parenthesized expression: (number)
/// let tokens = vec![
///     Token { token_type: TokenType::Lparen },
///     Token { token_type: TokenType::Number },
///     Token { token_type: TokenType::Rparen },
/// ];
/// let mut token_iter = tokens.iter().peekable();
/// let expr = parse_parenthesized_expression(&mut token_iter);
/// assert!(expr.is_some());
/// ```
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
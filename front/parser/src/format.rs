use crate::ast::Expression::Variable;
use crate::ast::{AssignOperator, Expression, FormatPart, IncDecKind, Literal, Operator};
use lexer::Token;
use lexer::token::TokenType;
use std::iter::Peekable;
use std::slice::Iter;
use crate::asm::{parse_asm_inout_clause, parse_asm_operand};

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

fn desugar_incdec(line: usize, target: Expression, is_inc: bool) -> Option<Expression> {
    if !is_assignable(&target) {
        println!("Error: ++/-- target must bee assignable (line {})", line);
        return None;
    }

    Some(Expression::AssignOperation {
        target: Box::new(target),
        operator: if is_inc {
            AssignOperator::AddAssign
        } else {
            AssignOperator::SubAssign
        },
        value: Box::new(Expression::Literal(Literal::Int("1".to_string()))),
    })
}

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

fn parse_logical_or_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
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

fn parse_logical_and_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
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

fn parse_bitwise_or_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
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

fn parse_bitwise_xor_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
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

fn parse_bitwise_and_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
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

fn parse_equality_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
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

fn parse_unary_expression<'a, T>(tokens: &mut std::iter::Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    if let Some(token) = tokens.peek() {
        match token.token_type {
            TokenType::Not => {
                tokens.next();
                let inner = parse_unary_expression(tokens)?;
                return Some(Expression::Unary {
                    operator: Operator::Not,
                    expr: Box::new(inner),
                });
            }
            TokenType::BitwiseNot => {
                tokens.next();
                let inner = parse_unary_expression(tokens)?;
                return Some(Expression::Unary {
                    operator: Operator::BitwiseNot,
                    expr: Box::new(inner),
                });
            }
            TokenType::AddressOf => {
                tokens.next();
                let inner = parse_unary_expression(tokens)?;
                return Some(Expression::AddressOf(Box::new(inner)));
            }
            TokenType::Deref => {
                tokens.next();
                let inner = parse_unary_expression(tokens)?;
                return Some(Expression::Deref(Box::new(inner)));
            }
            TokenType::Increment => {
                let tok = tokens.next()?; // '++'
                let inner = parse_unary_expression(tokens)?;
                if !is_assignable(&inner) {
                    println!("Error: ++ target must be assignable (line {})", tok.line);
                    return None;
                }
                return Some(Expression::IncDec {
                    kind: IncDecKind::PreInc,
                    target: Box::new(inner),
                });
            }
            TokenType::Decrement => {
                let tok = tokens.next()?; // '--'
                let inner = parse_unary_expression(tokens)?;
                if !is_assignable(&inner) {
                    println!("Error: -- target must be assignable (line {})", tok.line);
                    return None;
                }
                return Some(Expression::IncDec {
                    kind: IncDecKind::PreDec,
                    target: Box::new(inner),
                });
            }
            TokenType::Minus => {
                let tok = tokens.next()?; // '-'
                let inner = parse_unary_expression(tokens)?;

                match inner {
                    Expression::Literal(Literal::Int(s)) => {
                        return Some(Expression::Literal(Literal::Int(format!("-{}", s))));
                    }
                    Expression::Literal(Literal::Float(f)) => {
                        return Some(Expression::Literal(Literal::Float(-f)));
                    }
                    _ => {
                        println!("Error: unary '-' only supports numeric literals (line {})", tok.line);
                        return None;
                    }
                }
            }

            TokenType::Plus => {
                tokens.next(); // consume '+'
                let inner = parse_unary_expression(tokens)?;
                return Some(inner);
            }
            _ => {}
        }
    }

    parse_primary_expression(tokens)
}

pub fn parse_primary_expression<'a, T>(tokens: &mut Peekable<T>) -> Option<Expression>
where
    T: Iterator<Item = &'a Token>,
{
    let token = (*tokens.peek()?).clone();

    let mut expr = match &token.token_type {
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
            tokens.next(); // consume the string token
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

                    TokenType::In => {
                        tokens.next(); // consume 'in'
                        parse_asm_inout_clause(tokens, true, &mut inputs, &mut outputs)?;
                    }

                    TokenType::Out => {
                        tokens.next(); // consume 'out'
                        parse_asm_inout_clause(tokens, false, &mut inputs, &mut outputs)?;
                    }


                    TokenType::Identifier(s) if s == "in" => {
                        tokens.next(); // consume identifier 'in'
                        parse_asm_inout_clause(tokens, true, &mut inputs, &mut outputs)?;
                    }

                    TokenType::Identifier(s) if s == "out" => {
                        tokens.next(); // consume identifier 'out'
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
            TokenType::Continue | TokenType::Break | TokenType::Return | TokenType::SemiColon => {
                None
            }
            _ => {
                println!(
                    "Error: Expected primary expression, found {:?}",
                    token.token_type
                );
                println!(
                    "Error: Expected primary expression, found {:?}",
                    token.lexeme
                );
                println!("Error: Expected primary expression, found {:?}", token.line);
                None
            }
        },
    };

    if expr.is_none() {
        return None;
    }

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

                let base_expr = match expr.take() {
                    Some(e) => e,
                    None => {
                        println!("Internal parser error: missing base expression before '.'");
                        return None;
                    }
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

                    expr = Some(Expression::MethodCall {
                        object: Box::new(base_expr),
                        name,
                        args,
                    });
                } else {
                    // ----- FieldAccess -----
                    expr = Some(Expression::FieldAccess {
                        object: Box::new(base_expr),
                        field: name,
                    });
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

                let base_expr = match expr.take() {
                    Some(e) => e,
                    None => {
                        println!("Internal parser error: missing base expression before '['");
                        return None;
                    }
                };

                expr = Some(Expression::IndexAccess {
                    target: Box::new(base_expr),
                    index: Box::new(index_expr),
                });
            }

            Some(TokenType::Increment) => {
                let line = tokens.peek().unwrap().line;
                tokens.next(); // consume '++'

                let base = expr.take()?;
                if !is_assignable(&base) {
                    println!("Error: postfix ++ target must be assignable (line {})", line);
                    return None;
                }

                expr = Some(Expression::IncDec {
                    kind: IncDecKind::PostInc,
                    target: Box::new(base),
                });
                break;
            }

            Some(TokenType::Decrement) => {
                let line = tokens.peek().unwrap().line;
                tokens.next(); // consume '--'

                let base = expr.take()?;
                if !is_assignable(&base) {
                    println!("Error: postfix -- target must be assignable (line {})", line);
                    return None;
                }

                expr = Some(Expression::IncDec {
                    kind: IncDecKind::PostDec,
                    target: Box::new(base),
                });
                break;
            }

            _ => break,
        }
    }

    expr
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

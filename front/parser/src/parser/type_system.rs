use std::iter::Peekable;
use std::slice::Iter;
use lexer::{Token, TokenType};
use crate::ast::WaveType;
use crate::*;

pub fn is_expression_start(token_type: &TokenType) -> bool {
    matches!(
        token_type,
        TokenType::Identifier(_)
            | TokenType::Number(_)
            | TokenType::Float(_)
            | TokenType::Lparen
            | TokenType::String(_)
            | TokenType::Lbrack
            | TokenType::Asm
    )
}

pub fn parse_type(type_str: &str) -> Option<TokenType> {
    let type_str = type_str.trim();

    if type_str == "void" {
        return Some(TokenType::TypeVoid);
    }

    if let Some(lt_index) = type_str.find('<') {
        if !type_str.ends_with('>') {
            return None;
        }

        let base = &type_str[..lt_index];
        let inner = &type_str[lt_index + 1..type_str.len() - 1];

        if base == "array" {
            let mut depth = 0;
            let mut split_pos = None;

            for (i, c) in inner.char_indices() {
                match c {
                    '<' => depth += 1,
                    '>' => depth -= 1,
                    ',' if depth == 0 => {
                        split_pos = Some(i);
                        break;
                    }
                    _ => {}
                }
            }

            let split_pos = split_pos?;
            let elem_type_str = inner[..split_pos].trim();
            let size_str = inner[split_pos + 1..].trim();

            let elem_type = parse_type(elem_type_str)?;
            let size = size_str.parse::<u32>().ok()?;

            return Some(TokenType::TypeArray(Box::new(elem_type), size));
        }

        if base == "ptr" {
            let inner_type = parse_type(inner)?;
            return Some(TokenType::TypePointer(Box::new(inner_type)));
        }

        return None;
    }

    if type_str.starts_with('i') {
        let bits = type_str[1..].parse::<u16>().ok()?;
        return Some(TokenType::TypeInt(bits));
    } else if type_str.starts_with('u') {
        let bits = type_str[1..].parse::<u16>().ok()?;
        return Some(TokenType::TypeUint(bits));
    } else if type_str.starts_with('f') {
        let bits = type_str[1..].parse::<u16>().ok()?;
        return Some(TokenType::TypeFloat(bits));
    } else if type_str == "bool" {
        return Some(TokenType::TypeBool);
    } else if type_str == "char" {
        return Some(TokenType::TypeChar);
    } else if type_str == "byte" {
        return Some(TokenType::TypeByte);
    } else if type_str == "str" {
        return Some(TokenType::TypeString);
    }

    if type_str.chars().next().map_or(false, |c| c.is_alphabetic() || c == '_') &&
        type_str.chars().all(|c| c.is_alphanumeric() || c == '_') {
        return Some(TokenType::TypeCustom(type_str.to_string()));
    }

    None
}

pub fn validate_type(expected: &TokenType, actual: &TokenType) -> bool {
    match (expected, actual) {
        (TokenType::TypeInt(_), TokenType::TypeInt(_)) => true,
        (TokenType::TypeUint(_), TokenType::TypeUint(_)) => true,
        (TokenType::TypeFloat(_), TokenType::TypeFloat(_)) => true,
        (TokenType::TypeBool, TokenType::TypeBool) => true,
        (TokenType::TypeChar, TokenType::TypeChar) => true,
        (TokenType::TypeByte, TokenType::TypeByte) => true,
        (TokenType::TypePointer(inner1), TokenType::TypePointer(inner2)) => {
            validate_type(&**inner1, &**inner2) // Double dereference to get TokenType
        }
        (TokenType::TypeArray(inner1, size1), TokenType::TypeArray(inner2, size2)) => {
            validate_type(&**inner1, &**inner2) && size1 == size2 // Double dereference to get TokenType
        }
        (TokenType::TypeString, TokenType::TypeString) => true,
        _ => false,
    }
}

pub fn parse_type_from_token(token_opt: Option<&Token>) -> Option<WaveType> {
    let token = token_opt?;

    match &token.token_type {
        ty @ TokenType::TypeInt(_)
        | ty @ TokenType::TypeUint(_)
        | ty @ TokenType::TypeFloat(_)
        | ty @ TokenType::TypeBool
        | ty @ TokenType::TypeChar
        | ty @ TokenType::TypeByte
        | ty @ TokenType::TypeString
        | ty @ TokenType::TypePointer(_)
        | ty @ TokenType::TypeArray(_, _) => token_type_to_wave_type(ty),

        TokenType::Identifier(name) => {
            if let Some(tt) = parse_type(name) {
                token_type_to_wave_type(&tt)
            } else {
                Some(WaveType::Struct(name.clone()))
            }
        }

        _ => None,
    }
}

pub fn parse_type_from_stream(tokens: &mut Peekable<Iter<Token>>) -> Option<WaveType> {
    let type_token = tokens.next()?; // consume identifier

    if let TokenType::Identifier(name) = &type_token.token_type {
        if let Some(Token { token_type: TokenType::Lchevr, .. }) = tokens.peek() {
            tokens.next(); // consume '<'

            let inner_type = parse_type_from_stream(tokens)?;

            if tokens.peek()?.token_type == TokenType::Comma {
                tokens.next(); // consume ','

                let size_token = tokens.next()?;
                let size = if let TokenType::Number(n) = size_token.token_type {
                    n as u32
                } else {
                    println!("Error: Expected number for array size");
                    return None;
                };

                if tokens.peek()?.token_type != TokenType::Rchevr {
                    println!("Error: Expected '>' after array type");
                    return None;
                }
                tokens.next(); // consume '>'

                return Some(WaveType::Array(Box::new(inner_type), size));
            } else {
                if tokens.peek()?.token_type == TokenType::Rchevr {
                    tokens.next(); // consume '>'
                    if name == "ptr" {
                        return Some(WaveType::Pointer(Box::new(inner_type)));
                    } else {
                        println!("Error: Unsupported generic type '{}'", name);
                        return None;
                    }
                } else {
                    println!("Error: Expected ',' or '>' in generic type");
                    return None;
                }
            }
        }

        return parse_type(name).and_then(|t| token_type_to_wave_type(&t));
    }

    token_type_to_wave_type(&type_token.token_type)
}
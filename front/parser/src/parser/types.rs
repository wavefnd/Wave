use std::iter::Peekable;
use std::slice::Iter;
use lexer::Token;
use lexer::token::*;
use crate::ast::WaveType;
use crate::decl::collect_generic_inner;

pub fn token_type_to_wave_type(token_type: &TokenType) -> Option<WaveType> {
    match token_type {
        TokenType::TypeVoid => Some(WaveType::Void),
        TokenType::TypeInt(bits) => Some(WaveType::Int(*bits)),
        TokenType::TokenTypeInt(int_type) => match int_type {
            IntegerType::I8 => Some(WaveType::Int(8)),
            IntegerType::I16 => Some(WaveType::Int(16)),
            IntegerType::I32 => Some(WaveType::Int(32)),
            IntegerType::I64 => Some(WaveType::Int(64)),
            IntegerType::I128 => Some(WaveType::Int(128)),
            IntegerType::I256 => Some(WaveType::Int(256)),
            IntegerType::I512 => Some(WaveType::Int(512)),
            IntegerType::I1024 => Some(WaveType::Int(1024)),
            _ => panic!("Unhandled integer type: {:?}", int_type),
        },
        TokenType::TypeUint(bits) => Some(WaveType::Uint(*bits)),
        TokenType::TokenTypeUint(uint_type) => match uint_type {
            UnsignedIntegerType::U8 => Some(WaveType::Uint(8)),
            UnsignedIntegerType::U16 => Some(WaveType::Uint(16)),
            UnsignedIntegerType::U32 => Some(WaveType::Uint(32)),
            UnsignedIntegerType::U64 => Some(WaveType::Uint(64)),
            UnsignedIntegerType::U128 => Some(WaveType::Uint(128)),
            UnsignedIntegerType::U256 => Some(WaveType::Uint(256)),
            UnsignedIntegerType::U512 => Some(WaveType::Uint(512)),
            UnsignedIntegerType::U1024 => Some(WaveType::Uint(1024)),
            _ => panic!("Unhandled uint type: {:?}", uint_type),
        },
        TokenType::TokenTypeFloat(float_type) => match float_type {
            FloatType::F32 => Some(WaveType::Float(32)),
            FloatType::F64 => Some(WaveType::Float(64)),
        },
        TokenType::TypeFloat(bits) => Some(WaveType::Float(*bits)),
        TokenType::TypeBool => Some(WaveType::Bool),
        TokenType::TypeChar => Some(WaveType::Char),
        TokenType::TypeByte => Some(WaveType::Byte),
        TokenType::TypeString => Some(WaveType::String),
        TokenType::TypePointer(inner) => {
            token_type_to_wave_type(inner).map(|t| WaveType::Pointer(Box::new(t)))
        }
        TokenType::TypeArray(inner, size) => {
            token_type_to_wave_type(inner).map(|t| WaveType::Array(Box::new(t), *size))
        }
        TokenType::TypeCustom(name) => Some(WaveType::Struct(name.clone())),
        _ => None,
    }
}

pub fn is_expression_start(token_type: &TokenType) -> bool {
    matches!(
        token_type,
        TokenType::Identifier(_)
            | TokenType::IntLiteral(_)
            | TokenType::Float(_)
            | TokenType::Lparen
            | TokenType::String(_)
            | TokenType::Lbrack
            | TokenType::Asm
            | TokenType::Deref
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

    if type_str
        .chars()
        .next()
        .map_or(false, |c| c.is_alphabetic() || c == '_')
        && type_str.chars().all(|c| c.is_alphanumeric() || c == '_')
    {
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

pub fn parse_type_from_token(token_opt: Option<&&Token>) -> Option<WaveType> {
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
        | ty @ TokenType::TypeArray(_, _)
        | ty @ TokenType::TokenTypeInt(_)
        | ty @ TokenType::TokenTypeUint(_)
        | ty @ TokenType::TokenTypeFloat(_) => token_type_to_wave_type(ty),

        TokenType::Identifier(name) => match name.as_str() {
            "i8" => Some(WaveType::Int(8)),
            "i16" => Some(WaveType::Int(16)),
            "i32" => Some(WaveType::Int(32)),
            "i64" => Some(WaveType::Int(64)),
            "u8" => Some(WaveType::Uint(8)),
            "u16" => Some(WaveType::Uint(16)),
            "u32" => Some(WaveType::Uint(32)),
            "u64" => Some(WaveType::Uint(64)),
            "f32" => Some(WaveType::Float(32)),
            "f64" => Some(WaveType::Float(64)),
            "bool" => Some(WaveType::Bool),
            "char" => Some(WaveType::Char),
            "byte" => Some(WaveType::Byte),
            "str" => Some(WaveType::String),
            _ => {
                if let Some(tt) = parse_type(name) {
                    token_type_to_wave_type(&tt)
                } else {
                    Some(WaveType::Struct(name.clone()))
                }
            }
        },

        _ => None,
    }
}

pub fn parse_type_from_stream(tokens: &mut Peekable<Iter<Token>>) -> Option<WaveType> {
    while matches!(tokens.peek().map(|t| &t.token_type), Some(TokenType::Whitespace)) {
        tokens.next();
    }

    let type_token = tokens.next()?;

    if let TokenType::Identifier(name) = &type_token.token_type {
        while matches!(tokens.peek().map(|t| &t.token_type), Some(TokenType::Whitespace)) {
            tokens.next();
        }

        if matches!(tokens.peek().map(|t| &t.token_type), Some(TokenType::Lchevr)) {
            tokens.next(); // consume '<'

            let inner = collect_generic_inner(tokens)?;
            let full_type_str = format!("{}<{}>", name, inner);

            let parsed_tt = parse_type(&full_type_str)?;
            return token_type_to_wave_type(&parsed_tt);
        }

        let parsed_tt = parse_type(name)?;
        return token_type_to_wave_type(&parsed_tt);
    }

    token_type_to_wave_type(&type_token.token_type)
}

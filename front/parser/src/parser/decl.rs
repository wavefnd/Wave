use std::iter::Peekable;
use std::slice::Iter;
use lexer::Token;
use lexer::token::TokenType;
use crate::ast::{ASTNode, Expression, ExternFunctionNode, Mutability, VariableNode, WaveType};
use crate::expr::parse_expression;
use crate::parser::types::{parse_type, token_type_to_wave_type};
use crate::types::parse_type_from_stream;

pub fn collect_generic_inner(tokens: &mut Peekable<Iter<'_, Token>>) -> Option<String> {
    let mut inner = String::new();
    let mut depth: i32 = 1;

    while let Some(t) = tokens.next() {
        match &t.token_type {
            TokenType::Lchevr => {
                depth += 1;
                inner.push('<');
                continue;
            }
            TokenType::Rchevr => {
                depth -= 1;
                if depth == 0 {
                    return Some(inner);
                }
                inner.push('>');
                continue;
            }
            _ => {}
        }

        let text: &str = if !t.lexeme.is_empty() {
            t.lexeme.as_str()
        } else if let TokenType::Identifier(name) = &t.token_type {
            name.as_str()
        } else {
            ""
        };

        for ch in text.chars() {
            match ch {
                '<' => {
                    depth += 1;
                    inner.push('<');
                }
                '>' => {
                    depth -= 1;
                    if depth == 0 {
                        return Some(inner);
                    }
                    inner.push('>');
                }
                _ => inner.push(ch),
            }
        }
    }

    println!("Unclosed generic type: missing '>'");
    None
}


pub fn parse_variable_decl(tokens: &mut Peekable<Iter<'_, Token>>, is_const: bool) -> Option<ASTNode> {
    let mut mutability = if is_const {
        Mutability::Const
    } else {
        Mutability::Let
    };

    if !is_const {
        if let Some(Token {
                        token_type: TokenType::Mut,
                        ..
                    }) = tokens.peek()
        {
            tokens.next(); // consume `mut`
            mutability = Mutability::LetMut;
        }
    }

    let name = match tokens.next() {
        Some(Token {
                 token_type: TokenType::Identifier(name),
                 ..
             }) => name.clone(),
        _ => {
            println!(
                "Expected identifier after `{}`",
                if is_const { "const" } else { "let" }
            );
            return None;
        }
    };

    if !matches!(tokens.next().map(|t| &t.token_type), Some(TokenType::Colon)) {
        println!("Expected ':' after identifier");
        return None;
    }

    let type_token = match tokens.next() {
        Some(token) => token.clone(),
        _ => {
            println!("Expected type after ':'");
            return None;
        }
    };

    let wave_type = if let TokenType::Identifier(ref name) = type_token.token_type {
        if let Some(Token {
                        token_type: TokenType::Lchevr,
                        ..
                    }) = tokens.peek()
        {
            tokens.next(); // consume '<'

            let inner = collect_generic_inner(tokens)?;
            let full_type_str = format!("{}<{}>", name, inner);
            let parsed_type = parse_type(&full_type_str);

            if parsed_type.is_none() {
                println!("Unknown generic type: {}", full_type_str);
                return None;
            }

            match token_type_to_wave_type(&parsed_type.unwrap()) {
                Some(wt) => wt,
                None => {
                    println!("Failed to convert to WaveType: {}", full_type_str);
                    return None;
                }
            }
        } else {
            match parse_type(&name).and_then(|tt| token_type_to_wave_type(&tt)) {
                Some(wt) => wt,
                None => {
                    println!("Unknown type: {}", name);
                    return None;
                }
            }
        }
    } else {
        match token_type_to_wave_type(&type_token.token_type) {
            Some(t) => t,
            None => {
                println!("Unknown or unsupported type: {}", type_token.lexeme);
                return None;
            }
        }
    };

    let initial_value = if let Some(Token {
                                        token_type: TokenType::Equal,
                                        ..
                                    }) = tokens.peek()
    {
        tokens.next(); // consume '='
        let expr = parse_expression(tokens)?;
        Some(expr)
    } else {
        None
    };

    if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::SemiColon) {
        println!("Expected ';' after expression");
        return None;
    }
    tokens.next();

    if let (WaveType::Array(_, expected_len), Some(Expression::ArrayLiteral(elements))) =
        (&wave_type, &initial_value)
    {
        if *expected_len != elements.len() as u32 {
            println!(
                "❌ Error: Array length mismatch. Expected {}, but got {} elements",
                expected_len,
                elements.len()
            );
            return None;
        }
    }

    Some(ASTNode::Variable(VariableNode {
        name,
        type_name: wave_type,
        initial_value,
        mutability,
    }))
}

pub fn parse_const(tokens: &mut Peekable<Iter<'_, Token>>) -> Option<ASTNode> {
    parse_variable_decl(tokens, true)
}

pub fn parse_let(tokens: &mut Peekable<Iter<'_, Token>>) -> Option<ASTNode> {
    parse_variable_decl(tokens, false)
}

// VAR parsing
pub fn parse_var(tokens: &mut Peekable<Iter<'_, Token>>) -> Option<ASTNode> {
    let mutability = Mutability::Var;

    let name = match tokens.next() {
        Some(Token {
                 token_type: TokenType::Identifier(name),
                 ..
             }) => name.clone(),
        _ => {
            println!("Expected identifier");
            return None;
        }
    };

    if !matches!(tokens.next().map(|t| &t.token_type), Some(TokenType::Colon)) {
        println!("Expected ':' after identifier");
        return None;
    }

    let type_token = match tokens.next() {
        Some(token) => token.clone(),
        _ => {
            println!("Expected type after ':'");
            return None;
        }
    };

    let wave_type = if let TokenType::Identifier(ref name) = type_token.token_type {
        if let Some(Token {
                        token_type: TokenType::Lchevr,
                        ..
                    }) = tokens.peek()
        {
            tokens.next(); // consume '<'

            let inner = collect_generic_inner(tokens)?;
            let full_type_str = format!("{}<{}>", name, inner);
            let parsed_type = parse_type(&full_type_str);

            if parsed_type.is_none() {
                println!("Unknown generic type: {}", full_type_str);
                return None;
            }

            match token_type_to_wave_type(&parsed_type.unwrap()) {
                Some(wt) => wt,
                None => {
                    println!("Failed to convert to WaveType: {}", full_type_str);
                    return None;
                }
            }
        } else {
            match parse_type(&name).and_then(|tt| token_type_to_wave_type(&tt)) {
                Some(wt) => wt,
                None => {
                    println!("Unknown type: {}", name);
                    return None;
                }
            }
        }
    } else {
        match token_type_to_wave_type(&type_token.token_type) {
            Some(t) => t,
            None => {
                println!("Unknown or unsupported type: {}", type_token.lexeme);
                return None;
            }
        }
    };

    let initial_value = if let Some(Token {
                                        token_type: TokenType::Equal,
                                        ..
                                    }) = tokens.peek()
    {
        tokens.next(); // consume '='
        let expr = parse_expression(tokens)?;
        Some(expr)
    } else {
        None
    };

    if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::SemiColon) {
        println!("Expected ';' after expression");
        return None;
    }
    tokens.next();

    if let (WaveType::Array(_, expected_len), Some(Expression::ArrayLiteral(elements))) =
        (&wave_type, &initial_value)
    {
        if *expected_len != elements.len() as u32 {
            println!(
                "❌ Error: Array length mismatch. Expected {}, but got {} elements",
                expected_len,
                elements.len()
            );
            return None;
        }
    }

    Some(ASTNode::Variable(VariableNode {
        name,
        type_name: wave_type,
        initial_value,
        mutability,
    }))
}

fn skip_ws(tokens: &mut Peekable<Iter<'_, Token>>) {
    while let Some(t) = tokens.peek() {
        match t.token_type {
            TokenType::Whitespace | TokenType::Newline => {
                tokens.next();
            }
            _ => break,
        }
    }
}

fn expect(tokens: &mut Peekable<Iter<'_, Token>>, ty: TokenType, msg: &str) -> bool {
    if tokens.peek().map(|t| t.token_type.clone()) == Some(ty) {
        tokens.next();
        true
    } else {
        println!("Error: {}", msg);
        false
    }
}

/// extern(abi) ... / extern(abi, "sym") ...
fn parse_extern_header(tokens: &mut Peekable<Iter<'_, Token>>) -> Option<(String, Option<String>)> {
    skip_ws(tokens);

    if !expect(tokens, TokenType::Lparen, "Expected '(' after 'extern'") {
        return None;
    }

    skip_ws(tokens);

    let abi = match tokens.next() {
        Some(Token { token_type: TokenType::Identifier(name), .. }) => name.clone(),
        other => {
            println!("Error: Expected ABI identifier in extern(...), found {:?}", other);
            return None;
        }
    };

    skip_ws(tokens);

    // optional: , "symbol"
    let mut global_symbol: Option<String> = None;
    if tokens.peek().map(|t| t.token_type.clone()) == Some(TokenType::Comma) {
        tokens.next(); // consume ','
        skip_ws(tokens);

        global_symbol = match tokens.next() {
            Some(Token { token_type: TokenType::String(s), .. }) => Some(s.clone()),
            other => {
                println!("Error: Expected string literal after ',' in extern(...), found {:?}", other);
                return None;
            }
        };

        skip_ws(tokens);
    }

    if !expect(tokens, TokenType::Rparen, "Expected ')' to close extern(...)") {
        return None;
    }

    Some((abi, global_symbol))
}

fn peek_non_ws_token_type(tokens: &Peekable<Iter<'_, Token>>) -> Option<TokenType> {
    let mut it = tokens.clone();
    while let Some(t) = it.peek() {
        match t.token_type {
            TokenType::Whitespace | TokenType::Newline => {
                it.next();
            }
            _ => return Some(t.token_type.clone()),
        }
    }
    None
}

fn parse_extern_fun_decl(
    tokens: &mut Peekable<Iter<'_, Token>>,
    abi: String,
    global_symbol: Option<&String>,
) -> Option<ExternFunctionNode> {
    skip_ws(tokens);

    // 'fun'
    match tokens.peek() {
        Some(Token { token_type: TokenType::Fun, .. }) => { tokens.next(); }
        other => {
            println!("Error: Expected 'fun' in extern block, found {:?}", other);
            return None;
        }
    }

    skip_ws(tokens);

    // name
    let name = match tokens.next() {
        Some(Token { token_type: TokenType::Identifier(n), .. }) => n.clone(),
        other => {
            println!("Error: Expected function name after 'fun', found {:?}", other);
            return None;
        }
    };

    skip_ws(tokens);

    if !expect(tokens, TokenType::Lparen, "Expected '(' after extern function name") {
        return None;
    }

    // params
    let mut params: Vec<(String, WaveType)> = Vec::new();
    let mut idx: usize = 0;

    loop {
        skip_ws(tokens);

        if tokens.peek().map(|t| t.token_type.clone()) == Some(TokenType::Rparen) {
            tokens.next(); // consume ')'
            break;
        }

        // named param? (Identifier ... :)
        let is_named = match tokens.peek() {
            Some(Token { token_type: TokenType::Identifier(_), .. }) => {
                let next_ty = peek_non_ws_token_type(tokens);
                if let Some(TokenType::Identifier(_)) = tokens.peek().map(|t| t.token_type.clone()) {
                    let mut la = tokens.clone();
                    la.next(); // consume identifier
                    while let Some(t) = la.peek() {
                        match t.token_type {
                            TokenType::Whitespace | TokenType::Newline => { la.next(); }
                            _ => break,
                        }
                    }
                    la.peek().map(|t| t.token_type.clone()) == Some(TokenType::Colon)
                } else {
                    false
                }
            }
            _ => false,
        };

        if is_named {
            // name :
            let param_name = if let Some(Token { token_type: TokenType::Identifier(n), .. }) = tokens.next() {
                n.clone()
            } else {
                unreachable!();
            };

            skip_ws(tokens);

            if !expect(tokens, TokenType::Colon, "Expected ':' after parameter name in extern function") {
                return None;
            }

            skip_ws(tokens);

            let ty = match parse_type_from_stream(tokens) {
                Some(t) => t,
                None => {
                    println!("Error: Invalid type in extern parameter '{}'", param_name);
                    return None;
                }
            };

            params.push((param_name, ty));
        } else {
            // type-only param
            let ty = match parse_type_from_stream(tokens) {
                Some(t) => t,
                None => {
                    println!("Error: Invalid type in extern parameter list");
                    return None;
                }
            };
            let param_name = format!("arg{}", idx);
            idx += 1;
            params.push((param_name, ty));
        }

        skip_ws(tokens);

        // ',' or ')'
        match tokens.peek().map(|t| t.token_type.clone()) {
            Some(TokenType::Comma) => {
                tokens.next();
                continue;
            }
            Some(TokenType::Rparen) => {
                tokens.next();
                break;
            }
            other => {
                println!("Error: Expected ',' or ')' in extern parameter list, found {:?}", other);
                return None;
            }
        }
    }

    skip_ws(tokens);

    // return type: optional -> ty
    let return_type = match tokens.peek().map(|t| t.token_type.clone()) {
        Some(TokenType::Arrow) => {
            tokens.next(); // consume '->'
            skip_ws(tokens);

            match parse_type_from_stream(tokens) {
                Some(t) => t,
                None => {
                    println!("Error: Invalid return type in extern function '{}'", name);
                    return None;
                }
            }
        }
        _ => WaveType::Void,
    };

    skip_ws(tokens);

    // per-function symbol: optional string literal
    let mut symbol: Option<String> = None;
    if let Some(Token { token_type: TokenType::String(s), .. }) = tokens.peek() {
        let s = s.clone();
        tokens.next();
        symbol = Some(s);
    }

    // apply global symbol if per-fn missing
    if symbol.is_none() {
        symbol = global_symbol.cloned();
    }

    skip_ws(tokens);

    if !expect(tokens, TokenType::SemiColon, "Expected ';' after extern function declaration") {
        return None;
    }

    Some(ExternFunctionNode {
        name,
        abi,
        symbol,
        params,
        return_type,
    })
}

pub fn parse_extern(tokens: &mut Peekable<Iter<'_, Token>>) -> Option<Vec<ASTNode>> {
    let (abi, global_symbol) = parse_extern_header(tokens)?;

    skip_ws(tokens);

    // block or single
    if tokens.peek().map(|t| t.token_type.clone()) == Some(TokenType::Lbrace) {
        tokens.next(); // consume '{'

        let mut nodes: Vec<ASTNode> = Vec::new();

        loop {
            skip_ws(tokens);

            match tokens.peek().map(|t| t.token_type.clone()) {
                Some(TokenType::Rbrace) => {
                    tokens.next(); // consume '}'
                    break;
                }
                Some(TokenType::Fun) => {
                    let ef = parse_extern_fun_decl(tokens, abi.clone(), global_symbol.as_ref())?;
                    nodes.push(ASTNode::ExternFunction(ef));
                }
                Some(TokenType::Whitespace) | Some(TokenType::Newline) => {
                    tokens.next();
                }
                other => {
                    println!("Error: Unexpected token in extern block: {:?}", other);
                    return None;
                }
            }
        }

        skip_ws(tokens);
        if tokens.peek().map(|t| t.token_type.clone()) == Some(TokenType::SemiColon) {
            tokens.next();
        }

        Some(nodes)
    } else if tokens.peek().map(|t| t.token_type.clone()) == Some(TokenType::Fun) {
        let ef = parse_extern_fun_decl(tokens, abi, global_symbol.as_ref())?;
        Some(vec![ASTNode::ExternFunction(ef)])
    } else {
        println!("Error: Expected 'fun' or '{{' after extern(...)");
        None
    }
}
use std::collections::HashSet;
use std::iter::Peekable;
use std::slice::Iter;
use regex::Regex;
use ::lexer::*;
use parser::ast::*;
use crate::*;
use crate::parser::format::*;
use crate::type_system::*;

pub fn parse(tokens: &Vec<Token>) -> Option<Vec<ASTNode>> {
    let mut iter = tokens.iter().peekable();
    let mut nodes = vec![];

    while let Some(token) = iter.peek() {
        match token.token_type {
            TokenType::Whitespace | TokenType::Newline => {
                iter.next();
                continue;
            }
            TokenType::Import => {
                iter.next();
                if let Some(path) = parse_import(&mut iter) {
                    nodes.push(path);
                } else {
                    return None;
                }
            }
            TokenType::Const => {
                iter.next();
                if let Some(var) = parse_const(&mut iter) {
                    nodes.push(var);
                } else {
                    return None;
                }
            }
            TokenType::Proto => {
                iter.next();
                if let Some(proto) = parse_proto(&mut iter) {
                    nodes.push(proto);
                } else {
                    return None;
                }
            }
            TokenType::Struct => {
                iter.next();
                if let Some(struct_node) = parse_struct(&mut iter) {
                    nodes.push(struct_node);
                } else {
                    println!("Failed to parse struct");
                    return None;
                }
            }
            TokenType::Fun => {
                if let Some(func) = parse_function(&mut iter) {
                    nodes.push(func);
                } else {
                    println!("❌ Failed to parse function");
                    return None;
                }
            }
            TokenType::Eof => break,
            _ => {
                println!("❌ Unexpected token at top level: {:?}", token);
                return None;
            }
        }
    }

    Some(nodes)
}

pub fn parse_parameters(tokens: &mut Peekable<Iter<Token>>) -> Vec<ParameterNode> {
    let mut params = vec![];
    while tokens.peek().map_or(false, |t| t.token_type != TokenType::Rparen) {
        let name = if let Some(Token { token_type: TokenType::Identifier(n), .. }) = tokens.next() {
            n.clone()
        } else {
            println!("Error: Expected parameter name");
            break;
        };

        if tokens.peek().map_or(true, |t| t.token_type != TokenType::Colon) {
            println!("Error: Expected ':' after parameter name '{}'", name);
            break;
        }
        tokens.next();

        let param_type = match parse_type_from_stream(tokens) {
            Some(pt) => pt,
            None => {
                println!("Error: Failed to parse type for parameter '{}'", name);
                break;
            }
        };

        let initial_value = if tokens.peek().map_or(false, |t| t.token_type == TokenType::Equal) {
            tokens.next(); // consume '='
            match tokens.next() {
                Some(Token { token_type: TokenType::Number(n), .. }) => Some(Value::Int(*n)),
                Some(Token { token_type: TokenType::Float(f), .. }) => Some(Value::Float(*f)),
                Some(Token { token_type: TokenType::String(s), .. }) => Some(Value::Text(s.clone())),
                _ => {
                    println!("Error: Unsupported initializer for parameter '{}'", name);
                    None
                }
            }
        } else {
            None
        };

        params.push(ParameterNode {
            name,
            param_type,
            initial_value,
        });

        match tokens.peek().map(|t| &t.token_type) {
            Some(TokenType::Comma) => {
                tokens.next(); // consume ','
            }
            Some(TokenType::SemiColon) => {
                println!("Error: use `,` instead of `;` to separate parameters");
                break;
            }
            Some(TokenType::Rparen) => {
                // 루프 끝
            }
            _ => {
                println!("Error: Expected ',' or ')' after parameter");
                break;
            }
        }
    }

    if tokens.peek().map_or(true, |t| t.token_type != TokenType::Rparen) {
        println!("Error: Expected ')' or ',' in parameter list");
    } else {
        tokens.next();
    }

    params
}

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
            FloatType::F128 => Some(WaveType::Float(128)),
            FloatType::F256 => Some(WaveType::Float(256)),
            FloatType::F512 => Some(WaveType::Float(512)),
            FloatType::F1024 => Some(WaveType::Float(1024)),
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
        TokenType::TypeCustom(name) => {
            Some(WaveType::Struct(name.clone()))
        }
        _ => None,
    }
}

pub fn extract_body(tokens: &mut Peekable<Iter<Token>>) -> Option<Vec<ASTNode>> {
    let mut body = vec![];

    if tokens.peek()?.token_type != TokenType::Lbrace {
        println!("❌ Expected '{{' at the beginning of function body");
        return None;
    }
    tokens.next(); // consume '{'

    while let Some(token) = tokens.peek() {
        match &token.token_type {
            TokenType::Whitespace => {
                tokens.next(); // ignore
            }
            TokenType::Rbrace => {
                tokens.next();
                break;
            }
            TokenType::Eof => {
                println!("❌ Unexpected EOF inside function body");
                return None;
            }
            TokenType::Asm => {
                tokens.next();
                body.push(parse_asm_block(tokens)?);
            }
            TokenType::Var => {
                tokens.next(); // consume 'var'
                body.push(parse_var(tokens)?);
            }
            TokenType::Let => {
                tokens.next(); // consume 'let'
                body.push(parse_let(tokens)?);
            }
            TokenType::Println => {
                tokens.next(); // consume 'println'
                let node = parse_println(tokens)?;
                // Added semicolon handling
                if let Some(Token { token_type: TokenType::SemiColon, .. }) = tokens.peek() {
                    tokens.next();
                }
                body.push(node);
            }
            TokenType::Print => {
                tokens.next(); // consume 'print'
                let node = parse_print(tokens)?;
                // Added semicolon handling
                if let Some(Token { token_type: TokenType::SemiColon, .. }) = tokens.peek() {
                    tokens.next();
                }
                body.push(node);
            }
            TokenType::If => {
                tokens.next();
                body.push(parse_if(tokens)?);
            }
            TokenType::For => {
                tokens.next();
                body.push(parse_for(tokens)?);
            }
            TokenType::While => {
                tokens.next();
                body.push(parse_while(tokens)?);
            }
            TokenType::Identifier(_) => {
                if let Some(expr) = parse_expression(tokens) {
                    if let Some(Token { token_type: TokenType::SemiColon, .. }) = tokens.peek() {
                        tokens.next(); // consume ';'
                    }
                    body.push(ASTNode::Statement(StatementNode::Expression(expr)));
                } else {
                    println!("❌ Failed to parse expression starting with identifier");
                    return None;
                }
            }
            TokenType::Break => {
                tokens.next(); // consume 'break'
                if let Some(Token { token_type: TokenType::SemiColon, .. }) = tokens.peek() {
                    tokens.next(); // consume ;
                }
                body.push(ASTNode::Statement(StatementNode::Break));
            }
            TokenType::Continue => {
                tokens.next(); // consume 'continue'
                if let Some(Token { token_type: TokenType::SemiColon, .. }) = tokens.peek() {
                    tokens.next(); // consume ;
                }
                body.push(ASTNode::Statement(StatementNode::Continue));
            }
            TokenType::Return => {
                tokens.next(); // consume 'return'

                let expr = if let Some(Token { token_type: TokenType::SemiColon, .. }) = tokens.peek() {
                    tokens.next(); // return;
                    None
                } else {
                    let value = match parse_expression(tokens) {
                        Some(v) => v,
                        None => {
                            println!("Error: Expected valid expression after 'return'");
                            return None;
                        }
                    };

                    if let Some(Token { token_type: TokenType::SemiColon, .. }) = tokens.peek() {
                        tokens.next();
                    } else {
                        println!("Error: Missing semicolon after return expression");
                        return None;
                    }
                    Some(value)
                };

                body.push(ASTNode::Statement(StatementNode::Return(expr)));
            }
            TokenType::Deref => {
                let token = (*token).clone();
                tokens.next();
                body.push(parse_assignment(tokens, &token)?);
            }
            _ => {
                if let Some(expr) = parse_expression(tokens) {
                    if let Some(Token { token_type: TokenType::SemiColon, .. }) = tokens.peek() {
                        tokens.next(); // consume ;
                    }
                    body.push(ASTNode::Statement(StatementNode::Expression(expr)));
                } else {
                    tokens.next(); // fallback skip
                }
            }
        }
    }

    Some(body)
}

pub fn parse_function_call(name: Option<String>, tokens: &mut Peekable<Iter<Token>>) -> Option<Expression> {
    let name = name?;

    if tokens.peek()?.token_type != TokenType::Lparen {
        println!("❌ Expected '(' after function name '{}'", name);
        return None;
    }
    tokens.next(); // consume '('

    let mut args = vec![];

    while let Some(token) = tokens.peek() {
        if token.token_type == TokenType::Rparen {
            tokens.next(); // consume ')'
            break;
        }

        let arg = parse_expression(tokens)?;
        args.push(arg);

        match tokens.peek().map(|t| &t.token_type) {
            Some(TokenType::Comma) => {
                tokens.next(); // consume ','
            }
            Some(TokenType::Rparen) => continue,
            _ => {
                println!("❌ Unexpected token in function arguments: {:?}", tokens.peek());
                return None;
            }
        }
    }

    Some(Expression::FunctionCall {
        name,
        args,
    })
}

fn parse_parentheses(tokens: &mut Peekable<Iter<Token>>) -> Vec<Token> {
    let mut param_tokens = vec![];
    let mut paren_depth = 1;

    while let Some(token) = tokens.next() {
        match token.token_type {
            TokenType::Lparen => paren_depth += 1,
            TokenType::Rparen => {
                paren_depth -= 1;
                if paren_depth == 0 {
                    break;
                }
            }
            _ => {}
        }
        param_tokens.push(token.clone());
    }
    param_tokens
}

// FUN parsing
fn parse_function(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    tokens.next();

    let name = match tokens.next() {
        Some(Token { token_type: TokenType::Identifier(name), .. }) => name.clone(),
        _ => return None,
    };

    if tokens.peek()?.token_type != TokenType::Lparen {
        return None;
    }

    tokens.next(); // consume '('
    let parameters = parse_parameters(tokens);

    let mut param_names = HashSet::new();
    for param in &parameters {
        if !param_names.insert(param.name.clone()) {
            println!("Error: Parameter '{}' is declared multiple times", param.name);
            return None;
        }
    }

    let return_type = if let Some(Token { token_type: TokenType::Arrow, .. }) = tokens.peek() {
        tokens.next(); // consume '->'
        parse_type_from_stream(tokens)
    } else {
        None
    };

    let body = extract_body(tokens)?;
    Some(ASTNode::Function(FunctionNode {
        name,
        parameters,
        body,
        return_type,
    }))
}

fn parse_variable_decl(
    tokens: &mut Peekable<Iter<'_, Token>>,
    is_const: bool
) -> Option<ASTNode> {
    let mut mutability = if is_const {
        Mutability::Const
    } else {
        Mutability::Let
    };

    if !is_const {
        if let Some(Token { token_type: TokenType::Mut, .. }) = tokens.peek() {
            tokens.next(); // consume `mut`
            mutability = Mutability::LetMut;
        }
    }

    let name = match tokens.next() {
        Some(Token { token_type: TokenType::Identifier(name), .. }) => name.clone(),
        _ => {
            println!("Expected identifier after `{}`", if is_const { "const" } else { "let" });
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
        if let Some(Token { token_type: TokenType::Lchevr, .. }) = tokens.peek() {
            tokens.next(); // consume '<'

            let mut inner = String::new();
            let mut depth = 1;

            while let Some(t) = tokens.next() {
                match &t.token_type {
                    TokenType::Lchevr => {
                        depth += 1;
                        inner.push('<');
                    },
                    TokenType::Rchevr => {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        } else {
                            inner.push('>');
                        }
                    },
                    _ => inner.push_str(&t.lexeme),
                }
            }

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

    let initial_value = if let Some(Token { token_type: TokenType::Equal, .. }) = tokens.peek() {
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

    if let (WaveType::Array(_, expected_len), Some(Expression::ArrayLiteral(elements))) = (&wave_type, &initial_value) {
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

fn parse_const(tokens: &mut Peekable<Iter<'_, Token>>) -> Option<ASTNode> {
    parse_variable_decl(tokens, true)
}

fn parse_let(tokens: &mut Peekable<Iter<'_, Token>>) -> Option<ASTNode> {
    parse_variable_decl(tokens, false)
}

// VAR parsing
fn parse_var(tokens: &mut Peekable<Iter<'_, Token>>) -> Option<ASTNode> {
    let mutability = Mutability::Var;

    let name = match tokens.next() {
        Some(Token { token_type: TokenType::Identifier(name), .. }) => name.clone(),
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
        if let Some(Token { token_type: TokenType::Lchevr, .. }) = tokens.peek() {
            tokens.next(); // consume '<'

            let mut inner = String::new();
            let mut depth = 1;

            while let Some(t) = tokens.next() {
                match &t.token_type {
                    TokenType::Lchevr => {
                        depth += 1;
                        inner.push('<');
                    },
                    TokenType::Rchevr => {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        } else {
                            inner.push('>');
                        }
                    },
                    _ => inner.push_str(&t.lexeme),
                }
            }

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

    let initial_value = if let Some(Token { token_type: TokenType::Equal, .. }) = tokens.peek() {
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

    if let (WaveType::Array(_, expected_len), Some(Expression::ArrayLiteral(elements))) = (&wave_type, &initial_value) {
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

// PRINTLN parsing
fn parse_println(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    if tokens.peek()?.token_type != TokenType::Lparen {
        println!("Error: Expected '(' after 'println'");
        return None;
    }
    tokens.next(); // Consume '('

    let content = if let Some(Token { token_type: TokenType::String(content), .. }) = tokens.next() {
        content.clone()
    } else {
        println!("Error: Expected string literal in 'println'");
        return None;
    };

    let placeholder_count = Regex::new(r"\{[^}]*\}")
        .unwrap()
        .find_iter(&content)
        .count();

    if placeholder_count == 0 {
        if tokens.peek()?.token_type != TokenType::Rparen {
            println!("Error: Expected closing ')'");
            return None;
        }
        tokens.next(); // Consume ')'

        if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::SemiColon) {
            println!("Expected ';' after expression");
            return None;
        }
        tokens.next();

        return Some(ASTNode::Statement(StatementNode::Println(
            format!("{}\n", content),
        )));
    }

    let mut args = Vec::new();
    while let Some(Token { token_type: TokenType::Comma, .. }) = tokens.peek() {
        tokens.next(); // Consume ','
        if let Some(expr) = parse_expression(tokens) {
            args.push(expr);
        } else {
            println!("Error: Failed to parse expression in 'println'");
            return None;
        }
    }

    if tokens.peek()?.token_type != TokenType::Rparen {
        println!("Error: Expected closing ')'");
        return None;
    }
    tokens.next(); // Consume ')'

    if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::SemiColon) {
        println!("Expected ';' after expression");
        return None;
    }
    tokens.next();

    if placeholder_count != args.len() {
        println!(
            "Error: Expected {} arguments, found {}",
            placeholder_count,
            args.len()
        );
        return None;
    }

    Some(ASTNode::Statement(StatementNode::PrintlnFormat {
        format: format!("{}\n", content),
        args,
    }))
}

// PRINT parsing
fn parse_print(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    if tokens.peek()?.token_type != TokenType::Lparen {
        println!("Error: Expected '(' after 'println'");
        return None;
    }
    tokens.next(); // Consume '('

    let content = if let Some(Token { token_type: TokenType::String(content), .. }) = tokens.next() {
        content.clone() // Need clone() because it is String
    } else {
        println!("Error: Expected string literal in 'println'");
        return None;
    };

    let placeholder_count = Regex::new(r"\{[^}]*\}")
        .unwrap()
        .find_iter(&content)
        .count();

    if placeholder_count == 0 {
        // No format → Print just a string
        if tokens.peek()?.token_type != TokenType::Rparen {
            println!("Error: Expected closing ')'");
            return None;
        }
        tokens.next(); // Consume ')'

        if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::SemiColon) {
            println!("Expected ';' after expression");
            return None;
        }
        tokens.next();

        return Some(ASTNode::Statement(StatementNode::Print(
            format!("{}", content),
        )));
    }

    let mut args = Vec::new();
    while let Some(Token { token_type: TokenType::Comma, .. }) = tokens.peek() {
        tokens.next(); // Consume ','
        if let Some(expr) = parse_expression(tokens) {
            args.push(expr);
        } else {
            println!("Error: Failed to parse expression in 'println'");
            return None;
        }
    }

    if tokens.peek()?.token_type != TokenType::Rparen {
        println!("Error: Expected closing ')'");
        return None;
    }
    tokens.next(); // Consume ')'

    if tokens.peek().map(|t| &t.token_type) != Some(&TokenType::SemiColon) {
        println!("Expected ';' after expression");
        return None;
    }
    tokens.next();

    if placeholder_count != args.len() {
        println!(
            "Error: Expected {} arguments, found {}",
            placeholder_count,
            args.len()
        );
        return None;
    }

    Some(ASTNode::Statement(StatementNode::PrintFormat {
        format: content,
        args,
    }))
}

fn skip_whitespace(tokens: &mut Peekable<Iter<Token>>) {
    while let Some(token) = tokens.peek() {
        if token.token_type == TokenType::Whitespace {
            tokens.next();
        } else {
            break;
        }
    }
}

fn parse_if(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
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

        if let Some(Token { token_type: TokenType::If, .. }) = tokens.peek() {
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
fn parse_for(_tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
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
fn parse_while(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
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

fn parse_import(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    if tokens.peek()?.token_type != TokenType::Lparen {
        println!("Error: Expected '(' after 'import'");
        return None;
    }
    tokens.next();

    let import_path = match tokens.next() {
        Some(Token { token_type: TokenType::String(s), .. }) => s.clone(),
        other => {
            println!("Error: Expected string literal in import, found {:?}", other);
            return None;
        }
    };

    if tokens.peek()?.token_type != TokenType::Rparen {
        println!("Error: Expected ')' after 'import' condition");
        return None;
    }
    tokens.next();

    if tokens.peek()?.token_type != TokenType::SemiColon {
        println!("Error: Expected ';' after 'import' condition");
        return None;
    }
    tokens.next();

    Some(ASTNode::Statement(StatementNode::Import(import_path)))
}

fn parse_proto(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    let proto_name = match tokens.next() {
        Some(Token { token_type: TokenType::Identifier(name), .. }) => name.clone(),
        other => {
            println!("Error: Expected identifier after 'proto', found {:?}", other);
            return None;
        }
    };

    if tokens.peek()?.token_type != TokenType::Lbrace {
        println!("Error: Expected '{{' after proto name '{}'", proto_name);
        return None;
    }
    tokens.next(); // consume '{'

    let mut methods = Vec::new();

    while let Some(token) = tokens.peek() {
        match token.token_type {
            TokenType::Rbrace => {
                tokens.next(); // consume '}'
                break;
            }

            TokenType::Fun => {
                tokens.next(); // consume 'fun'

                let method_name = match tokens.next() {
                    Some(Token { token_type: TokenType::Identifier(name), .. }) => name.clone(),
                    other => {
                        println!("Error: Expected function name in proto, found {:?}", other);
                        return None;
                    }
                };

                if tokens.peek()?.token_type != TokenType::Lparen {
                    println!("Error: Expected '(' after proto method name '{}'", method_name);
                    return None;
                }
                tokens.next(); // consume '('

                let mut params = Vec::new();

                while let Some(param_token) = tokens.peek() {
                    match param_token.token_type {
                        TokenType::Rparen => {
                            tokens.next(); // consume ')'
                            break;
                        }

                        TokenType::Identifier(ref param_name) => {
                            let name = param_name.clone();
                            tokens.next(); // consume name

                            if tokens.peek()?.token_type != TokenType::Colon {
                                println!("Error: Expected ':' after param name '{}'", name);
                                return None;
                            }
                            tokens.next(); // consume ':'

                            let type_token = tokens.next();
                            let wave_type = parse_type_from_token(type_token)?;
                            params.push((name, wave_type));

                            // ',' or ')'
                            if tokens.peek()?.token_type == TokenType::Comma {
                                tokens.next(); // consume ','
                            }
                        }

                        _ => {
                            println!("Error: Unexpected token in proto param list: {:?}", param_token);
                            return None;
                        }
                    }
                }

                if tokens.peek()?.token_type != TokenType::Arrow {
                    println!("Error: Expected '->' in proto method '{}'", method_name);
                    return None;
                }
                tokens.next(); // consume '->'

                let return_token = tokens.next();
                let return_type = parse_type_from_token(return_token)?;

                if tokens.peek()?.token_type != TokenType::SemiColon {
                    println!("Error: Expected ';' after proto method signature '{}'", method_name);
                    return None;
                }
                tokens.next(); // consume ';'

                methods.push(FunctionSignature {
                    name: method_name,
                    params,
                    return_type,
                });
            }

            _ => {
                println!("Error: Unexpected token in proto body: {:?}", token);
                return None;
            }
        }
    }

    Some(ASTNode::Proto(ProtoNode {
        name: proto_name,
        methods,
    }))
}

fn parse_struct(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    let name = match tokens.next() {
        Some(Token { token_type: TokenType::Identifier(name), .. }) => name.clone(),
        _ => {
            println!("Error: Expected struct name after 'struct' keyword.");
            return None;
        }
    };

    if tokens.peek().map_or(true, |t| t.token_type != TokenType::Lbrace) {
        println!("Error: Expected '{{' after struct name '{}'.", name);
        return None;
    }
    tokens.next();

    let mut fields = Vec::new();
    let mut methods = Vec::new();

    loop {
        let token_type = if let Some(t) = tokens.peek() {
            t.token_type.clone()
        } else {
            println!("Error: Unexpected end of file inside struct '{}' definition.", name);
            return None;
        };

        match token_type {
            TokenType::Rbrace => {
                tokens.next();
                break;
            }
            TokenType::Fun => {
                if let Some(ASTNode::Function(func_node)) = parse_function(tokens) {
                    if func_node.return_type.is_none() {
                        let mut func_node_with_return = func_node.clone();
                        func_node_with_return.return_type = Some(WaveType::Void);
                        methods.push(func_node_with_return);
                    } else {
                        methods.push(func_node);
                    }
                } else {
                    println!("Error: Failed to parse method inside struct '{}'.", name);
                    return None;
                }
            }
            TokenType::Identifier(_) => {
                let mut lookahead = tokens.clone();
                lookahead.next();

                if let Some(Token { token_type: TokenType::Colon, .. }) = lookahead.peek() {
                    let field_name = if let Some(Token { token_type: TokenType::Identifier(n), .. }) = tokens.next() {
                        n.clone()
                    } else { unreachable!() };

                    tokens.next();

                    let type_token = tokens.next();
                    let wave_type = parse_type_from_token(type_token).or_else(|| {
                        if let Some(Token { token_type: TokenType::Identifier(id), .. }) = type_token {
                            Some(WaveType::Struct(id.clone()))
                        } else {
                            None
                        }
                    });

                    if wave_type.is_none() {
                        println!("Error: Invalid type for field '{}' in struct '{}'.", field_name, name);
                        return None;
                    }

                    if tokens.peek().map_or(true, |t| t.token_type != TokenType::SemiColon) {
                        println!("Error: Expected ';' after field declaration in struct '{}'.", name);
                        return None;
                    }
                    tokens.next();

                    fields.push((field_name, wave_type.unwrap()));
                } else {
                    let id_str = if let TokenType::Identifier(id) = &tokens.peek().unwrap().token_type {
                        id.clone()
                    } else {
                        "".to_string()
                    };
                    println!("Error: Unexpected identifier '{}' in struct '{}' body. Expected field or method.", id_str, name);
                    return None;
                }
            }
            other_token => {
                println!("Error: Unexpected token inside struct body: {:?}", other_token);
                return None;
            }
        }
    }

    Some(ASTNode::Struct(StructNode {
        name,
        fields,
        methods,
    }))
}


fn parse_asm_block(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    if tokens.peek()?.token_type != TokenType::Lbrace {
        println!("Expected '{{' after 'asm'");
        return None;
    }
    tokens.next();

    let mut instructions = vec![];
    let mut inputs = vec![];
    let mut outputs = vec![];

    while let Some(token) = tokens.next() {
        match &token.token_type {
            TokenType::Rbrace => break,

            TokenType::In | TokenType::Out => {
                let is_input = matches!(token.token_type, TokenType::In);

                if tokens.next().map(|t| t.token_type.clone()) != Some(TokenType::Lparen) {
                    println!("Expected '(' after in/out");
                    return None;
                }

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

                if tokens.next().map(|t| t.token_type.clone()) != Some(TokenType::Rparen) {
                    println!("Expected ')' after in/out");
                    return None;
                }

                let value_token = tokens.next();
                let value = match value_token {
                    Some(Token { token_type: TokenType::Minus, .. }) => {
                        match tokens.next() {
                            Some(Token { token_type: TokenType::Number(n), .. }) => format!("-{}", n),
                            Some(other) => {
                                println!("Expected number after '-', got {:?}", other.token_type);
                                return None;
                            }
                            None => {
                                println!("Expected number after '-'");
                                return None;
                            }
                        }
                    }
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
                    inputs.push((reg.clone(), value));
                } else {
                    outputs.push((reg.clone(), value));
                }
            }

            TokenType::String(s) => {
                instructions.push(s.clone());
            }

            other => {
                println!("Unexpected token in asm expression {:?}", other);
            }
        }
    }

    Some(ASTNode::Statement(StatementNode::AsmBlock {
        instructions,
        inputs,
        outputs,
    }))
}

fn parse_assignment(tokens: &mut Peekable<Iter<Token>>, first_token: &Token) -> Option<ASTNode> {
    let left_expr = match parse_expression_from_token(first_token, tokens) {
        Some(expr) => expr,
        None => {
            println!("Error: Failed to parse left-hand side of assignment. Token: {:?}", first_token.token_type);
            return None;
        }
    };

    let assign_op = match tokens.peek()?.token_type {
        TokenType::PlusEq => {
            tokens.next();
            Some(AssignOperator::AddAssign)
        }
        TokenType::MinusEq => {
            tokens.next();
            Some(AssignOperator::SubAssign)
        }
        TokenType::StarEq => {
            tokens.next();
            Some(AssignOperator::MulAssign)
        }
        TokenType::DivEq => {
            tokens.next();
            Some(AssignOperator::DivAssign)
        }
        TokenType::RemainderEq => {
            tokens.next();
            Some(AssignOperator::RemAssign)
        }
        TokenType::Equal => {
            tokens.next();
            None
        }
        _ => return None,
    };

    let right_expr = parse_expression(tokens)?;

    if let Some(Token { token_type: TokenType::SemiColon, .. }) = tokens.peek() {
        tokens.next();
    }

    match (assign_op, &left_expr) {
        (Some(op), Expression::Variable(name)) => Some(ASTNode::Expression(Expression::AssignOperation {
            target: Box::new(Expression::Variable(name.clone())),
            operator: op,
            value: Box::new(right_expr),
        })),
        (None, Expression::Variable(name)) => Some(ASTNode::Statement(StatementNode::Assign {
            variable: name.clone(),
            value: right_expr,
        })),
        (None, Expression::Deref(_)) => Some(ASTNode::Statement(StatementNode::Assign {
            variable: "deref".to_string(),
            value: Expression::BinaryExpression {
                left: Box::new(left_expr),
                operator: Operator::Assign,
                right: Box::new(right_expr),
            },
        })),
        (_, _) => {
            println!("Error: Unsupported assignment left expression: {:?}", left_expr);
            None
        }
    }
}

fn parse_block(tokens: &mut Peekable<Iter<Token>>) -> Option<Vec<ASTNode>> {
    let mut body = vec![];

    while let Some(token) = tokens.peek() {
        if token.token_type == TokenType::Rbrace {
            break;
        }

        if let Some(node) = parse_statement(tokens) {
            body.push(node);
        } else {
            println!("Error: Failed to parse statement inside block.");
            return None;
        }
    }

    if let Some(token) = tokens.next() {
        if token.token_type != TokenType::Rbrace {
            println!("Error: Expected '}}' to close the block, but found {:?}", token.token_type);
            return None;
        }
    } else {
        println!("Error: Unexpected end of file, expected '}}'");
        return None;
    }

    Some(body)
}

fn parse_statement(tokens: &mut Peekable<Iter<Token>>) -> Option<ASTNode> {
    let token = match tokens.peek() {
        Some(t) => (*t).clone(),
        None => return None,
    };

    let node = match token.token_type {
        TokenType::Var => {
            tokens.next();
            parse_var(tokens)
        }
        TokenType::Println => {
            tokens.next();
            parse_println(tokens)
        }
        TokenType::Print => {
            tokens.next();
            parse_print(tokens)
        }
        TokenType::If => {
            tokens.next();
            parse_if(tokens)
        }
        TokenType::For => {
            tokens.next();
            parse_for(tokens)
        }
        TokenType::While => {
            tokens.next();
            parse_while(tokens)
        }
        TokenType::Continue => {
            tokens.next();
            if let Some(Token { token_type: TokenType::SemiColon, .. }) = tokens.peek() {
                tokens.next();
            }
            Some(ASTNode::Statement(StatementNode::Continue))
        }
        TokenType::Break => {
            tokens.next();
            if let Some(Token { token_type: TokenType::SemiColon, .. }) = tokens.peek() {
                tokens.next();
            }
            Some(ASTNode::Statement(StatementNode::Break))
        }
        TokenType::Return => {
            tokens.next();
            let expr = if let Some(Token { token_type: TokenType::SemiColon, .. }) = tokens.peek() {
                tokens.next();
                None
            } else if tokens.peek().is_none() {
                None
            } else {
                let value = parse_expression(tokens)?;
                if let Some(Token { token_type: TokenType::SemiColon, .. }) = tokens.peek() {
                    tokens.next();
                }
                Some(value)
            };
            Some(ASTNode::Statement(StatementNode::Return(expr)))
        }
        TokenType::Rbrace => None,

        _ => {
            if is_expression_start(&token.token_type) {
                if let Some(expr) = parse_expression(tokens) {
                    if let Some(Token { token_type: TokenType::SemiColon, .. }) = tokens.peek() {
                        tokens.next();
                    }
                    Some(ASTNode::Statement(StatementNode::Expression(expr)))
                } else {
                    println!("Error: Failed to parse expression statement.");
                    None
                }
            } else {
                println!("Error: Unexpected token, cannot start a statement with: {:?}", token.token_type);
                tokens.next();
                None
            }
        }
    };

    node
}

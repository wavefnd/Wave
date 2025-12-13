use std::str::FromStr;
use crate::*;

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize) -> Self {
        Token {
            token_type,
            lexeme,
            line,
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Token {
            token_type: TokenType::Eof, // Set default token type to EOF
            lexeme: String::new(),      // The default lexeme is an empty string
            line: 0,                    // Default line number is 0
        }
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    pub source: &'a str,
    pub current: usize,
    pub line: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        Lexer {
            source,
            current: 0,
            line: 1,
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        let rest = &self.source[self.current..];
        let (ch, size) = match std::str::from_utf8(rest.as_ref()) {
            Ok(s) => {
                let mut chars = s.chars();
                if let Some(c) = chars.next() {
                    (c, c.len_utf8())
                } else {
                    ('\0', 1)
                }
            }
            Err(_) => ('\0', 1),
        };

        self.current += size;
        ch
    }

    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            let c = self.peek();
            match c {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                _ => break,
            }
        }
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            let rest = &self.source[self.current..];
            match std::str::from_utf8(rest.as_ref()) {
                Ok(s) => s.chars().next().unwrap_or('\0'),
                Err(_) => '\0',
            }
        }
    }

    fn match_next(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.peek() != expected {
            return false;
        }
        self.advance();
        true
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        loop {
            let token = self.next_token();

            if token.token_type == TokenType::Eof {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }
        tokens
    }

    fn skip_comment(&mut self) {
        while !self.is_at_end() && self.peek() != '\n' {
            self.advance();
        }
    }

    fn skip_multiline_comment(&mut self) {
        while !self.is_at_end() {
            if self.peek() == '*' && self.peek_next() == '/' {
                self.advance();
                self.advance();
                break;
            }

            if self.peek() == '\n' {
                self.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            panic!("Unterminated block comment");
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source.chars().nth(self.current + 1).unwrap_or('\0')
        }
    }

    /*
    pub fn consume(&mut self) {
        if let Some(current_char) = self.source.chars().nth(self.current) {
            if current_char == '\n' {
                self.line += 1;
            }
            println!("Consuming character: {}, at position: {}", current_char, self.current);
            self.current += 1;
        }
    }

    pub fn consume_n(&mut self, n: usize) {
        for _ in 0..n {
            self.consume();
        }
        println!("Consumed {} characters, current position: {}", n, self.current);
    }
     */

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        if self.is_at_end() {
            return Token {
                token_type: TokenType::Eof,
                lexeme: String::new(),
                line: self.line,
            };
        }

        let c = self.advance();

        match c {
            '+' => {
                if self.match_next('+') {
                    Token {
                        token_type: TokenType::Increment,
                        lexeme: "++".to_string(),
                        line: self.line,
                    }
                } else if self.match_next('=') {
                    Token {
                        token_type: TokenType::PlusEq,
                        lexeme: "+=".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::Plus,
                        lexeme: "+".to_string(),
                        line: self.line,
                    }
                }
            },
            '-' => {
                if self.match_next('-') {
                    Token {
                        token_type: TokenType::Decrement,
                        lexeme: "--".to_string(),
                        line: self.line,
                    }
                } else if self.match_next('>') {
                    Token {
                        token_type: TokenType::Arrow,
                        lexeme: "->".to_string(),
                        line: self.line,
                    }
                } else if self.match_next('=') {
                    Token {
                        token_type: TokenType::MinusEq,
                        lexeme: "-=".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::Minus,
                        lexeme: "-".to_string(),
                        line: self.line,
                    }
                }
            },
            '*' => {
                if self.match_next('=') {
                    Token {
                        token_type: TokenType::StarEq,
                        lexeme: "*=".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::Star,
                        lexeme: "*".to_string(),
                        line: self.line,
                    }
                }
            } ,
            '.' => {
                Token {
                    token_type: TokenType::Dot,
                    lexeme: ".".to_string(),
                    line: self.line,
                }
            },
            '/' => {
                if self.match_next('/') {
                    self.skip_comment();
                    self.next_token()
                } else if self.match_next('*') {
                    self.skip_multiline_comment();
                    self.next_token()
                } else if self.match_next('=') {
                    Token {
                        token_type: TokenType::DivEq,
                        lexeme: "/=".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::Div,
                        lexeme: "/".to_string(),
                        line: self.line,
                    }
                }
            },
            '%' => {
                if self.match_next('=') {
                    Token {
                        token_type: TokenType::RemainderEq,
                        lexeme: "%=".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::Remainder,
                        lexeme: "%".to_string(),
                        line: self.line,
                    }
                }
            }
            ';' => {
                Token {
                    token_type: TokenType::SemiColon,
                    lexeme: ";".to_string(),
                    line: self.line,
                }
            },
            ':' => {
                Token {
                    token_type: TokenType::Colon,
                    lexeme: ":".to_string(),
                    line: self.line,
                }
            },
            '<' => {
                if self.match_next('=') {
                    Token {
                        token_type: TokenType::LchevrEq,
                        lexeme: "<=".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::Lchevr,
                        lexeme: "<".to_string(),
                        line: self.line,
                    }
                }

            },
            '>' => {
                if self.match_next('=') {
                    Token {
                        token_type: TokenType::RchevrEq,
                        lexeme: ">=".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::Rchevr,
                        lexeme: ">".to_string(),
                        line: self.line,
                    }
                }

            },
            '(' => {
                Token {
                    token_type: TokenType::Lparen,
                    lexeme: "(".to_string(),
                    line: self.line,
                }
            },
            ')' => {
                Token {
                    token_type: TokenType::Rparen,
                    lexeme: ")".to_string(),
                    line: self.line,
                }
            },
            '{' => {
                Token {
                    token_type: TokenType::Lbrace,
                    lexeme: "{".to_string(),
                    line: self.line,
                }
            },
            '}' => {
                Token {
                    token_type: TokenType::Rbrace,
                    lexeme: "}".to_string(),
                    line: self.line,
                }
            },
            '[' => {
                Token {
                    token_type: TokenType::Lbrack,
                    lexeme: "[".to_string(),
                    line: self.line,
                }
            },
            ']' => {
                Token {
                    token_type: TokenType::Rbrack,
                    lexeme: "]".to_string(),
                    line: self.line,
                }
            },
            '=' => {
                if self.match_next('=') {
                    Token {
                        token_type: TokenType::EqualTwo,
                        lexeme: "==".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::Equal,
                        lexeme: "=".to_string(),
                        line: self.line,
                    }
                }
            },
            '&' => {
                if self.match_next('&') {
                    Token {
                        token_type: TokenType::LogicalAnd,
                        lexeme: "&&".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::AddressOf,
                        lexeme: "&".to_string(),
                        line: self.line,
                    }
                }
            },
            '|' => {
                if self.match_next('|') {
                    Token {
                        token_type: TokenType::LogicalOr,
                        lexeme: "||".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::BitwiseOr,
                        lexeme: "|".to_string(),
                        line: self.line,
                    }
                }
            },
            '!' => {
                if self.match_next('=') {
                    Token {
                        token_type: TokenType::NotEqual,
                        lexeme: "!=".to_string(),
                        line: self.line,
                    }
                } else if self.match_next('&') {
                    Token {
                        token_type: TokenType::Nand,
                        lexeme: "!&".to_string(),
                        line: self.line,
                    }
                } else if self.match_next('|') {
                    Token {
                        token_type: TokenType::Nor,
                        lexeme: "!|".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::Not,
                        lexeme: "!".to_string(),
                        line: self.line,
                    }
                }
            },
            '^' => {
                Token {
                    token_type: TokenType::Xor,
                    lexeme: "^".to_string(),
                    line: self.line,
                }
            },
            '~' => {
                if self.match_next('^') {
                    Token {
                        token_type: TokenType::Xnor,
                        lexeme: "~^".to_string(),
                        line: self.line,
                    }
                } else {
                    Token {
                        token_type: TokenType::BitwiseNot,
                        lexeme: "~".to_string(),
                        line: self.line,
                    }
                }
            },
            '?' => {
                if self.match_next('?') {
                    Token {
                        token_type: TokenType::NullCoalesce,
                        lexeme: "??".to_string(),
                        line: self.line,
                    }
                } else {
                     Token {
                        token_type: TokenType::Condition,
                        lexeme: "?".to_string(),
                        line: self.line,
                    }
                }
            },
            ',' => {
                Token {
                    token_type: TokenType::Comma,
                    lexeme: ",".to_string(),
                    line: self.line,
                }
            },
            '"' => {
                let string_value = self.string();
                Token {
                    token_type: TokenType::String(string_value.clone()),
                    lexeme: format!("\"{}\"", string_value),
                    line: self.line,
                }
            },
            'a'..='z' | 'A'..='Z' | '_' => {
                let identifier = self.identifier();
                match identifier.as_str() {
                    "fun" => {
                        Token {
                            token_type: TokenType::Fun,
                            lexeme: "fun".to_string(),
                            line: self.line,
                        }
                    },
                    "var" => {
                        Token {
                            token_type: TokenType::Var,
                            lexeme: "var".to_string(),
                            line: self.line,
                        }
                    },
                    "deref" => {
                        Token {
                            token_type: TokenType::Deref,
                            lexeme: "deref".to_string(),
                            line: self.line,
                        }
                    },
                    "let" => {
                        Token {
                            token_type: TokenType::Let,
                            lexeme: "let".to_string(),
                            line: self.line,
                        }
                    }
                    "mut" => {
                        Token {
                            token_type: TokenType::Mut,
                            lexeme: "mut".to_string(),
                            line: self.line,
                        }
                    }
                    "const" => {
                        Token {
                            token_type: TokenType::Const,
                            lexeme: "const".to_string(),
                            line: self.line,
                        }
                    },
                    "if" => {
                        Token {
                            token_type: TokenType::If,
                            lexeme: "if".to_string(),
                            line: self.line,
                        }
                    },
                    "else" => {
                        Token {
                            token_type: TokenType::Else,
                            lexeme: "else".to_string(),
                            line: self.line,
                        }
                    },
                    "proto" => {
                        Token {
                            token_type: TokenType::Proto,
                            lexeme: "proto".to_string(),
                            line: self.line,
                        }
                    },
                    "struct" => {
                        Token {
                            token_type: TokenType::Struct,
                            lexeme: "struct".to_string(),
                            line: self.line,
                        }
                    },
                    "while" => {
                        Token {
                            token_type: TokenType::While,
                            lexeme: "while".to_string(),
                            line: self.line,
                        }
                    },
                    "for" => {
                        Token {
                            token_type: TokenType::For,
                            lexeme: "for".to_string(),
                            line: self.line,
                        }
                    },
                    "module" => {
                        Token {
                            token_type: TokenType::Module,
                            lexeme: "module".to_string(),
                            line: self.line,
                        }
                    },
                    "class" => {
                        Token {
                            token_type: TokenType::Class,
                            lexeme: "class".to_string(),
                            line: self.line,
                        }
                    },
                    "in" => {
                        Token {
                            token_type: TokenType::In,
                            lexeme: "in".to_string(),
                            line: self.line,
                        }
                    },
                    "out" => {
                        Token {
                            token_type: TokenType::Out,
                            lexeme: "out".to_string(),
                            line: self.line,
                        }
                    },
                    "is" => {
                        Token {
                            token_type: TokenType::Is,
                            lexeme: "is".to_string(),
                            line: self.line,
                        }
                    },
                    "asm" => {
                        Token {
                            token_type: TokenType::Asm,
                            lexeme: "asm".to_string(),
                            line: self.line,
                        }
                    },
                    "rol" => {
                        Token {
                            token_type: TokenType::Rol,
                            lexeme: "rol".to_string(),
                            line: self.line,
                        }
                    },
                    "ror" => {
                        Token {
                            token_type: TokenType::Ror,
                            lexeme: "ror".to_string(),
                            line: self.line,
                        }
                    },
                    "xnand" => {
                        Token {
                            token_type: TokenType::Xnand,
                            lexeme: "xnand".to_string(),
                            line: self.line,
                        }
                    },
                    "import" => {
                        Token {
                            token_type: TokenType::Import,
                            lexeme: "import".to_string(),
                            line: self.line,
                        }
                    },
                    "return" => {
                        Token {
                            token_type: TokenType::Return,
                            lexeme: "return".to_string(),
                            line: self.line,
                        }
                    },
                    "continue" => {
                        Token {
                            token_type: TokenType::Continue,
                            lexeme: "continue".to_string(),
                            line: self.line,
                        }
                    },
                    "print" => {
                        Token {
                            token_type: TokenType::Print,
                            lexeme: "print".to_string(),
                            line: self.line,
                        }
                    },
                    "input" => {
                        Token {
                            token_type: TokenType::Input,
                            lexeme: "input".to_string(),
                            line: self.line,
                        }
                    },
                    "println" => {
                        Token {
                            token_type: TokenType::Println,
                            lexeme: "println".to_string(),
                            line: self.line,
                        }
                    },
                    "match" => {
                        Token {
                            token_type: TokenType::Match,
                            lexeme: "match".to_string(),
                            line: self.line,
                        }
                    },
                    "char" => {
                        Token {
                            token_type: TokenType::TypeChar,
                            lexeme: "char".to_string(),
                            line: self.line,
                        }
                    },
                    "byte" => {
                        Token {
                            token_type: TokenType::TypeByte,
                            lexeme: "byte".to_string(),
                            line: self.line,
                        }
                    },
                    "ptr" => {
                        Token {
                            token_type: TokenType::Identifier("ptr".to_string()),
                            lexeme: "ptr".to_string(),
                            line: self.line,
                        }
                    },
                    "array" => {
                        Token {
                            token_type: TokenType::Identifier("array".to_string()),
                            lexeme: "array".to_string(),
                            line: self.line,
                        }
                    },
                    "isz" => {
                        Token {
                            token_type: TokenType::TokenTypeInt(IntegerType::ISZ),
                            lexeme: "isz".to_string(),
                            line: self.line,
                        }
                    },
                    "i8" => {
                        Token {
                            token_type: TokenType::TokenTypeInt(IntegerType::I8),
                            lexeme: "i8".to_string(),
                            line: self.line,
                        }
                    },
                    "i16" => {
                        Token {
                            token_type: TokenType::TokenTypeInt(IntegerType::I16),
                            lexeme: "i16".to_string(),
                            line: self.line,
                        }
                    },
                    "i32" => {
                        Token {
                            token_type: TokenType::TokenTypeInt(IntegerType::I32),
                            lexeme: "i32".to_string(),
                            line: self.line,
                        }
                    },
                    "i64" => {
                        Token {
                            token_type: TokenType::TokenTypeInt(IntegerType::I64),
                            lexeme: "i64".to_string(),
                            line: self.line,
                        }
                    },
                    "i128" => {
                        Token {
                            token_type: TokenType::TokenTypeInt(IntegerType::I128),
                            lexeme: "i128".to_string(),
                            line: self.line,
                        }
                    },
                    "i256" => {
                        Token {
                            token_type: TokenType::TokenTypeInt(IntegerType::I256),
                            lexeme: "i256".to_string(),
                            line: self.line,
                        }
                    },
                    "i512" => {
                       Token {
                            token_type: TokenType::TokenTypeInt(IntegerType::I512),
                            lexeme: "i512".to_string(),
                            line: self.line,
                        }
                    },
                    "i1024" => {
                        Token {
                            token_type: TokenType::TokenTypeInt(IntegerType::I1024),
                            lexeme: "i1024".to_string(),
                            line: self.line,
                        }
                    },
                    "usz" => Token {
                        token_type: TokenType::TokenTypeUint(UnsignedIntegerType::USZ),
                        lexeme: "usz".to_string(),
                        line: self.line,
                    },
                    "u8" => Token {
                        token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U8),
                        lexeme: "u8".to_string(),
                        line: self.line,
                    },
                    "u16" => Token {
                        token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U16),
                        lexeme: "u16".to_string(),
                        line: self.line,
                    },
                    "u32" => Token {
                        token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U32),
                        lexeme: "u32".to_string(),
                        line: self.line,
                    },
                    "u64" => Token {
                        token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U64),
                        lexeme: "u64".to_string(),
                        line: self.line,
                    },
                    "u128" => Token {
                        token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U128),
                        lexeme: "u128".to_string(),
                        line: self.line,
                    },
                    "u256" => Token {
                        token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U256),
                        lexeme: "u256".to_string(),
                        line: self.line,
                    },
                    "u512" => Token {
                        token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U512),
                        lexeme: "u512".to_string(),
                        line: self.line,
                    },
                    "u1024" => Token {
                        token_type: TokenType::TokenTypeUint(UnsignedIntegerType::U1024),
                        lexeme: "u1024".to_string(),
                        line: self.line,
                    },
                    "f32" => Token {
                        token_type: TokenType::TokenTypeFloat(FloatType::F32),
                        lexeme: "f32".to_string(),
                        line: self.line,
                    },
                    "f64" => Token {
                        token_type: TokenType::TokenTypeFloat(FloatType::F64),
                        lexeme: "f64".to_string(),
                        line: self.line,
                    },
                    "str" => {
                        Token {
                            token_type: TokenType::TypeString,
                            lexeme: "str".to_string(),
                            line: self.line,
                        }
                    },
                    "break" => {
                        Token {
                            token_type: TokenType::Break,
                            lexeme: "break".to_string(),
                            line: self.line,
                        }
                    },
                    _ => {
                        Token {
                            token_type: TokenType::Identifier(identifier.clone()),
                            lexeme: identifier,
                            line: self.line,
                        }
                    }
                }
            },
            '0'..='9' => {
                let mut num_str = self.number().to_string(); // 첫 숫자만 읽음

                // 16진수 접두사 체크
                if num_str == "0" && (self.peek() == 'x' || self.peek() == 'X') {
                    num_str.push(self.advance()); // 'x' 붙이기

                    while self.peek().is_digit(16) {
                        num_str.push(self.advance());
                    }

                    let value = i64::from_str_radix(&num_str[2..], 16).unwrap_or(0);
                    return Token {
                        token_type: TokenType::Number(value),
                        lexeme: num_str,
                        line: self.line,
                    };
                }

                // float 처리
                let is_float = if self.peek() == '.' {
                    num_str.push('.');
                    self.advance();

                    while self.peek().is_digit(10) {
                        num_str.push(self.advance()); // 소수점 뒤 숫자
                    }
                    true
                } else {
                    false
                };

                // 일반 숫자/실수 토큰 결정
                let token_type = if is_float {
                    num_str.parse::<f64>().map(TokenType::Float).unwrap_or(TokenType::Float(0.0))
                } else {
                    num_str.parse::<i64>().map(TokenType::Number).unwrap_or(TokenType::Number(0))
                };

                Token {
                    token_type,
                    lexeme: num_str,
                    line: self.line,
                }
            }
            _ => {
                if c == '\0' {
                    eprintln!("[eprintln] Null character encountered — likely unintended");
                    panic!("[panic] Null character (`\\0`) is not allowed in source");
                } else if c == '\\' {
                    eprintln!("[eprintln] Unexpected backslash outside of string");
                    panic!("[panic] Unexpected character: '\\' outside of string");
                } else {
                    eprintln!("[eprintln] Unexpected character: {:?} (code: {})", c, c as u32);
                    panic!("[panic] Unexpected character: {:?}", c);
                }
            }
        }
    }

    // Helper methods to create tokens
    fn create_int_token(&self, int_type: IntegerType, lexeme: String) -> Token {
        Token {
            token_type: TokenType::TokenTypeInt(int_type),
            lexeme,
            line: self.line,
        }
    }

    fn create_float_token(&self, float_type: FloatType, lexeme: String) -> Token {
        Token {
            token_type: TokenType::TokenTypeFloat(float_type),
            lexeme,
            line: self.line,
        }
    }

    fn create_identifier_token(&self, identifier: String) -> Token {
        Token {
            token_type: TokenType::Identifier(identifier.clone()),
            lexeme: identifier,
            line: self.line,
        }
    }
    
    // Add string literal processing function
    fn string(&mut self) -> String {
        if self.peek() == '"' {
            self.advance();
        }

        let mut string_literal = String::new();

        while !self.is_at_end() && self.peek() != '"' {
            let c = self.advance();

            if c == '\\' {
                let next = self.advance();
                match next {
                    'n' => string_literal.push('\n'),
                    't' => string_literal.push('\t'),
                    'r' => string_literal.push('\r'),
                    '\\' => string_literal.push('\\'),
                    '"' => string_literal.push('"'),
                    _ => {
                        string_literal.push('\\');
                        string_literal.push(next);
                    }
                }
            } else {
                string_literal.push(c);
            }
        }

        if self.is_at_end() {
            panic!("Unterminated string.");
        }

        self.advance(); // closing quote

        string_literal
    }

    fn identifier(&mut self) -> String {
        let start = if self.current > 0 {
            self.current - 1
        } else {
            0
        };

        while !self.is_at_end() {
            let c = self.peek();
            if c.is_alphabetic() || c.is_numeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        self.source[start..self.current].to_string()
    }

    fn number(&mut self) -> i64 {
        let start = self.current - 1;
        while !self.is_at_end() && self.peek().is_numeric() {
            self.advance();
        }

        let number_str = &self.source[start..self.current];
        i64::from_str(number_str).unwrap_or_else(|_| 0)
    }
}

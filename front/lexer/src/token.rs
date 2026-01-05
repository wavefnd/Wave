use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum IntegerType {
    I8,
    I16,
    I32,
    I64,
    I128,
    I256,
    I512,
    I1024,
    ISZ,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum UnsignedIntegerType {
    U8,
    U16,
    U32,
    U64,
    U128,
    U256,
    U512,
    U1024,
    USZ,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum FloatType {
    F32,
    F64,
}

impl fmt::Display for IntegerType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            IntegerType::I8 => "i8",
            IntegerType::I16 => "i16",
            IntegerType::I32 => "i32",
            IntegerType::I64 => "i64",
            IntegerType::I128 => "i128",
            IntegerType::I256 => "i256",
            IntegerType::I512 => "i512",
            IntegerType::I1024 => "i1024",
            IntegerType::ISZ => "isz",
        };
        write!(f, "{}", name)
    }
}

impl fmt::Display for UnsignedIntegerType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            UnsignedIntegerType::U8 => "u8",
            UnsignedIntegerType::U16 => "u16",
            UnsignedIntegerType::U32 => "u32",
            UnsignedIntegerType::U64 => "u64",
            UnsignedIntegerType::U128 => "u128",
            UnsignedIntegerType::U256 => "u256",
            UnsignedIntegerType::U512 => "u512",
            UnsignedIntegerType::U1024 => "u1024",
            UnsignedIntegerType::USZ => "usz",
        };
        write!(f, "{}", name)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Fun,
    Var,
    Let,
    Mut,
    Deref,
    Const,
    If,
    Else,
    While,
    For,
    Import,
    Return,
    Continue,
    Input,
    Print,
    Println,
    Module,
    Class,
    Match,
    LogicalAnd,   // &&
    AddressOf,    // &
    LogicalOr,    // ||
    BitwiseOr,    // |
    NotEqual,     // !=
    Xor,          // ^
    Xnor,         // ~^
    BitwiseNot,   // ~
    Nand,         // !&
    Nor,          // !|
    Not,          // !
    Condition,    // ?
    NullCoalesce, // ??
    Conditional,  // ?:
    In,           // in
    Out,          // out
    Is,           // is
    Asm,
    Rol,
    Ror,
    Xnand,
    Operator(String),
    TokenTypeInt(IntegerType),
    TokenTypeUint(UnsignedIntegerType),
    TokenTypeFloat(FloatType),
    TypeInt(u16),
    TypeUint(u16),
    TypeFloat(u16),
    TypeBool,
    TypeChar,
    TypeByte,
    TypeString,
    TypeCustom(String),
    TypePointer(Box<TokenType>),
    TypeArray(Box<TokenType>, u32),
    Identifier(String),
    String(String),
    Number(i64),
    Float(f64),
    Plus,        // +
    Increment,   // ++
    PlusEq,      // +=
    Minus,       // -
    Decrement,   // --
    MinusEq,     // -=
    Star,        // *
    StarEq,      // *=
    Div,         // /
    DivEq,       // /=
    Remainder,   // %
    RemainderEq, // %=
    Equal,       // =
    EqualTwo,    // ==
    Comma,       // ,
    Dot,         // .
    SemiColon,   // ;
    Colon,       // :
    Lchevr,      // <
    LchevrEq,    // <=
    Rchevr,      // >
    RchevrEq,    // >=
    Lparen,      // (
    Rparen,      // )
    Lbrace,      // {
    Rbrace,      // }
    Lbrack,      // [
    Rbrack,      // ]
    Eof,         // End of file
    Error,
    Whitespace,
    Break,
    Arrow, // ->
    Array,
    Newline,
    Proto,
    Struct,
    TypeVoid,
    CharLiteral(char),
    BoolLiteral(bool),
}

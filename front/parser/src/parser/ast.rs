#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Text(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WaveType {
    Int(u16),
    Uint(u16),
    Float(u16),
    Bool,
    Char,
    Byte,
    String,
    Pointer(Box<WaveType>),
    Array(Box<WaveType>, u32),
    Void,
    Struct(String),
    Proto(String),
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    Function(FunctionNode),
    Program(ParameterNode),
    Statement(StatementNode),
    Variable(VariableNode),
    Expression(Expression),
    Struct(StructNode),
    Proto(ProtoNode),
}

#[derive(Debug, Clone)]
pub struct FunctionNode {
    pub name: String,
    pub parameters: Vec<ParameterNode>,
    pub return_type: Option<WaveType>,
    pub body: Vec<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct StructNode {
    pub name: String,
    pub methods: Vec<FunctionSignature>,
}

#[derive(Debug, Clone)]
pub struct ProtoNode {
    pub name: String,
    pub methods: Vec<FunctionSignature>,
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub params: Vec<(String, WaveType)>,
    pub return_type: WaveType,
}

#[derive(Debug, Clone)]
pub struct ParameterNode {
    pub name: String,
    pub param_type: WaveType,
    pub initial_value: Option<Value>,
}

#[derive(Debug, Clone)]
pub enum FormatPart {
    Literal(String),
    Placeholder,
}

#[derive(Debug, Clone)]
pub enum Expression {
    FunctionCall {
        name: String,
        args: Vec<Expression>,
    },
    MethodCall {
        object: Box<Expression>,
        name: String,
        args: Vec<Expression>,
    },
    Literal(Literal),
    Variable(String),
    Deref(Box<Expression>),
    AddressOf(Box<Expression>),
    BinaryExpression {
        left: Box<Expression>,
        operator: Operator,
        right: Box<Expression>,
    },
    IndexAccess {
        target: Box<Expression>,
        index: Box<Expression>,
    },
    ArrayLiteral(Vec<Expression>),
    Grouped(Box<Expression>),
    AssignOperation {
        target: Box<Expression>,
        operator: AssignOperator,
        value: Box<Expression>,
    },
    Assignment {
        target: Box<Expression>,
        value: Box<Expression>,
    },
    AsmBlock {
        instructions: Vec<String>,
        inputs: Vec<(String, String)>,
        outputs: Vec<(String, String)>,
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(i64),
    Float(f64),
    String(String),
}

#[derive(Debug, Clone)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    GreaterEqual,
    LessEqual,
    Greater,
    Less,
    Equal,
    NotEqual,
    LogicalAnd,
    BitwiseAnd,
    LogicalOr,
    BitwiseOr,
    Assign,
}

#[derive(Debug, Clone)]
pub enum AssignOperator {
    Assign,      // =
    AddAssign,   // +=
    SubAssign,   // -=
    MulAssign,  // *=
    DivAssign,  // /=
    RemAssign,  // %=
}

#[derive(Debug, Clone)]
pub enum StatementNode {
    Print(String),
    PrintFormat {
        format: String,
        args: Vec<Expression>,
    },
    Println(String),
    PrintlnFormat {
        format: String,
        args: Vec<Expression>,
    },
    Variable(String),
    If {
        condition: Expression,
        body: Vec<ASTNode>,
        else_if_blocks: Option<Box<Vec<(Expression, Vec<ASTNode>)>>>,
        else_block: Option<Box<Vec<ASTNode>>>,
    },
    For {
        initialization: Expression,
        condition: Expression,
        increment: Expression,
        body: Vec<ASTNode>,
    },
    While {
        condition: Expression,
        body: Vec<ASTNode>,
    },
    Import(String),
    Assign {
        variable: String,
        value: Expression,
    },
    AsmBlock {
        instructions: Vec<String>,
        inputs: Vec<(String, String)>,
        outputs: Vec<(String, String)>,
    },
    Break,
    Continue,
    Return(Option<Expression>),
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Mutability {
    Var,
    Let,
    LetMut,
    Const,
}

#[derive(Debug, Clone)]
pub struct VariableNode {
    pub name: String,
    pub type_name: WaveType,
    pub initial_value: Option<Expression>,
    pub mutability: Mutability,
}
use std::collections::BTreeMap;
use std::fmt::Display;
use std::fmt::Formatter;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Void,
    Int,
    Float,
    String,
    Boolean,
    Array(Box<Type>),
    Struct(String),
    Song(String),
    Class(String),
    Variable(Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Type::Void => write!(f, "void"),
            Type::Int => write!(f, "note"),
            Type::Float => write!(f, "pitch"),
            Type::String => write!(f, "lyric"),
            Type::Boolean => write!(f, "beat"),
            Type::Array(_) => write!(f, "track"),
            Type::Struct(_) => write!(f, "harmony"),
            Type::Song(_) => write!(f, "song"),
            Type::Class(_) => todo!(),
            Type::Variable(_) => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: String,
    pub typed: Type,
    pub initializer: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub is_method: bool,
    pub class: String,
    pub name: String,
    pub parameters: Vec<(String, Type)>,
    pub return_type: Type,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Integer(i64),
    Number(f64),
    String(String),
    Boolean(bool),
    Class(String),
    Variable(String, Type),
    Binary {
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Array(Vec<Expression>),
    Struct(String, BTreeMap<String, Expression>),
    Field {
        target: Box<Expression>,
        name: String,
        typed: Type,
    },
    Method {
        target: Box<Expression>,
        name: String,
    },
    Call {
        target: Box<Expression>,
        name: String,
        arguments: Vec<Expression>,
        typed: Type,
    },
    Play(String),
    Unary {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
    Harmonic,   // ~
    Dissonance, // #
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    VariableDeclaration(Variable),
    ForLoop {
        variable: String,
        iterator: Expression,
        body: Vec<Statement>,
    },
    CountdownLoop {
        variable: String,
        count: Expression,
        body: Vec<Statement>,
    },
    Expression(Expression),
    Sing(Vec<(Type, Expression)>),
    Echo(Vec<(Type, Expression)>),
    Return(Option<Expression>),
    WhileLoop {
        condition: Expression,
        body: Vec<Statement>,
    },
    ForLoopOnStruct {
        struct_name: String,
        variable: String,
        target: Expression,
        bodies: Vec<Vec<Statement>>,
    },
    Conditional {
        condition: Expression,
        then_branch: Vec<Statement>,
        else_ifs: Vec<(Expression, Vec<Statement>)>,
        else_branch: Option<Vec<Statement>>,
    },
    RepeatLoop {
        count: Expression,
        body: Vec<Statement>,
    },
    Assignment {
        target: Expression,
        value: Expression,
    },
    TuneStatement {
        variable: Expression,
        direction: TuneDirection,
        amount: Option<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum TuneDirection {
    Up,
    Down,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Not,
    Minus,
    Plus,
}

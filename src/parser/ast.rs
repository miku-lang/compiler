#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}

impl Default for Span {
    fn default() -> Self {
        Self {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub imports: Vec<RemixStatement>,
    pub song: SongDeclaration,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RemixStatement {
    pub import_type: RemixType,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SongDeclaration {
    pub name: String,
    pub parent: Option<String>,
    pub statements: Vec<SongStatementKind>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SongStatementKind {
    MelodyDeclaration {
        verse: bool,
        name: String,
        parameters: Vec<Parameter>,
        return_type: MikuType,
        body: Vec<Statement>,
    },
    VariableDeclaration {
        name: String,
        miku_type: Option<MikuType>,
        initializer: Option<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    VariableDeclaration {
        name: String,
        miku_type: Option<MikuType>,
        initializer: Option<Expression>,
    },
    Assignment {
        target: Expression,
        value: Expression,
    },
    RepeatLoop {
        count: Expression,
        body: Vec<Statement>,
    },
    CountdownLoop {
        variable: String,
        count: Expression,
        body: Vec<Statement>,
    },
    WhileLoop {
        condition: Expression,
        body: Vec<Statement>,
    },
    ForLoop {
        variable: String,
        iterable: Expression,
        body: Vec<Statement>,
    },
    Conditional {
        condition: Expression,
        then_branch: Vec<Statement>,
        else_ifs: Vec<(Expression, Vec<Statement>)>,
        else_branch: Option<Vec<Statement>>,
    },
    SingStatement {
        expressions: Vec<Expression>,
    },
    EchoStatement {
        expressions: Vec<Expression>,
    },
    TuneStatement {
        variable: Expression,
        direction: TuneDirection,
        amount: Option<Expression>,
    },
    ReturnStatement {
        value: Option<Expression>,
    },
    ExpressionStatement {
        expression: Expression,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum RemixType {
    Module {
        module: String,
        alias: Option<String>,
    },
    Selective {
        items: Vec<ImportItem>,
        module: String,
    },
    #[allow(unused)]
    Wildcard { module: String },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportItem {
    pub name: String,
    pub alias: Option<String>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TuneDirection {
    Up,
    Down,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub miku_type: MikuType,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum MikuType {
    Note,
    Pitch,
    Lyric,
    Beat,
    Track,
    Harmony,
    Void,
    Identifier(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    Integer(i64),
    Number(f64),
    String(String),
    Boolean(bool),
    Identifier(String),
    Binary {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    Unary {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Index {
        object: Box<Expression>,
        index: Box<Expression>,
    },
    Member {
        object: Box<Expression>,
        member: String,
    },
    Play(String),
    Array(Vec<Expression>),
    Harmony(Vec<(HarmonyKey, Expression)>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum HarmonyKey {
    Identifier(String),
    String(String),
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
    Harmonic,
    Dissonance,
}

impl BinaryOperator {
    pub fn from_str(string: &str) -> BinaryOperator {
        match string {
            "+" => BinaryOperator::Add,
            "-" => BinaryOperator::Subtract,
            "*" => BinaryOperator::Multiply,
            "/" => BinaryOperator::Divide,
            "%" => BinaryOperator::Modulo,
            "==" => BinaryOperator::Equal,
            "!=" => BinaryOperator::NotEqual,
            "<" => BinaryOperator::Less,
            "<=" => BinaryOperator::LessEqual,
            ">" => BinaryOperator::Greater,
            ">=" => BinaryOperator::GreaterEqual,
            "&&" => BinaryOperator::And,
            "||" => BinaryOperator::Or,
            "~" => BinaryOperator::Harmonic,
            "#" => BinaryOperator::Dissonance,
            _ => panic!("Unknown binary operator: {string}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Not,
    Minus,
    Plus,
}

impl Statement {
    pub fn new(kind: StatementKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl Expression {
    pub fn new(kind: ExpressionKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn integer(value: i64) -> Self {
        Self::new(ExpressionKind::Integer(value), Span::default())
    }
    pub fn number(value: f64) -> Self {
        Self::new(ExpressionKind::Number(value), Span::default())
    }

    pub fn string(value: String) -> Self {
        Self::new(ExpressionKind::String(value), Span::default())
    }

    pub fn boolean(value: bool) -> Self {
        Self::new(ExpressionKind::Boolean(value), Span::default())
    }

    pub fn identifier(name: String) -> Self {
        Self::new(ExpressionKind::Identifier(name), Span::default())
    }

    pub fn unary(operator: UnaryOperator, expression: Expression) -> Self {
        Self::new(
            ExpressionKind::Unary {
                operator,
                operand: Box::new(expression),
            },
            Span::default(),
        )
    }

    pub fn binary(left: Expression, op: BinaryOperator, right: Expression) -> Self {
        Self::new(
            ExpressionKind::Binary {
                left: Box::new(left),
                operator: op,
                right: Box::new(right),
            },
            Span::default(),
        )
    }

    pub fn call(callee: Expression, args: Vec<Expression>) -> Self {
        Self::new(
            ExpressionKind::Call {
                callee: Box::new(callee),
                arguments: args,
            },
            Span::default(),
        )
    }

    pub fn member(object: Expression, name: String) -> Self {
        Self::new(
            ExpressionKind::Member {
                object: Box::new(object),
                member: name,
            },
            Span::default(),
        )
    }

    pub fn index(object: Expression, index: Expression) -> Self {
        Self::new(
            ExpressionKind::Index {
                object: Box::new(object),
                index: Box::new(index),
            },
            Span::default(),
        )
    }
}

impl std::fmt::Display for MikuType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MikuType::Note => write!(f, "note"),
            MikuType::Pitch => write!(f, "pitch"),
            MikuType::Lyric => write!(f, "lyric"),
            MikuType::Beat => write!(f, "beat"),
            MikuType::Track => write!(f, "track"),
            MikuType::Harmony => write!(f, "harmony"),
            MikuType::Void => write!(f, "void"),
            MikuType::Identifier(_) => todo!(),
        }
    }
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "+"),
            BinaryOperator::Subtract => write!(f, "-"),
            BinaryOperator::Multiply => write!(f, "*"),
            BinaryOperator::Divide => write!(f, "/"),
            BinaryOperator::Modulo => write!(f, "%"),
            BinaryOperator::Equal => write!(f, "=="),
            BinaryOperator::NotEqual => write!(f, "!="),
            BinaryOperator::Less => write!(f, "<"),
            BinaryOperator::LessEqual => write!(f, "<="),
            BinaryOperator::Greater => write!(f, ">"),
            BinaryOperator::GreaterEqual => write!(f, ">="),
            BinaryOperator::And => write!(f, "&&"),
            BinaryOperator::Or => write!(f, "||"),
            BinaryOperator::Harmonic => write!(f, "~"),
            BinaryOperator::Dissonance => write!(f, "#"),
        }
    }
}

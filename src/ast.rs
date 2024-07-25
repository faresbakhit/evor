use crate::span::Span;

pub struct Module {
    pub name: Identifier,
    pub functions: Vec<Function>,
    pub span: Span,
}

pub struct Function {
    pub name: Identifier,
    pub inputs: Vec<FunctionParameter>,
    pub output: Path,
    pub body: Vec<Statement>,
    pub span: Span,
}

pub struct FunctionParameter {
    pub name: Identifier,
    pub ty: Path,
}

pub enum Statement {
    Var(Identifier, Path, Expression),
    If(Expression, Vec<Statement>),
    For(Expression, Vec<Statement>),
    Return(Expression),
}

pub enum Expression {
    Binary(BinOp, Box<Expression>, Box<Expression>),
}

pub struct BinOp {
    pub kind: BinOpKind,
    pub span: Span,
}

pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
}

pub struct Path {
    pub segments: Vec<PathSegment>,
    pub span: Span,
}

pub struct PathSegment {
    pub ident: Identifier,
    pub span: Span,
}

pub struct Identifier {
    pub span: Span,
}

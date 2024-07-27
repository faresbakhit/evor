pub use crate::token::Base;
pub use string_interner::symbol::SymbolU32 as Symbol;

use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Mod {
    pub name: Ident,
    pub functions: Vec<Fun>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Fun {
    pub name: Ident,
    pub inputs: Vec<Param>,
    pub output: Ident,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub name: Ident,
    pub ty: Ident,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Stmt {
    pub stmt: StmtKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StmtKind {
    Call(Ident, Vec<Expr>),
    Expr(Expr),
    If(Expr, Block),
    Return(Expr),
    Var(Ident, Expr),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
    Call(Ident, Vec<Expr>),
    Ident(Symbol),
    Infix(InfixOp, Box<Expr>, Box<Expr>),
    Lit(Lit),
    Prefix(PrefixOp, Box<Expr>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct InfixOp {
    pub kind: InfixOpKind,
    pub span: Span,
}

impl InfixOp {
    pub fn binding_power(&self) -> (u8, u8) {
        self.kind.binding_power()
    }

    pub fn from_token(token: &crate::token::Token) -> Option<InfixOp> {
        let op = InfixOp {
            kind: InfixOpKind::from_token_kind(token.kind)?,
            span: token.span,
        };
        Some(op)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum InfixOpKind {
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
    ShiftLeft,
    ShiftRight,
    Equal,
    Less,
    LessEqual,
    NotEqual,
    GreaterEqual,
    Greater,
}

impl InfixOpKind {
    pub fn binding_power(&self) -> (u8, u8) {
        use InfixOpKind::*;

        match self {
            Mul | Div | Rem => (19, 20),
            Add | Sub => (17, 18),
            ShiftLeft | ShiftRight => (15, 16),
            Less | LessEqual | GreaterEqual | Greater => (13, 14),
            Equal | NotEqual => (11, 12),
            BitAnd => (9, 10),
            BitXor => (7, 8),
            BitOr => (5, 6),
            And => (3, 4),
            Or => (1, 2),
        }
    }

    pub fn from_token_kind(kind: crate::token::TokenKind) -> Option<InfixOpKind> {
        use crate::token::TokenKind;
        use InfixOpKind::*;

        let kind = match kind {
            TokenKind::Plus => Add,
            TokenKind::Minus => Sub,
            TokenKind::Star => Mul,
            TokenKind::Slash => Div,
            TokenKind::Percent => Rem,
            TokenKind::AndAnd => And,
            TokenKind::BarBar => Or,
            TokenKind::Caret => BitXor,
            TokenKind::And => BitAnd,
            TokenKind::Bar => BitOr,
            TokenKind::ShiftLeft => ShiftLeft,
            TokenKind::ShiftRight => ShiftRight,
            TokenKind::EqualEqual => Equal,
            TokenKind::Less => Less,
            TokenKind::LessEqual => LessEqual,
            TokenKind::NotEqual => NotEqual,
            TokenKind::GreaterEqual => GreaterEqual,
            TokenKind::Greater => Greater,
            _ => return None,
        };

        Some(kind)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Lit {
    pub kind: LitKind,
    pub sym: Symbol,
    pub span: Span,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum LitKind {
    Char,
    Float,
    Int(Base),
    String,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum PrefixOp {
    Neg,
    Not,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    pub sym: Symbol,
    pub span: Span,
}

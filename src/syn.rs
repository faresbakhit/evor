use std::fmt;

use crate::handle::impl_handle;
use crate::span::Span;
use crate::token::IdentId;
use crate::token::Symbol;
use crate::types::TyId;

pub type Syn = Vec<Item>;

#[derive(Clone, Debug, Hash)]
pub enum Item {
    Struct(Struct),
    Func(Func),
    FuncDecl(FuncDecl),
}

#[derive(Clone, Debug, Hash)]
pub struct Struct {
    pub ident: IdentId,
    pub members: Vec<VarDecl>,
    pub span: Span,
}

#[derive(Clone, Debug, Hash)]
pub struct Func {
    pub decl: FuncDecl,
    pub block: Vec<Stmt>,
    pub span: Span,
}

#[derive(Clone, Debug, Hash)]
pub struct FuncDecl {
    pub ident: IdentId,
    pub params: Vec<VarDecl>,
    pub ret_ty: TyId,
    pub span: Span,
}

#[derive(Copy, Clone, Debug, Hash)]
pub struct VarDecl {
    pub ident: Option<IdentId>,
    pub ty: TyId,
    pub span: Span,
}

#[derive(Clone, Debug, Hash)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Clone, Debug, Hash)]
pub enum StmtKind {
    VarDecl {
        ident: IdentId,
        ty: Option<TyId>,
        init: Option<ExprId>,
    },
    Assign {
        lhs: ExprId,
        rhs: ExprId,
    },
    Expr {
        id: ExprId,
    },
    If {
        cond: ExprId,
        then: Vec<Stmt>,
        otherwise: Vec<Stmt>,
    },
    While {
        cond: ExprId,
        block: Vec<Stmt>,
    },
    Break,
    Continue,
    Return {
        expr: Option<ExprId>,
    },
}

#[derive(Copy, Clone, Debug, Hash)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Copy, Clone, Debug, Hash)]
pub enum ExprKind {
    Un { op: UnOp, expr: ExprId },
    Bin { op: BinOp, lhs: ExprId, rhs: ExprId },
    Call { func: IdentId, args: Span<ExprId> },
    Bool { val: bool },
    Int { val: u32 },
    Str,
    Var { id: IdentId },
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum UnOp {
    Plus,
    Minus,
    BitwiseNot,
    LogicalNot,
    Deref,
    Addr,
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let r = match self {
            UnOp::Plus => "+",
            UnOp::Minus => "-",
            UnOp::BitwiseNot => "~",
            UnOp::LogicalNot => "!",
            UnOp::Deref => "*",
            UnOp::Addr => "&",
        };
        f.write_str(r)
    }
}

impl UnOp {
    pub fn binding_power(&self) -> ((), u8) {
        ((), 21)
    }

    pub fn from_sym(sym: Symbol) -> Option<UnOp> {
        use UnOp::*;
        match sym {
            Symbol::Plus => Some(Plus),
            Symbol::Minus => Some(Minus),
            Symbol::Tilde => Some(BitwiseNot),
            Symbol::Not => Some(LogicalNot),
            Symbol::Star => Some(Deref),
            Symbol::And => Some(Addr),
            _ => None,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseLeftShift,
    BitwiseRightShift,
    LogicalAnd,
    LogicalOr,
    EqualTo,
    NotEqualTo,
    LessThan,
    GreaterThan,
    LessThanOrEqualTo,
    GreaterThanOrEqualTo,
    ArraySubscript,
    MemberAccess,
    IndirectMemberAccess,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let r = match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Rem => "%",
            BinOp::BitwiseAnd => "%",
            BinOp::BitwiseOr => "|",
            BinOp::BitwiseXor => "^",
            BinOp::BitwiseLeftShift => "<<",
            BinOp::BitwiseRightShift => ">>",
            BinOp::LogicalAnd => "&&",
            BinOp::LogicalOr => "||",
            BinOp::EqualTo => "==",
            BinOp::NotEqualTo => "!=",
            BinOp::LessThan => "<",
            BinOp::GreaterThan => ">",
            BinOp::LessThanOrEqualTo => "<=",
            BinOp::GreaterThanOrEqualTo => ">=",
            BinOp::ArraySubscript => "[]",
            BinOp::MemberAccess => ".",
            BinOp::IndirectMemberAccess => "->",
        };
        f.write_str(r)
    }
}

impl BinOp {
    pub fn binding_power(&self) -> (u8, u8) {
        use BinOp::*;

        match self {
            LogicalOr => (1, 2),
            LogicalAnd => (3, 4),
            BitwiseOr => (5, 6),
            BitwiseXor => (7, 8),
            BitwiseAnd => (9, 10),
            EqualTo | NotEqualTo => (11, 12),
            LessThan | GreaterThan | LessThanOrEqualTo | GreaterThanOrEqualTo => (13, 14),
            BitwiseLeftShift | BitwiseRightShift => (15, 16),
            Add | Sub => (17, 18),
            Mul | Div | Rem => (19, 20),
            ArraySubscript | MemberAccess | IndirectMemberAccess => (22, 23),
        }
    }

    pub fn from_sym(sym: Symbol) -> Option<BinOp> {
        use BinOp::*;
        match sym {
            Symbol::Plus => Some(Add),
            Symbol::Minus => Some(Sub),
            Symbol::Star => Some(Mul),
            Symbol::Slash => Some(Div),
            Symbol::Percent => Some(Rem),
            Symbol::And => Some(BitwiseAnd),
            Symbol::Bar => Some(BitwiseOr),
            Symbol::Caret => Some(BitwiseXor),
            Symbol::ShiftLeft => Some(BitwiseLeftShift),
            Symbol::ShiftRight => Some(BitwiseRightShift),
            Symbol::AndAnd => Some(LogicalAnd),
            Symbol::BarBar => Some(LogicalOr),
            Symbol::EqualEqual => Some(EqualTo),
            Symbol::NotEqual => Some(NotEqualTo),
            Symbol::Less => Some(LessThan),
            Symbol::Greater => Some(GreaterThan),
            Symbol::LessEqual => Some(LessThanOrEqualTo),
            Symbol::GreaterEqual => Some(GreaterThanOrEqualTo),
            Symbol::OpenBracket => Some(ArraySubscript),
            Symbol::Dot => Some(MemberAccess),
            Symbol::Arrow => Some(IndirectMemberAccess),
            _ => None,
        }
    }
}

impl_handle! {
    pub struct ExprId(u32);
}

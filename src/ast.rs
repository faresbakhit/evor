use crate::handle::impl_handle;
use crate::span::Span;
use crate::token::IdentId;

pub use crate::syn::{BinOp, Ty, TyId, UnOp, VarDecl};

pub type AST = Vec<Func>;

#[derive(Clone, Debug, Hash)]
pub struct Func {
    pub ident: IdentId,
    pub params: Vec<VarDecl>,
    pub ret_ty: TyId,
    pub block: Vec<Stmt>,
    pub span: Span,
}

#[derive(Clone, Debug, Hash)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Clone, Debug, Hash)]
pub enum StmtKind {
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

#[derive(Clone, Debug, Hash)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: TyId,
    pub span: Span,
}

#[derive(Clone, Debug, Hash)]
pub enum ExprKind {
    Un { op: UnOp, expr: ExprId },
    Bin { op: BinOp, lhs: ExprId, rhs: ExprId },
    Call { func: IdentId, args: Span<ExprId> },
    Bool { val: bool },
    Int { val: u32 },
    Str,
    Var { id: VarId },
}

impl ExprKind {
    pub fn is_lvalue(&self) -> bool {
        matches!(
            self,
            ExprKind::Var { .. }
                | ExprKind::Un {
                    op: UnOp::Deref,
                    ..
                }
        )
    }
}

impl_handle! {
    pub struct ExprId(u32);
    pub struct VarId(u32);
}

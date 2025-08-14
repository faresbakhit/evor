use std::ops::Not;

use crate::{
    handle::impl_handle, pool::Pool, span::Span, token::IdentId, types::TyId, vars::VarId,
};

pub type NormalForm = Vec<Func>;

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Func {
    pub name: IdentId,
    pub code: Vec<Instr>,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum Instr {
    Label {
        id: LabelId,
    },
    Bin {
        dest: VarId,
        op: BinOp,
        lhs: Atom,
        rhs: Atom,
    },
    Un {
        dest: VarId,
        op: UnOp,
        atom: Atom,
    },
    Copy {
        dest: VarId,
        src: Atom,
    },
    Load {
        dest: VarId,
        src_ptr: VarId,
    },
    Store {
        dest_ptr: VarId,
        src: Atom,
    },
    Ref {
        dest_ptr: VarId,
        src: VarId,
    },
    Call {
        dest: VarId,
        func: IdentId,
        args: u32,
    },
    Jump {
        to: LabelId,
    },
    CondJump {
        cond: Cond,
        to: LabelId,
    },
    Param {
        atom: Atom,
    },
    Ret {
        atom: Option<Atom>,
    },
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum Cond {
    If { atom: Atom },
    IfFalse { atom: Atom },
    IfRel { op: RelOp, lhs: Atom, rhs: Atom },
}

impl Not for Cond {
    type Output = Cond;

    fn not(self) -> Self::Output {
        match self {
            Cond::If { atom } => Cond::IfFalse { atom },
            Cond::IfFalse { atom } => Cond::If { atom },
            Cond::IfRel { op, lhs, rhs } => Cond::IfRel {
                op: op.not(),
                lhs,
                rhs,
            },
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum Atom {
    Var(VarId),
    Const(Const),
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum Const {
    Int(u32),
    Str(Span),
    Bool(bool),
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Xor,
    LeftShift,
    RightShift,
    Rel(RelOp),
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum RelOp {
    EqualTo,
    NotEqualTo,
    LessThan,
    GreaterThan,
    LessThanOrEqualTo,
    GreaterThanOrEqualTo,
}

impl Not for RelOp {
    type Output = RelOp;
    fn not(self) -> Self::Output {
        use RelOp::*;
        match self {
            EqualTo => NotEqualTo,
            NotEqualTo => EqualTo,
            LessThan => GreaterThanOrEqualTo,
            GreaterThan => LessThanOrEqualTo,
            LessThanOrEqualTo => GreaterThan,
            GreaterThanOrEqualTo => LessThan,
        }
    }
}

impl_handle! {
    pub struct LabelId(pub u32);
}

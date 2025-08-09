use crate::{ast::BinOp, handle::impl_handle};

pub type Program = Vec<Instr>;

pub enum Instr {
    Bin {
        op: BinOp,
        dest: VarId,
        lhs: Atom,
        rhs: Atom,
    },
}

pub enum Atom {
    Var(VarId),
    Int(u32),
    Bool(bool),
}

impl_handle! {
    pub struct VarId(u32);
}

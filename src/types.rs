use std::fmt;

use crate::{
    ast::{BinOp, UnOp},
    handle::impl_handle,
    interner::Interner,
};

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum Ty {
    Void,
    Bool,
    I32,
    U8,
    Pointer(TyId),
}

impl Ty {
    fn is_pointer(&self) -> bool {
        matches!(self, Ty::Pointer(_))
    }
}

impl_handle! {
    pub struct TyId(u32);
}

#[derive(Debug)]
pub struct Types {
    interner: Interner<Ty, TyId>,
}

impl Types {
    pub const VOID: TyId = TyId(0);
    pub const BOOL: TyId = TyId(1);
    pub const I32: TyId = TyId(2);
    pub const U8: TyId = TyId(3);
    pub const STR: TyId = TyId(4);

    pub fn new() -> Self {
        let mut interner = Interner::new();
        interner.intern(Ty::Void);
        interner.intern(Ty::Bool);
        interner.intern(Ty::I32);
        let u8_ty = interner.intern(Ty::U8);
        interner.intern(Ty::Pointer(u8_ty));
        Self { interner }
    }

    pub fn insert(&mut self, ty: Ty) -> TyId {
        self.interner.intern(ty)
    }

    pub fn get(&self, id: TyId) -> &Ty {
        self.interner.resolve(id)
    }

    pub fn display(&self, id: TyId) -> TyDisplay {
        TyDisplay { id, hoarder: &self }
    }

    pub fn apply_un(&mut self, un_op: UnOp, ty: TyId) -> Option<TyId> {
        match un_op {
            UnOp::Plus | UnOp::Minus | UnOp::BitwiseNot => (ty == Types::I32).then_some(Types::I32),
            UnOp::LogicalNot => (ty == Types::BOOL).then_some(Types::BOOL),
            UnOp::Deref => match self.get(ty) {
                Ty::Pointer(pointee_id) => Some(*pointee_id),
                _ => None,
            },
            UnOp::Addr => Some(self.insert(Ty::Pointer(ty))),
        }
    }

    pub fn apply_bin(&mut self, bin_op: BinOp, lhs: TyId, rhs: TyId) -> Option<TyId> {
        let lhs_ty = self.get(lhs);
        let rhs_ty = self.get(rhs);
        match bin_op {
            BinOp::EqualTo | BinOp::NotEqualTo => (lhs == rhs).then_some(Types::BOOL),
            BinOp::Add | BinOp::Sub if lhs_ty.is_pointer() && rhs == Types::I32 => Some(lhs),
            BinOp::Add | BinOp::Sub if rhs_ty.is_pointer() && lhs == Types::I32 => Some(rhs),
            BinOp::Add
            | BinOp::Sub
            | BinOp::Mul
            | BinOp::Div
            | BinOp::Rem
            | BinOp::BitwiseAnd
            | BinOp::BitwiseOr
            | BinOp::BitwiseXor
            | BinOp::BitwiseLeftShift
            | BinOp::BitwiseRightShift => (lhs == rhs && lhs == Types::I32).then_some(Types::I32),
            BinOp::LessThan
            | BinOp::GreaterThan
            | BinOp::LessThanOrEqualTo
            | BinOp::GreaterThanOrEqualTo => {
                (lhs == rhs && lhs == Types::I32).then_some(Types::BOOL)
            }
            BinOp::LogicalAnd | BinOp::LogicalOr => {
                (lhs == rhs && lhs == Types::BOOL).then_some(Types::BOOL)
            }
        }
    }
}

pub struct TyDisplay<'p> {
    id: TyId,
    hoarder: &'p Types,
}

impl<'p> fmt::Display for TyDisplay<'p> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ty = self.hoarder.get(self.id);
        match ty {
            Ty::Void => f.write_str("void"),
            Ty::Bool => f.write_str("bool"),
            Ty::I32 => f.write_str("i32"),
            Ty::U8 => f.write_str("u8"),
            Ty::Pointer(pointee_id) => write!(
                f,
                "*{}",
                TyDisplay {
                    id: *pointee_id,
                    hoarder: self.hoarder
                }
            ),
        }
    }
}

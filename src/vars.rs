use crate::{
    handle::{impl_handle, Handle},
    symbol_table::SymbolTable,
    token::IdentId,
    types::TyId,
};

#[derive(Debug)]
pub struct Vars {
    ident_table: SymbolTable<IdentId, VarId>,
    first_temp: Option<VarId>,
    next_temp: usize,
    types_pool: Vec<TyId>,
}

impl Vars {
    pub fn new() -> Self {
        Self {
            ident_table: SymbolTable::new(),
            first_temp: None,
            next_temp: 0,
            types_pool: Vec::new(),
        }
    }

    pub fn insert(&mut self, ident: IdentId, ty: TyId) -> VarId {
        let id = self.ident_table.insert(ident);
        self.types_pool.push(ty);
        id
    }

    pub fn resolve(&self, ident: IdentId) -> Option<VarId> {
        self.ident_table.resolve(&ident).copied()
    }

    pub fn get_type(&self, var: VarId) -> TyId {
        self.types_pool[var.to_usize()]
    }

    pub fn get_ident(&self, id: VarId) -> Option<IdentId> {
        if !self.is_temp(id) {
            Some(self.ident_table.get(id))
        } else {
            None
        }
    }

    pub fn enter_scope(&mut self) {
        self.ident_table.enter_scope();
    }

    pub fn exit_scope(&mut self) {
        self.ident_table.exit_scope();
    }

    pub fn enter_temp_zone(&mut self) {
        self.next_temp = self.ident_table.len();
        self.first_temp = Some(VarId::from_usize(self.next_temp));
    }

    pub fn is_temp(&self, var: VarId) -> bool {
        self.first_temp.is_some_and(|temp| var >= temp)
    }

    pub fn next_temp(&mut self, ty: TyId) -> VarId {
        let id = VarId::from_usize(self.next_temp);
        self.types_pool.push(ty);
        self.next_temp += 1;
        id
    }
}

impl_handle! {
    pub struct VarId(u32);
}

#[cfg(test)]
mod tests {
    use crate::types::Types;

    use super::*;

    #[test]
    fn basic_usage() {
        let mut vars = Vars::new();
        vars.enter_scope();
        let ident_id = IdentId::from_usize(0);
        let var = vars.insert(ident_id, Types::BOOL);
        assert_eq!(vars.resolve(ident_id), Some(var));
        assert_eq!(vars.get_type(var), Types::BOOL);
        assert_eq!(vars.get_ident(var), Some(ident_id));
        assert!(!vars.is_temp(var));
        vars.enter_temp_zone();
        let temp = vars.next_temp(Types::I32);
        assert_ne!(temp, var);
        assert!(vars.is_temp(temp));
        assert_eq!(vars.get_type(temp), Types::I32);
        assert!(vars.get_ident(temp).is_none())
    }
}

use crate::{
    ast::{Ast, Expr, ExprId},
    normal_form::NormalForm,
    pool::Pool,
    types::Types,
    vars::Vars,
};

#[derive(Debug)]
pub struct Normalizer {
    expr_pool: Pool<Expr, ExprId>,
    types: Types,
    vars: Vars,
}

impl Normalizer {
    pub fn new(types: Types, vars: Vars, expr_pool: Pool<Expr, ExprId>) -> Self {
        Self {
            types,
            vars,
            expr_pool,
        }
    }

    pub fn lower(&mut self, ast: Ast) -> NormalForm {
        self.vars.enter_temp_zone();
        todo!("lowering")
    }
}

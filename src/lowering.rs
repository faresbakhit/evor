use crate::{
    analyzer::Vars,
    ast::{Ast, Expr, ExprId},
    pool::Pool,
    ty::Types,
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

    pub fn lower(&mut self, ast: Ast) {}
}

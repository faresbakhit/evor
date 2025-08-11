use std::{any::Any, collections::HashMap};

use crate::{
    ast::{self, BinOp, Expr, Ty, TyId, UnOp, VarId},
    parser::Parser,
    pool::Pool,
    span::Span,
    symbol_table::SymbolTable,
    syn::{self, FuncDecl},
    token::IdentId,
};

#[derive(Copy, Clone, Hash, Debug)]
struct Var {
    id: VarId,
    ty: TyId,
}

pub struct Analyzer<'a> {
    parser: Parser<'a>,
    exprs: Pool<ast::Expr, ast::ExprId>,
    funcs: HashMap<IdentId, FuncDecl>,
    var_table: SymbolTable<IdentId, VarId>,
    vars: HashMap<VarId, Var>,
}

pub type Result<T> = std::result::Result<T, Error>;

macro_rules! err {
    (at: $at:expr, $($arg:tt)*) => {
        Err(Error {
            message: format!($($arg)*),
            span: $at,
        })
    };
}

impl<'a> Analyzer<'a> {
    pub fn new(parser: Parser<'a>) -> Self {
        Self {
            parser,
            funcs: HashMap::new(),
            exprs: Pool::new(),
            var_table: SymbolTable::new(),
            vars: HashMap::new(),
        }
    }

    pub fn analyze(&mut self, prog: syn::Syn) -> Result<ast::AST> {
        for item in prog.iter() {
            match item {
                syn::Item::Struct(_) => todo!("structs"),
                syn::Item::Func(func) => {
                    self.funcs.insert(func.decl.ident, func.decl.clone());
                }
                syn::Item::FuncDecl(func_decl) => {
                    self.funcs.insert(func_decl.ident, func_decl.clone());
                }
            }
        }
        prog.into_iter()
            .filter_map(|item| {
                self.var_table.enter_scope();
                let r = if let syn::Item::Func(func) = item {
                    Some(Ok(ast::Func {
                        ident: func.decl.ident,
                        params: func.decl.params,
                        ret_ty: func.decl.ret_ty,
                        block: match self.analyze_block(func.block) {
                            Ok(block) => block,
                            Err(err) => return Some(Err(err)),
                        },
                        span: func.span,
                    }))
                } else {
                    None
                };
                self.var_table.exit_scope();
                r
            })
            .collect()
    }

    fn analyze_block(&mut self, block: Vec<syn::Stmt>) -> Result<Vec<ast::Stmt>> {
        block
            .into_iter()
            .map(|stmt| self.analyze_stmt(stmt))
            .collect()
    }

    fn analyze_stmt(&mut self, stmt: syn::Stmt) -> Result<ast::Stmt> {
        let stmt = match stmt.kind {
            syn::StmtKind::VarDecl { ident, ty, init } => {
                let id = self.var_table.insert(ident);
                match (ty, init) {
                    (Some(ty), Some(expr)) => {
                        let expr = self.analyze_expr(expr)?;
                        if ty != expr.ty {
                            return err!(at: expr.span, "expected `{}`, found `{}`", self.parser.ty_display(ty), self.parser.ty_display(expr.ty));
                        }
                        ast::Stmt::Assign {
                            lhs: self.exprs.insert(Expr {
                                kind: ast::ExprKind::Var { id },
                                ty,
                                span: Span::dumb(),
                            }),
                            rhs: self.exprs.insert(expr),
                        }
                    }
                    (None, Some(expr)) => {
                        let expr = self.analyze_expr(expr)?;
                        self.vars.insert(id, Var { id, ty: expr.ty });
                        ast::Stmt::Assign {
                            lhs: self.exprs.insert(Expr {
                                kind: ast::ExprKind::Var { id },
                                ty: expr.ty,
                                span: Span::dumb(),
                            }),
                            rhs: self.exprs.insert(expr),
                        }
                    }
                    (Some(ty), None) => match self.parser.types.resolve(ty) {
                        Ty::Void => {
                            return err!(at: Span::dumb(), "variable declared with type `void`")
                        }
                        Ty::Bool => {
                            self.vars.insert(id, Var { id, ty });
                            ast::Stmt::Assign {
                                lhs: self.exprs.insert(Expr {
                                    kind: ast::ExprKind::Var { id },
                                    ty,
                                    span: Span::dumb(),
                                }),
                                rhs: self.exprs.insert(ast::Expr {
                                    kind: ast::ExprKind::Bool { val: false },
                                    ty,
                                    span: Span::dumb(),
                                }),
                            }
                        }
                        Ty::I32 => {
                            self.vars.insert(id, Var { id, ty });
                            ast::Stmt::Assign {
                                lhs: self.exprs.insert(Expr {
                                    kind: ast::ExprKind::Var { id },
                                    ty,
                                    span: Span::dumb(),
                                }),
                                rhs: self.exprs.insert(ast::Expr {
                                    kind: ast::ExprKind::Int { val: 0 },
                                    ty,
                                    span: Span::dumb(),
                                }),
                            }
                        }
                        Ty::Pointer(ty_id) => {
                            self.vars.insert(id, Var { id, ty });
                            ast::Stmt::Assign {
                                lhs: self.exprs.insert(Expr {
                                    kind: ast::ExprKind::Var { id },
                                    ty,
                                    span: Span::dumb(),
                                }),
                                rhs: self.exprs.insert(ast::Expr {
                                    kind: ast::ExprKind::Int { val: 0 },
                                    ty,
                                    span: Span::dumb(),
                                }),
                            }
                        }
                        Ty::Struct(ident_id) => todo!("structs"),
                    },
                    (None, None) => {
                        return err!(at: stmt.span, "variable declaration without type or initializer")
                    }
                }
            }
            syn::StmtKind::Assign { lhs, rhs } => todo!(),
            syn::StmtKind::Expr { id } => {
                let expr = self.analyze_expr(id)?;
                let id = self.exprs.insert(expr);
                ast::Stmt::Expr { id }
            }
            syn::StmtKind::If {
                cond,
                then,
                otherwise,
            } => todo!(),
            syn::StmtKind::While { cond, block } => todo!(),
            syn::StmtKind::Break => todo!(),
            syn::StmtKind::Continue => todo!(),
            syn::StmtKind::Return { expr } => todo!(),
        };
        Ok(stmt)
    }

    fn analyze_expr(&mut self, id: syn::ExprId) -> Result<ast::Expr> {
        let expr = self.parser.exprs.get(id);
        let span = expr.span;
        let expr = match expr.kind {
            syn::ExprKind::Un { op, expr } => {
                let expr = self.analyze_expr(expr)?;
                if op == UnOp::Addr && !expr.kind.is_lvalue() {
                    return err!(at: span, "cannot take address of rvalue expression");
                }
                let ty = self.un_on_ty(op, expr.ty, span)?;
                let expr = self.exprs.insert(expr);
                let expr = ast::Expr {
                    kind: ast::ExprKind::Un { op, expr },
                    ty,
                    span,
                };
                expr
            }
            syn::ExprKind::Bin { op, lhs, rhs } => todo!(),
            syn::ExprKind::Call { func, args } => todo!(),
            syn::ExprKind::Bool { val } => ast::Expr {
                kind: ast::ExprKind::Bool { val },
                ty: self.parser.types.intern(ast::Ty::Bool),
                span: expr.span,
            },
            syn::ExprKind::Int { val } => ast::Expr {
                kind: ast::ExprKind::Int { val },
                ty: self.parser.types.intern(ast::Ty::I32),
                span: expr.span,
            },
            syn::ExprKind::Str => todo!("strings"),
            syn::ExprKind::Var { id } => {
                let span = expr.span;
                let var = self.resolve_var(id, span)?;
                ast::Expr {
                    kind: ast::ExprKind::Var { id: var.id },
                    ty: var.ty,
                    span,
                }
            }
        };
        Ok(expr)
    }

    fn resolve_var(&mut self, ident_id: IdentId, span: Span) -> Result<Var> {
        match self.var_table.resolve(&ident_id) {
            Some(var_id) => Ok(*self.vars.get(var_id).expect("undefined variable")),
            None => {
                let ident = self.parser.idents().resolve(ident_id);
                err!(at: span, "undefined variable `{ident}`")
            }
        }
    }

    fn un_on_ty(&mut self, op: UnOp, ty_id: TyId, span: Span) -> Result<TyId> {
        let ty = self.parser.types.resolve(ty_id);
        let ty = match ty {
            ast::Ty::Void => match op {
                UnOp::Plus | UnOp::Minus | UnOp::BitwiseNot | UnOp::LogicalNot => {
                    return err!(at: span, "invalid operation on expression of primitive type `void`")
                }
                UnOp::Deref => {
                    return err!(at: span, "can't dereference expression of primitive type `void`")
                }
                UnOp::Addr => ast::Ty::Pointer(ty_id),
            },
            ast::Ty::Bool => match op {
                UnOp::Plus | UnOp::Minus | UnOp::BitwiseNot | UnOp::LogicalNot => {
                    return err!(at: span, "invalid operation on expression of primitive type `bool`")
                }
                UnOp::Deref => {
                    return err!(at: span, "can't dereference expression of primitive type `bool`")
                }
                UnOp::Addr => ast::Ty::Pointer(ty_id),
            },
            ast::Ty::I32 => match op {
                UnOp::Plus => ast::Ty::I32,
                UnOp::Minus => ast::Ty::I32,
                UnOp::BitwiseNot => ast::Ty::I32,
                UnOp::LogicalNot => ast::Ty::I32,
                UnOp::Deref => {
                    return err!(at: span, "can't dereference expression of primitive type `i32`")
                }
                UnOp::Addr => ast::Ty::Pointer(ty_id),
            },
            ast::Ty::Pointer(pointee_ty_id) => match op {
                UnOp::Plus | UnOp::Minus | UnOp::BitwiseNot | UnOp::LogicalNot => {
                    return err!(at: span, "invalid operation on expression of primitive type `bool`")
                }
                UnOp::Deref => return Ok(*pointee_ty_id),
                UnOp::Addr => ast::Ty::Pointer(ty_id),
            },
            ast::Ty::Struct(ident_id) => todo!("structs"),
        };
        Ok(self.parser.types.intern(ty))
    }
}

#[derive(Clone, Hash, Debug)]
pub struct Error {
    pub message: String,
    pub span: Span,
}

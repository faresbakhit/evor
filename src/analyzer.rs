use std::collections::HashMap;

use crate::{
    ast::{self, Expr, FuncDecl, UnOp},
    handle::Handle,
    interner::StringInterner,
    pool::Pool,
    span::Span,
    syn,
    token::IdentId,
    types::{Ty, TyId, Types},
    vars::Vars,
};

#[derive(Debug)]
pub struct Analyzer {
    idents: StringInterner<IdentId>,
    syn_expr_pool: Pool<syn::Expr, syn::ExprId>,
    types: Types,
    expr_pool: Pool<ast::Expr, ast::ExprId>,
    funcs: HashMap<IdentId, FuncDecl>,
    vars: Vars,
}

#[derive(Debug)]
pub struct Output {
    pub idents: StringInterner<IdentId>,
    pub types: Types,
    pub expr_pool: Pool<Expr, ast::ExprId>,
    pub funcs: HashMap<IdentId, FuncDecl>,
    pub vars: Vars,
}

pub type Result<T> = std::result::Result<T, Error>;

macro_rules! err {
    (at: $at:expr, $($arg:tt)*) => {
        Err(Error { message: format!($($arg)*), span: $at })
    };
}

impl Analyzer {
    pub fn new(
        idents: StringInterner<IdentId>,
        syn_expr_pool: Pool<syn::Expr, syn::ExprId>,
        types: Types,
    ) -> Self {
        Self {
            idents,
            syn_expr_pool,
            types,
            expr_pool: Pool::new(),
            funcs: HashMap::new(),
            vars: Vars::new(),
        }
    }

    pub fn finish(self) -> Output {
        Output {
            idents: self.idents,
            types: self.types,
            expr_pool: self.expr_pool,
            funcs: self.funcs,
            vars: self.vars,
        }
    }

    pub fn analyze(&mut self, prog: syn::Syn) -> Result<ast::Ast> {
        for item in prog.iter() {
            match item {
                syn::Item::Struct(_) => todo!("structs"),
                syn::Item::Func(syn::Func { decl, .. }) | syn::Item::FuncDecl(decl) => {
                    if self.funcs.get(&decl.name).is_some() {
                        let name = self.idents.resolve(decl.name);
                        return err!(at: decl.span, "redefinition of function `{name}`");
                    }
                    self.funcs.insert(
                        decl.name,
                        FuncDecl {
                            domain: decl.params.iter().map(|p| p.ty).collect(),
                            range: decl.ret_ty,
                        },
                    );
                }
            }
        }
        prog.into_iter()
            .filter_map(|item| {
                if let syn::Item::Func(func) = item {
                    self.vars.enter_scope();
                    for param in func.decl.params.iter() {
                        match param.ident {
                            Some(ident) => {
                                self.vars.insert(ident, param.ty);
                            }
                            None => (),
                        }
                    }
                    let func = ast::Func {
                        name: func.decl.name,
                        params: func.decl.params,
                        ret_ty: func.decl.ret_ty,
                        block: match func
                            .block
                            .into_iter()
                            .map(|stmt| self.analyze_stmt(stmt, InLoop::No, func.decl.ret_ty))
                            .collect()
                        {
                            Ok(block) => block,
                            Err(err) => return Some(Err(err)),
                        },
                        span: func.span,
                    };
                    self.vars.exit_scope();
                    Some(Ok(func))
                } else {
                    None
                }
            })
            .collect()
    }

    fn analyze_block(
        &mut self,
        block: Vec<syn::Stmt>,
        in_loop: InLoop,
        returnable: TyId,
    ) -> Result<Vec<ast::Stmt>> {
        self.vars.enter_scope();
        let block = block
            .into_iter()
            .map(|stmt| self.analyze_stmt(stmt, in_loop, returnable))
            .collect();
        self.vars.exit_scope();
        block
    }

    fn analyze_stmt(
        &mut self,
        stmt: syn::Stmt,
        in_loop: InLoop,
        returnable: TyId,
    ) -> Result<ast::Stmt> {
        let span = stmt.span;
        let kind = match stmt.kind {
            syn::StmtKind::VarDecl { ident, ty, init } => match (ty, init) {
                (Some(ty), Some(expr)) => {
                    if ty == Types::VOID {
                        return err!(at: span, "can't declare variable with type `void`");
                    }
                    let expr = self.analyze_expr(expr)?;
                    if ty != expr.ty {
                        return err!(at: expr.span, "expected `{}`, found `{}`", self.types.display(ty), self.types.display(expr.ty));
                    }
                    let id = self.vars.insert(ident, expr.ty);
                    ast::StmtKind::Assign {
                        lhs: id,
                        rhs: self.expr_pool.insert(expr),
                    }
                }
                (None, Some(expr)) => {
                    let expr = self.analyze_expr(expr)?;
                    let id = self.vars.insert(ident, expr.ty);
                    ast::StmtKind::Assign {
                        lhs: id,
                        rhs: self.expr_pool.insert(expr),
                    }
                }
                (Some(ty), None) => match self.types.get(ty) {
                    Ty::Void => return err!(at: span, "can't declare variable with type `void`"),
                    Ty::Bool => {
                        let id = self.vars.insert(ident, ty);
                        ast::StmtKind::Assign {
                            lhs: id,
                            rhs: self.expr_pool.insert(ast::Expr {
                                kind: ast::ExprKind::Bool { val: false },
                                ty,
                                span: Span::dumb(),
                            }),
                        }
                    }
                    Ty::I32 => {
                        let id = self.vars.insert(ident, ty);
                        ast::StmtKind::Assign {
                            lhs: id,
                            rhs: self.expr_pool.insert(ast::Expr {
                                kind: ast::ExprKind::Int { val: 0 },
                                ty,
                                span: Span::dumb(),
                            }),
                        }
                    }
                    Ty::U8 => {
                        let id = self.vars.insert(ident, ty);
                        ast::StmtKind::Assign {
                            lhs: id,
                            rhs: self.expr_pool.insert(ast::Expr {
                                kind: ast::ExprKind::Int { val: 255 },
                                ty,
                                span: Span::dumb(),
                            }),
                        }
                    }
                    Ty::Pointer(_) => {
                        let id = self.vars.insert(ident, ty);
                        ast::StmtKind::Assign {
                            lhs: id,
                            rhs: self.expr_pool.insert(ast::Expr {
                                kind: ast::ExprKind::Int { val: 0 },
                                ty,
                                span: Span::dumb(),
                            }),
                        }
                    }
                },
                (None, None) => return err!(at: span, "can't declare variable with no type"),
            },
            syn::StmtKind::Assign { lhs, rhs } => {
                let lhs = self.analyze_expr(lhs)?;
                let rhs = self.analyze_expr(rhs)?;
                if lhs.ty != rhs.ty {
                    return err!(at: rhs.span, "expected `{}`, found `{}`", self.types.display(lhs.ty), self.types.display(rhs.ty));
                }
                match lhs.kind {
                    ast::ExprKind::Un { op, expr } if op == UnOp::Deref => {
                        ast::StmtKind::DerefAssign {
                            lhs: expr,
                            rhs: self.expr_pool.insert(rhs),
                        }
                    }
                    ast::ExprKind::Var { id } => ast::StmtKind::Assign {
                        lhs: id,
                        rhs: self.expr_pool.insert(rhs),
                    },
                    _ => return err!(at: lhs.span, "not an lvalue expression"),
                }
            }
            syn::StmtKind::Expr { id } => {
                let expr = self.analyze_expr(id)?;
                let id = self.expr_pool.insert(expr);
                ast::StmtKind::Expr { id }
            }
            syn::StmtKind::If {
                cond,
                then,
                otherwise,
            } => {
                let cond = self.analyze_expr(cond)?;
                if cond.ty != Types::BOOL {
                    return err!(at: cond.span, "expected `bool`, found `{}`", self.types.display(cond.ty));
                }
                ast::StmtKind::If {
                    cond: self.expr_pool.insert(cond),
                    then: self.analyze_block(then, in_loop, returnable)?,
                    r#else: self.analyze_block(otherwise, in_loop, returnable)?,
                }
            }
            syn::StmtKind::While { cond, block } => {
                let cond = self.analyze_expr(cond)?;
                if cond.ty != Types::BOOL {
                    return err!(at: cond.span, "expected `bool`, found `{}`", self.types.display(cond.ty));
                }
                ast::StmtKind::While {
                    cond: self.expr_pool.insert(cond),
                    block: self.analyze_block(block, InLoop::Yes, returnable)?,
                }
            }
            syn::StmtKind::Break => {
                if in_loop == InLoop::No {
                    return err!(at: span, "can't `break` outside of loop");
                }
                ast::StmtKind::Break
            }
            syn::StmtKind::Continue => {
                if in_loop == InLoop::No {
                    return err!(at: span, "can't `continue` outside of loop");
                }
                ast::StmtKind::Continue
            }
            syn::StmtKind::Return { expr } => {
                let expr = match expr {
                    Some(expr) => {
                        let expr = self.analyze_expr(expr)?;
                        if expr.ty != returnable {
                            return err!(at: expr.span, "expected `{}`, found `{}`", self.types.display(returnable), self.types.display(expr.ty));
                        }
                        Some(self.expr_pool.insert(expr))
                    }
                    None => {
                        if returnable != Types::VOID {
                            return err!(at: stmt.span, "expected `{}`, found `void`", self.types.display(returnable));
                        }
                        None
                    }
                };
                ast::StmtKind::Return { expr }
            }
        };
        Ok(ast::Stmt { kind, span })
    }

    fn analyze_expr(&mut self, id: syn::ExprId) -> Result<ast::Expr> {
        let expr = self.syn_expr_pool.get(id);
        let span = expr.span;
        let expr = match expr.kind {
            syn::ExprKind::Un { op, expr } => {
                let expr = self.analyze_expr(expr)?;
                if op == UnOp::Addr && !expr.kind.is_lvalue() {
                    return err!(at: span, "can't take address of rvalue expression");
                }
                let ty = match self.types.apply_un(op, expr.ty) {
                    Some(ty) => ty,
                    None => {
                        return err!(at: span, "can't apply unary operator `{op}` on type `{}`", self.types.display(expr.ty))
                    }
                };
                let expr = self.expr_pool.insert(expr);
                ast::Expr {
                    kind: ast::ExprKind::Un { op, expr },
                    ty,
                    span,
                }
            }
            syn::ExprKind::Bin { op, lhs, rhs } => {
                let lhs = self.analyze_expr(lhs)?;
                let rhs = self.analyze_expr(rhs)?;
                let ty = match self.types.apply_bin(op, lhs.ty, rhs.ty) {
                    Some(ty) => ty,
                    None => {
                        return err!(
                            at: span,
                            "can't apply binary operator `{}` on types `{}` and `{}`",
                            op,
                            self.types.display(lhs.ty),
                            self.types.display(rhs.ty)
                        )
                    }
                };
                let lhs = self.expr_pool.insert(lhs);
                let rhs = self.expr_pool.insert(rhs);
                ast::Expr {
                    kind: ast::ExprKind::Bin { op, lhs, rhs },
                    ty,
                    span,
                }
            }
            syn::ExprKind::Call { func, args } => {
                let decl = match self.funcs.get(&func) {
                    Some(decl) => decl,
                    None => {
                        let name = self.idents.resolve(func);
                        return err!(at: expr.span, "can't find function `{name}` in this scope");
                    }
                };
                if args.len() != decl.domain.len() {
                    let name = self.idents.resolve(func);
                    return err!(at: expr.span, "function `{name}` expects {} arguments, found {}", decl.domain.len(), args.len());
                }
                let ty = decl.range;
                let params = decl.domain.clone();
                let start = self.expr_pool.len();
                let mut ast_args = Vec::with_capacity(args.len());
                for (i, (arg, param_ty)) in args.zip(params).enumerate() {
                    let arg = self.analyze_expr(arg)?;
                    if arg.ty != param_ty {
                        let func = self.idents.resolve(func);
                        return err!(
                            at: arg.span,
                            "function `{func}` expects `{}` for parameter {}, found `{}`",
                            self.types.display(param_ty),
                            i + 1,
                            self.types.display(arg.ty)
                        );
                    }
                    ast_args.push(arg);
                }
                let args = self.expr_pool.insert_many(ast_args);
                ast::Expr {
                    kind: ast::ExprKind::Call { func, args },
                    ty,
                    span,
                }
            }
            syn::ExprKind::Bool { val } => ast::Expr {
                kind: ast::ExprKind::Bool { val },
                ty: Types::BOOL,
                span: expr.span,
            },
            syn::ExprKind::Int { val } => ast::Expr {
                kind: ast::ExprKind::Int { val },
                ty: Types::I32,
                span: expr.span,
            },
            syn::ExprKind::Str => ast::Expr {
                kind: ast::ExprKind::Str,
                ty: Types::STR,
                span: expr.span,
            },
            syn::ExprKind::Var { id } => {
                let span = expr.span;
                let id = match self.vars.resolve(id) {
                    Some(var) => var,
                    None => {
                        let ident = self.idents.resolve(id);
                        return err!(at: span, "can't find variable `{ident}` in this scope");
                    }
                };
                ast::Expr {
                    kind: ast::ExprKind::Var { id },
                    ty: self.vars.get_type(id),
                    span,
                }
            }
        };
        Ok(expr)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum InLoop {
    No,
    Yes,
}

#[derive(Clone, Hash, Debug)]
pub struct Error {
    pub message: String,
    pub span: Span,
}

use std::fmt;

use crate::interner::Interner;
use crate::interner::StringInterner;
use crate::lexer::Lexer;
use crate::pool::Pool;
use crate::span::Span;
use crate::syn::*;
use crate::token::Keyword::*;
use crate::token::Symbol::*;
use crate::token::TokenKind::*;
use crate::token::*;

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Lexer<'a>,
    pub types: Interner<Ty, TyId>,
    pub exprs: Pool<Expr, ExprId>,
}

pub type Result<T> = std::result::Result<T, Error>;

macro_rules! expected {
    ($expr:expr, found: $found:expr, at: $at:expr) => {
        Err(Error {
            expected: format!("{}", $expr),
            found: format!("{}", $found),
            span: $at,
        })
    };

    ($expr:expr, found: $token:expr) => {
        expected!($expr, found: $token.kind, at: $token.span)
    };
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            tokens: lexer,
            types: Interner::new(),
            exprs: Pool::new(),
        }
    }

    pub fn parse(&mut self) -> Result<Syn> {
        let mut syn = Syn::new();
        loop {
            let token = self.tokens.peek();
            let item = match token.kind {
                Kw(Struct) => {
                    use crate::syn::Struct;
                    let start = token.span.start();
                    self.tokens.next();
                    let ident = self.parse_ident()?;
                    self.expect(OpenBrace)?;
                    let members = self.parse_var_decls(CloseBrace)?;
                    let end = self.tokens.next().span.end();
                    let span = Span::new(start, end);
                    Item::Struct(Struct {
                        ident,
                        members,
                        span,
                    })
                }
                Eof => break Ok(syn),
                _ => {
                    let (ret_ty, ret_ty_span) = match self.parse_type() {
                        Ok(r) => r,
                        Err(Error { found, span, .. }) => {
                            return expected!("function or struct", found: found, at: span)
                        }
                    };
                    let start = ret_ty_span.start();
                    let ident = self.parse_ident()?;
                    self.expect(OpenParen)?;
                    let params = self.parse_var_decls(CloseParen)?;
                    self.tokens.next();
                    let token = self.tokens.next();
                    match token.kind {
                        Sym(OpenBrace) => {
                            let mut block = Vec::new();
                            while self.tokens.peek().kind != Sym(CloseBrace) {
                                block.push(self.parse_stmt()?);
                            }
                            let end = self.tokens.next().span.end();
                            let span = Span::new(start, end);
                            let decl = FuncDecl {
                                ident,
                                params,
                                ret_ty,
                                span,
                            };
                            let func = Func { decl, block, span };
                            Item::Func(func)
                        }
                        Sym(Semicolon) => {
                            let span = Span::new(start, token.span.end());
                            let decl = FuncDecl {
                                ident,
                                params,
                                ret_ty,
                                span,
                            };
                            Item::FuncDecl(decl)
                        }
                        _ => return expected!("`{` or `;`", found: token),
                    }
                }
            };
            syn.push(item);
        }
    }

    pub fn idents(&self) -> &StringInterner<IdentId> {
        self.tokens.idents()
    }

    pub fn ty_display(&self, id: TyId) -> TyDisplay {
        TyDisplay {
            id,
            types: &self.types,
            idents: self.idents(),
        }
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>> {
        self.expect(OpenBrace)?;
        let mut block = Vec::new();
        while self.tokens.peek().kind != Sym(CloseBrace) {
            block.push(self.parse_stmt()?);
        }
        self.tokens.next();
        Ok(block)
    }

    fn parse_stmt(&mut self) -> Result<Stmt> {
        let token = self.tokens.peek();
        let stmt = match token.kind {
            Kw(Var) => {
                self.tokens.next();
                let ident = self.parse_ident()?;
                let token = self.tokens.next();
                let stmt = match token.kind {
                    Sym(Colon) => {
                        let (ty, _) = self.parse_type()?;
                        let token = self.tokens.next();
                        match token.kind {
                            Sym(Equal) => {
                                let expr = self.parse_expr()?;
                                self.expect(Semicolon)?;
                                Stmt {
                                    kind: StmtKind::VarDecl {
                                        ident,
                                        ty: Some(ty),
                                        init: Some(expr),
                                    },
                                    span: Span::dumb(),
                                }
                            }
                            Sym(Semicolon) => Stmt {
                                kind: StmtKind::VarDecl {
                                    ident,
                                    ty: Some(ty),
                                    init: None,
                                },
                                span: Span::dumb(),
                            },
                            _ => return expected!("`=` or `;`", found: token),
                        }
                    }
                    Sym(Equal) => {
                        let expr = self.parse_expr()?;
                        self.expect(Semicolon)?;
                        Stmt {
                            kind: StmtKind::VarDecl {
                                ident,
                                ty: None,
                                init: Some(expr),
                            },
                            span: Span::dumb(),
                        }
                    }
                    Sym(Semicolon) => Stmt {
                        kind: StmtKind::VarDecl {
                            ident,
                            ty: None,
                            init: None,
                        },
                        span: Span::dumb(),
                    },
                    _ => return expected!("one of `:`, `=`, or `;`", found: token),
                };
                stmt
            }
            Kw(If) => {
                self.tokens.next();
                Stmt {
                    kind: StmtKind::If {
                        cond: self.parse_expr()?,
                        then: self.parse_block()?,
                        otherwise: if self.tokens.peek().kind == Kw(Else) {
                            self.tokens.next();
                            self.parse_block()?
                        } else {
                            Vec::new()
                        },
                    },
                    span: Span::dumb(),
                }
            }
            Kw(While) => {
                self.tokens.next();
                Stmt {
                    kind: StmtKind::While {
                        cond: self.parse_expr()?,
                        block: self.parse_block()?,
                    },
                    span: Span::dumb(),
                }
            }
            Kw(Return) => {
                self.tokens.next();
                Stmt {
                    kind: StmtKind::Return {
                        expr: if self.tokens.peek().kind == Sym(Semicolon) {
                            self.tokens.next();
                            None
                        } else {
                            let expr = self.parse_expr()?;
                            self.expect(Semicolon)?;
                            Some(expr)
                        },
                    },
                    span: Span::dumb(),
                }
            }
            Kw(Break) => {
                self.tokens.next();
                self.expect(Semicolon)?;
                Stmt {
                    kind: StmtKind::Break,
                    span: Span::dumb(),
                }
            }
            Kw(Continue) => {
                self.tokens.next();
                self.expect(Semicolon)?;
                Stmt {
                    kind: StmtKind::Break,
                    span: Span::dumb(),
                }
            }
            _ => {
                let lhs = self.parse_expr()?;
                let token = self.tokens.next();
                match token.kind {
                    Sym(Semicolon) => Stmt {
                        kind: StmtKind::Expr { id: lhs },
                        span: Span::dumb(),
                    },
                    Sym(Equal) => {
                        let rhs = self.parse_expr()?;
                        self.expect(Semicolon)?;
                        Stmt {
                            kind: StmtKind::Assign { lhs, rhs },
                            span: Span::dumb(),
                        }
                    }
                    _ => return expected!("`;` or `=`", found: token),
                }
            }
        };
        Ok(stmt)
    }

    fn parse_expr(&mut self) -> Result<ExprId> {
        let expr = self.parse_expr_with_bp(0)?;
        Ok(self.exprs.insert(expr))
    }

    fn parse_expr_with_bp(&mut self, min_bp: u8) -> Result<Expr> {
        let token = self.tokens.next();
        let mut lhs = match token.kind {
            Sym(OpenParen) => {
                let r = self.parse_expr_with_bp(0)?;
                self.expect(CloseParen)?;
                r
            }
            Sym(sym) => {
                let op = match UnOp::from_sym(sym) {
                    Some(op) => op,
                    None => return expected!("expression", found: token),
                };
                let ((), r_bp) = op.binding_power();
                let expr = self.parse_expr_with_bp(r_bp)?;
                let end = expr.span.end();
                Expr {
                    kind: ExprKind::Un {
                        op,
                        expr: self.exprs.insert(expr),
                    },
                    span: Span::new(token.span.start(), end),
                }
            }
            Kw(True) => Expr {
                kind: ExprKind::Bool { val: true },
                span: token.span,
            },
            Kw(False) => Expr {
                kind: ExprKind::Bool { val: false },
                span: token.span,
            },
            Num(val) => Expr {
                kind: ExprKind::Int { val },
                span: token.span,
            },
            Str => Expr {
                kind: ExprKind::Str,
                span: token.span,
            },
            Ident(id) => {
                if self.tokens.next_if(Sym(OpenParen)).is_some() {
                    let mut end = self.tokens.peek().span.end();
                    let mut args = Vec::new();
                    while self.tokens.next_if(Sym(CloseParen)).is_none() {
                        args.push(self.parse_expr_with_bp(0)?);
                        self.tokens.next_if(Sym(Comma));
                        end = self.tokens.peek().span.end();
                    }
                    let args = self.exprs.insert_many(args);
                    Expr {
                        kind: ExprKind::Call { func: id, args },
                        span: Span::new(token.span.start(), end),
                    }
                } else {
                    Expr {
                        kind: ExprKind::Var { id },
                        span: token.span,
                    }
                }
            }
            _ => return expected!("expression", found: token),
        };
        while let Token { kind: Sym(sym), .. } = self.tokens.peek() {
            let sym = *sym;
            let op = match BinOp::from_sym(sym) {
                Some(op) => op,
                None => break,
            };
            let (l_bp, r_bp) = op.binding_power();
            if l_bp < min_bp {
                break;
            }
            self.tokens.next();
            let rhs = self.parse_expr_with_bp(r_bp)?;

            let span = if sym == OpenBracket {
                // array subscript
                Span::new(lhs.span.start(), self.expect(CloseBracket)?.end())
            } else {
                Span::new(lhs.span.start(), rhs.span.end())
            };
            lhs = Expr {
                kind: ExprKind::Bin {
                    op,
                    lhs: self.exprs.insert(lhs),
                    rhs: self.exprs.insert(rhs),
                },
                span,
            };
        }
        Ok(lhs)
    }

    fn parse_var_decls(&mut self, closing_sym: Symbol) -> Result<Vec<VarDecl>> {
        let mut params = Vec::new();
        loop {
            let token = self.tokens.peek();
            let start = token.span.start();
            let var = match token.kind {
                Ident(id) => Some(id),
                Kw(Underscore) => None,
                Sym(sym) if sym == closing_sym => break Ok(params),
                _ => break expected!("variable declarator", found: token),
            };
            self.tokens.next();
            self.expect(Colon)?;
            let (ty, ty_span) = self.parse_type()?;
            let span = Span::new(start, ty_span.end());
            params.push(VarDecl { var, ty, span });
            let token = self.tokens.peek();
            match token.kind {
                Sym(Comma) => {
                    self.tokens.next();
                }
                Sym(sym) if sym == closing_sym => break Ok(params),
                _ => break expected!(format!("`,` or {}", closing_sym), found: token),
            }
        }
    }

    fn parse_type(&mut self) -> Result<(TyId, Span)> {
        let token = self.tokens.next();
        let start = token.span.start();
        let mut end = token.span.end();
        let mut ty = match token.kind {
            Kw(Void) => self.types.intern(Ty::Void),
            Kw(Bool) => self.types.intern(Ty::Bool),
            Kw(I32) => self.types.intern(Ty::I32),
            Ident(id) => self.types.intern(Ty::Struct(id)),
            _ => return expected!("type", found: token),
        };
        while let Sym(Star) = self.tokens.peek().kind {
            ty = self.types.intern(Ty::Pointer(ty));
            end = self.tokens.next().span.end();
        }
        Ok((ty, Span::new(start, end)))
    }

    fn parse_ident(&mut self) -> Result<IdentId> {
        let token = self.tokens.next();
        match token.kind {
            Ident(id) => Ok(id),
            _ => expected!("identifier", found: token),
        }
    }

    #[inline]
    fn expect(&mut self, sym: Symbol) -> Result<Span> {
        let token = self.tokens.next();
        if token.kind == Sym(sym) {
            Ok(token.span)
        } else {
            expected!(sym, found: token)
        }
    }
}

pub struct TyDisplay<'p> {
    id: TyId,
    types: &'p Interner<Ty, TyId>,
    idents: &'p StringInterner<IdentId>,
}

impl<'p> fmt::Display for TyDisplay<'p> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ty = self.types.resolve(self.id);
        match ty {
            Ty::Void => f.write_str("void"),
            Ty::Bool => f.write_str("bool"),
            Ty::I32 => f.write_str("i32"),
            Ty::Pointer(pointee_id) => write!(
                f,
                "{}*",
                TyDisplay {
                    id: *pointee_id,
                    types: self.types,
                    idents: self.idents
                }
            ),
            Ty::Struct(ident_id) => {
                f.write_str("struct ")?;
                f.write_str(self.idents.resolve(*ident_id))
            }
        }
    }
}

#[derive(Clone, Hash, Debug)]
pub struct Error {
    pub expected: String,
    pub found: String,
    pub span: Span,
}

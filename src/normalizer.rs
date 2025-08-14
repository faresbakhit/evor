use std::{collections::HashMap, ops::Not};

use crate::{
    ast::{self, Ast, Expr, ExprId, ExprKind, FuncDecl, Stmt, StmtKind},
    interner::StringInterner,
    normal_form::{
        self, Atom, BinOp, Cond, Const,
        Instr::{self, *},
        LabelId, NormalForm, RelOp, UnOp,
    },
    pool::Pool,
    token::IdentId,
    types::Types,
    vars::Vars,
};

#[derive(Debug)]
pub struct Normalizer {
    idents: StringInterner<IdentId>,
    types: Types,
    expr_pool: Pool<Expr, ast::ExprId>,
    funcs: HashMap<IdentId, FuncDecl>,
    vars: Vars,
    label_count: u32,
    loop_start: LabelId,
    loop_end: LabelId,
}

macro_rules! analyzerbug {
    () => {
        unreachable!("precondition not met by the analyzer")
    };
}

impl Normalizer {
    pub fn new(
        idents: StringInterner<IdentId>,
        types: Types,
        expr_pool: Pool<Expr, ast::ExprId>,
        funcs: HashMap<IdentId, FuncDecl>,
        mut vars: Vars,
    ) -> Self {
        vars.enter_temp_zone();
        Self {
            idents,
            types,
            expr_pool,
            funcs,
            vars,
            label_count: 0,
            loop_start: LabelId(u32::MAX),
            loop_end: LabelId(u32::MAX),
        }
    }

    pub fn normalize(&mut self, ast: Ast) -> NormalForm {
        ast.into_iter()
            .map(|func| self.normalize_func(func))
            .collect()
    }

    pub fn normalize_func(&mut self, func: ast::Func) -> normal_form::Func {
        let mut code = Vec::new();
        self.normalize_block(&mut code, func.block);
        normal_form::Func {
            name: func.name,
            code,
        }
    }

    pub fn normalize_block(&mut self, instr_block: &mut Vec<Instr>, block: Vec<Stmt>) {
        instr_block.reserve(block.len());
        for stmt in block {
            self.normalize_stmt(instr_block, stmt)
        }
    }

    pub fn normalize_stmt(&mut self, block: &mut Vec<Instr>, stmt: Stmt) {
        let instr = match stmt.kind {
            StmtKind::Assign { lhs, rhs } => Copy {
                dest: lhs,
                src: self.atomize(block, rhs),
            },
            StmtKind::DerefAssign { lhs, rhs } => {
                let lhs_expr = self.expr_pool.get(lhs);
                let temp = self.vars.next_temp(lhs_expr.ty);
                let copy = Copy {
                    dest: temp,
                    src: self.atomize(block, lhs),
                };
                block.push(copy);
                Store {
                    dest_ptr: temp,
                    src: self.atomize(block, rhs),
                }
            }
            StmtKind::Expr { id } => {
                self.atomize(block, id);
                return;
            }
            StmtKind::If { cond, then, r#else } => {
                let then_label = self.next_label();
                let else_label = self.next_label();
                let endif_label = self.next_label();
                self.atomize_cond(block, cond, then_label, else_label);
                block.push(Label { id: then_label });
                self.normalize_block(block, then);
                block.push(Jump { to: endif_label });
                block.push(Label { id: else_label });
                self.normalize_block(block, r#else);
                block.push(Label { id: endif_label });
                return;
            }
            StmtKind::While { cond, block: stmts } => {
                self.loop_start = self.next_label();
                let then = self.next_label();
                self.loop_end = self.next_label();
                block.push(Label {
                    id: self.loop_start,
                });
                self.atomize_cond(block, cond, then, self.loop_end);
                block.push(Label { id: then });
                self.normalize_block(block, stmts);
                block.push(Jump {
                    to: self.loop_start,
                });
                block.push(Label { id: self.loop_end });
                return;
            }
            StmtKind::Break => {
                block.push(Jump { to: self.loop_end });
                return;
            }
            StmtKind::Continue => {
                block.push(Jump {
                    to: self.loop_start,
                });
                return;
            }
            StmtKind::Return { expr } => Ret {
                atom: expr.map(|expr| self.atomize(block, expr)),
            },
        };
        block.push(instr);
    }

    pub fn atomize_cond(
        &mut self,
        block: &mut Vec<Instr>,
        cond: ExprId,
        cont: LabelId,
        term: LabelId,
    ) {
        let cond = self.expr_pool.get(cond);
        let (op, lhs, rhs) = match cond.kind {
            ExprKind::Un { op, expr } => match op {
                ast::UnOp::LogicalNot => {
                    self.atomize_cond(block, expr, term, cont);
                    return;
                }
                _ => analyzerbug!(),
            },
            ExprKind::Bin { op, lhs, rhs } => match op {
                ast::BinOp::EqualTo => (RelOp::EqualTo, lhs, rhs),
                ast::BinOp::NotEqualTo => (RelOp::NotEqualTo, lhs, rhs),
                ast::BinOp::LessThan => (RelOp::LessThan, lhs, rhs),
                ast::BinOp::GreaterThan => (RelOp::GreaterThan, lhs, rhs),
                ast::BinOp::LessThanOrEqualTo => (RelOp::LessThanOrEqualTo, lhs, rhs),
                ast::BinOp::GreaterThanOrEqualTo => (RelOp::GreaterThanOrEqualTo, lhs, rhs),
                ast::BinOp::LogicalAnd => {
                    let mid = self.next_label();
                    self.atomize_cond(block, lhs, mid, term);
                    block.push(Instr::Label { id: mid });
                    self.atomize_cond(block, rhs, cont, term);
                    return;
                }
                ast::BinOp::LogicalOr => {
                    let mid = self.next_label();
                    self.atomize_cond(block, lhs, cont, mid);
                    block.push(Instr::Label { id: mid });
                    self.atomize_cond(block, rhs, cont, term);
                    return;
                }
                _ => analyzerbug!(),
            },
            ExprKind::Bool { val: true } => {
                block.push(Instr::Jump { to: cont });
                return;
            }
            ExprKind::Bool { val: false } => {
                block.push(Instr::Jump { to: term });
                return;
            }
            ExprKind::Var { id } => {
                block.push(Instr::CondJump {
                    cond: Cond::If {
                        atom: Atom::Var(id),
                    },
                    to: cont,
                });
                block.push(Instr::Jump { to: term });
                return;
            }
            _ => analyzerbug!(),
        };
        let lhs = self.atomize(block, lhs);
        let rhs = self.atomize(block, rhs);
        block.push(Instr::CondJump {
            cond: Cond::IfRel { op, lhs, rhs },
            to: cont,
        });
        block.push(Instr::Jump { to: term });
    }

    pub fn atomize(&mut self, block: &mut Vec<Instr>, expr: ExprId) -> Atom {
        let expr = self.expr_pool.get(expr);
        match expr.kind {
            ExprKind::Un { op, expr: un_expr } => {
                let op = match op {
                    ast::UnOp::Plus => return self.atomize(block, un_expr),
                    ast::UnOp::Minus => UnOp::Neg,
                    ast::UnOp::BitwiseNot | ast::UnOp::LogicalNot => UnOp::Not,
                    ast::UnOp::Deref => {
                        let temp = self.vars.next_temp(expr.ty);
                        let atom = self.atomize(block, un_expr);
                        block.push(Instr::Load {
                            dest: temp,
                            src_ptr: match atom {
                                Atom::Var(var_id) => var_id,
                                _ => analyzerbug!(),
                            },
                        });
                        return Atom::Var(temp);
                    }
                    ast::UnOp::Addr => {
                        let temp = self.vars.next_temp(expr.ty);
                        let atom = self.atomize(block, un_expr);
                        block.push(Instr::Ref {
                            dest_ptr: temp,
                            src: match atom {
                                Atom::Var(var_id) => var_id,
                                _ => analyzerbug!(),
                            },
                        });
                        return Atom::Var(temp);
                    }
                };
                let temp = self.vars.next_temp(expr.ty);
                let atom = self.atomize(block, un_expr);
                block.push(Instr::Un {
                    op,
                    dest: temp,
                    atom,
                });
                Atom::Var(temp)
            }
            ExprKind::Bin { op, lhs, rhs } => {
                let op = match op {
                    ast::BinOp::Add => BinOp::Add,
                    ast::BinOp::Sub => BinOp::Sub,
                    ast::BinOp::Mul => BinOp::Mul,
                    ast::BinOp::Div => BinOp::Div,
                    ast::BinOp::Rem => BinOp::Rem,
                    ast::BinOp::BitwiseAnd => BinOp::And,
                    ast::BinOp::BitwiseOr => BinOp::Or,
                    ast::BinOp::BitwiseXor => BinOp::Xor,
                    ast::BinOp::BitwiseLeftShift => BinOp::LeftShift,
                    ast::BinOp::BitwiseRightShift => BinOp::RightShift,
                    ast::BinOp::LogicalAnd => {
                        // lhs && rhs
                        //
                        // temp = false
                        // if !lhs goto end
                        // if !rhs goto end
                        // temp = true
                        // end:
                        // temp
                        let temp = self.vars.next_temp(expr.ty);
                        block.push(Instr::Copy {
                            dest: temp,
                            src: Atom::Const(Const::Bool(false)),
                        });
                        let end = self.next_label();
                        let cond_jump = Instr::CondJump {
                            cond: Cond::IfFalse {
                                atom: self.atomize(block, lhs),
                            },
                            to: end,
                        };
                        block.push(cond_jump);
                        let cond_jump = Instr::CondJump {
                            cond: Cond::IfFalse {
                                atom: self.atomize(block, rhs),
                            },
                            to: end,
                        };
                        block.push(cond_jump);
                        block.push(Instr::Copy {
                            dest: temp,
                            src: Atom::Const(Const::Bool(true)),
                        });
                        block.push(Label { id: end });
                        return Atom::Var(temp);
                    }
                    ast::BinOp::LogicalOr => {
                        // lhs || rhs
                        //
                        // temp = true
                        // if lhs goto end
                        // if rhs goto end
                        // temp = false
                        // end:
                        // temp
                        let temp = self.vars.next_temp(expr.ty);
                        block.push(Instr::Copy {
                            dest: temp,
                            src: Atom::Const(Const::Bool(true)),
                        });
                        let end = self.next_label();
                        let cond_jump = Instr::CondJump {
                            cond: Cond::If {
                                atom: self.atomize(block, lhs),
                            },
                            to: end,
                        };
                        block.push(cond_jump);
                        let cond_jump = Instr::CondJump {
                            cond: Cond::If {
                                atom: self.atomize(block, rhs),
                            },
                            to: end,
                        };
                        block.push(cond_jump);
                        block.push(Instr::Copy {
                            dest: temp,
                            src: Atom::Const(Const::Bool(false)),
                        });
                        block.push(Label { id: end });
                        return Atom::Var(temp);
                    }
                    ast::BinOp::EqualTo => BinOp::Rel(RelOp::EqualTo),
                    ast::BinOp::NotEqualTo => BinOp::Rel(RelOp::NotEqualTo),
                    ast::BinOp::LessThan => BinOp::Rel(RelOp::LessThan),
                    ast::BinOp::GreaterThan => BinOp::Rel(RelOp::GreaterThan),
                    ast::BinOp::LessThanOrEqualTo => BinOp::Rel(RelOp::LessThanOrEqualTo),
                    ast::BinOp::GreaterThanOrEqualTo => BinOp::Rel(RelOp::GreaterThanOrEqualTo),
                };
                let temp = self.vars.next_temp(expr.ty);
                let lhs = self.atomize(block, lhs);
                let rhs = self.atomize(block, rhs);
                block.push(Instr::Bin {
                    op,
                    dest: temp,
                    lhs,
                    rhs,
                });
                Atom::Var(temp)
            }
            ExprKind::Call { func, args } => {
                let n = args.len();
                let temp = self.vars.next_temp(expr.ty);
                for arg in args {
                    let atom = self.atomize(block, arg);
                    block.push(Param { atom });
                }
                block.push(Instr::Call {
                    dest: temp,
                    func,
                    args: n as u32,
                });
                Atom::Var(temp)
            }
            ExprKind::Bool { val } => Atom::Const(Const::Bool(val)),
            ExprKind::Int { val } => Atom::Const(Const::Int(val)),
            ExprKind::Str => Atom::Const(Const::Str(expr.span)),
            ExprKind::Var { id } => Atom::Var(id),
        }
    }

    pub fn next_label(&mut self) -> LabelId {
        let label = LabelId(self.label_count);
        self.label_count += 1;
        label
    }
}

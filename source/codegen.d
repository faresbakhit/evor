import ast;
import x86;

import std.sumtype : match, get, has;
import std.typecons : Tuple;

Program compile(Mod mod)
{
    Homes homes;
    Program prog;
    foreach (stmt; mod)
    {
        compile(prog, stmt, homes);
    }
    return prog;
}

alias Homes = Operand*[ItemExpr];

private void compile(ref Program prog, Stmt* stmt, ref Homes homes)
{
    (*stmt).match!(
        (AssignStmt ass)
        {
            auto varOp = ass.var.op(homes, prog);
            (*ass.expr).match!(
                (LitExpr lit)
                {
                    prog ~= movq(op(&lit), varOp);
                },
                (ItemExpr item)
                {
                    auto itemOp = op(&item, homes, prog);
                    if (itemOp != varOp)
                    {
                        prog ~= [
                            movq(itemOp, Reg.rax.op()),
                            movq(Reg.rax.op(), varOp)
                        ];
                    }
                },
                (UnExpr u)
                {
                    final switch (u.op)
                    {
                        case UnOp.minus:
                            (*u.expr).match!(
                                (LitExpr lit)
                                {
                                    lit.match!(
                                        (IntLitExpr lit)
                                        {
                                            prog ~= movq(imm(-lit.val), varOp);
                                        }
                                    );
                                },
                                (ItemExpr item)
                                {
                                    auto itemOp = op(&item, homes, prog);
                                    if (varOp == itemOp)
                                    {
                                        prog ~= negq(varOp);
                                    }
                                    else
                                    {
                                        prog ~= [
                                            movq(itemOp, Reg.rax.op()),
                                            negq(Reg.rax.op()),
                                            movq(Reg.rax.op(), varOp),
                                        ];
                                    }
                                },
                                _ => assert(0),
                            );
                    }
                },
                (BinExpr bin)
                {
                    auto lhsOp = bin.lhs.op(homes, prog);
                    auto rhsOp = bin.rhs.op(homes, prog);
                    if ((varOp == lhsOp && (*bin.lhs).has!LitExpr))
                    {
                        prog ~= addq(lhsOp, varOp);
                    }
                    else if ((varOp == lhsOp && (*bin.lhs).has!LitExpr))
                    {
                        prog ~= addq(rhsOp, varOp);
                    }
                    else
                    {
                        prog ~= [
                            movq(lhsOp, Reg.rax.op()),
                            binInstr(bin.op.binInstrType(), rhsOp, Reg.rax.op()),
                            movq(Reg.rax.op(), varOp)
                        ];
                    }
                },
                (CallExpr call)
                {
                    if (call.func.name == "input_int")
                    {
                        if (call.args.length != 0)
                        {
                            assert(0);
                        }
                        prog ~= [
                            callq(label("read_int")),
                            movq(Reg.rax.op(), varOp),
                        ];
                    }
                }
            );
        },
        (Expr* expr)
        {
            
            (*expr).match!(
                (CallExpr call)
                {
                    if (call.func.name == "print")
                    {
                        if (call.args.length != 1)
                        {
                            assert(0);
                        }
                        auto argOp = call.args[0].op(homes, prog);
                        prog ~= [
                            movq(argOp, Reg.rdi.op()),
                            callq(label("print_int")),
                        ];
                    }
                    else if (call.func.name == "input_int")
                    {
                        if (call.args.length != 0)
                        {
                            assert(0);
                        }
                        prog ~= callq(label("read_int"));
                    }
                },
                _ => assert(0)
            );
        }
    );
}

private Operand* op(Expr* expr, ref Homes homes, ref Program prog)
{
    return (*expr).match!(
        (LitExpr l)
        {
            return (&l).op();
        },
        (ItemExpr i)
        {
            return (&i).op(homes, prog);
        },
        _ => assert(0),
    );
}

private Operand* op(ItemExpr* item, ref Homes homes, ref Program prog)
{
    auto opp = (*item in homes);
    if (opp is null)
    {
        prog.stackSize += 8;
        auto op = deref(Reg.rbp, -prog.stackSize);
        homes[*item] = op;
        return op;
    }
    return *opp;
}

private Operand* op(LitExpr* lit)
{
    return (*lit).match!(
        (IntLitExpr lit)
        {
            return imm(lit.val);
        }
    );
}

private Operand* op(Reg reg) {
    return new Operand(reg);
}

private BinInstrType binInstrType(BinOp op)
{
    final switch (op)
    {
        case BinOp.add:
            return BinInstrType.addq;
        case BinOp.sub:
            return BinInstrType.subq;
    }
}

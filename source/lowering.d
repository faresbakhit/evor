import ast;

import std.sumtype : match;
import std.typecons : Tuple;
import std.conv : to;
import std.array : insertInPlace;

ref Mod flatten(return ref Mod mod)
{
    uint tempCount = 0;
    size_t stmtIndex = 0;
    while (stmtIndex != mod.length)
    {
        flatten(mod, stmtIndex, tempCount);
    }
    return mod;
}

private void flatten(ref Mod mod, ref size_t stmtIndex, ref uint tempCount)
{
    (*mod[stmtIndex]).match!(
        (ref AssignStmt ass)
        {
            ass.expr = flatten(mod, stmtIndex, ass.expr, tempCount);
            stmtIndex += 1;
        },
        (ref Expr* expr)
        {
            expr = flatten(mod, stmtIndex, expr, tempCount);
            stmtIndex += 1;
        }
    );
}

private Expr* flatten(ref Mod mod, ref size_t stmtIndex, Expr* expr, ref uint tempCount)
{
    return (*expr).match!(
        (LitExpr _)
        {
            return expr;
        },
        (ItemExpr _)
        {
            return expr;
        },
        (UnExpr u)
        {
            if (u.expr.is_atomic())
            {
                return expr;
            }
            else
            {
                import std.stdio;
                auto d = flatten(mod, stmtIndex, u.expr, tempCount);
                auto t = nextTemp(tempCount);
                mod.insertInPlace(stmtIndex, assign(t, d));
                stmtIndex += 1;
                return un(u.op, t);
            }
        },
        (BinExpr b)
        {
            bool lhs_is_atomic = b.lhs.is_atomic();
            bool rhs_is_atomic = b.rhs.is_atomic();
            if (lhs_is_atomic && rhs_is_atomic)
            {
                return expr;
            }
            else if (lhs_is_atomic)
            {
                auto d = flatten(mod, stmtIndex, b.rhs, tempCount);
                auto t = nextTemp(tempCount);
                mod.insertInPlace(stmtIndex, assign(t, d));
                stmtIndex += 1;
                return bin(b.op, b.lhs, t);
            }
            else if (rhs_is_atomic)
            {
                auto d = flatten(mod, stmtIndex, b.lhs, tempCount);
                auto t = nextTemp(tempCount);
                mod.insertInPlace(stmtIndex, assign(t, d));
                stmtIndex += 1;
                return bin(b.op, t, b.rhs);
            }
            else
            {
                auto d_lhs = flatten(mod, stmtIndex, b.lhs, tempCount);
                auto t_lhs = nextTemp(tempCount);
                auto d_rhs = flatten(mod, stmtIndex, b.rhs, tempCount);
                auto t_rhs = nextTemp(tempCount);
                mod.insertInPlace(stmtIndex, assign(t_lhs, d_lhs));
                mod.insertInPlace(stmtIndex + 1, assign(t_rhs, d_rhs));
                stmtIndex += 2;
                return bin(b.op, t_lhs, t_rhs);
            }
        },
        (CallExpr c)
        {
            foreach (ref a; c.args)
            {
                if (!a.is_atomic())
                {
                    auto d = flatten(mod, stmtIndex, a, tempCount);
                    auto t = nextTemp(tempCount);
                    mod.insertInPlace(stmtIndex, assign(t, d));
                    stmtIndex += 1;
                    a = t;
                }
            }
            return expr;
        }
    );
}

private bool is_atomic(Expr* expr)
{
    return (*expr).match!(
        (ItemExpr _) => true,
        (LitExpr _) => true,
        _ => false,
    );
}

private Expr* nextTemp(ref uint tempCount)
{
    auto item = temp(tempCount);
    tempCount += 1;
    return item;
}

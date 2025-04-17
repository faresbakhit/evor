import ast;

import std.sumtype : match;
import std.stdio : writeln;
import std.conv : to;

void interpret(Mod m)
{
    long[string] env;
    foreach (s; m)
    {
        (*s).match!(
            (AssignStmt assign) => env[(*assign.var).match!(
                (ItemRefExpr item) => item.name,
                (TempItemExpr item) => "#" ~ to!string(item.num),
            )] = eval(*assign.expr, env),
            (Expr* e) => eval(*e, env),
        );
    }
}

private long eval(Expr e, long[string] env)
{
    return e.match!(
        (LitExpr lit) => lit.match!(
            (IntLitExpr lit) => lit.val,
        ),
        (ItemExpr item) => env[item.match!(
            (ItemRefExpr item) => item.name,
            (TempItemExpr item) => "#" ~ to!string(item.num)
        )],
        (BinExpr bin)
        {
            long lhs = eval(*bin.lhs, env);
            long rhs = eval(*bin.rhs, env);
            final switch (bin.op)
            {
                case BinOp.add:
                    return lhs + rhs;
                case BinOp.sub:
                    return lhs - rhs;
            }
        },
        (UnExpr un)
        {
            long val = eval(*un.expr, env);
            final switch (un.op)
            {
                case UnOp.minus:
                    return -val;
            }
        },
        (CallExpr call)
        {
            if (call.func.name == "print" && call.args.length == 1)
            {
                writeln(eval(*call.args[0], env));
            }
            return 0;
        }
    );
}

import traits : isType;

import std.functional : partial;
import std.traits : EnumMembers, isPointer;
import std.typecons : Tuple;
import std.sumtype : SumType, This;
import std.meta : staticMap, allSatisfy, anySatisfy, templateOr;
import std.conv : to;

enum BinOp {
    add,
    sub,
}

enum UnOp {
    minus,
}

alias IntLitExpr = Tuple!(long, "val");
alias LitExpr = SumType!(IntLitExpr);
alias TempItemExpr = Tuple!(int, "num");
alias ItemRefExpr = Tuple!(string, "name");
alias ItemExpr = SumType!(TempItemExpr, ItemRefExpr);
alias Expr = SumType!(
    LitExpr,
    ItemExpr,
    /* UnExpr */ Tuple!(UnOp, "op", This*, "expr"),
    /* BinExpr */ Tuple!(BinOp, "op", This*, "lhs", This*, "rhs"),
    /* CallExpr */ Tuple!(ItemRefExpr*, "func", This*[], "args"),
);
alias UnExpr = Expr.Types[2];
alias BinExpr = Expr.Types[3];
alias CallExpr = Expr.Types[4];
alias AssignStmt = Tuple!(ItemExpr*, "var", Expr*, "expr");
alias Stmt = SumType!(
    Expr*,
    AssignStmt,
);
alias Mod = Stmt*[];

Expr* lit(long val)
{
    return new Expr(LitExpr(IntLitExpr(val)));
}

Expr* item(string name)
{
    return new Expr(ItemExpr(ItemRefExpr(name)));
}

Expr* temp(uint num)
{
    return new Expr(ItemExpr(TempItemExpr(num)));
}

Expr* bin(BinOp op, Expr* lhs, Expr* rhs)
{
    return new Expr(BinExpr(op, lhs, rhs));
}

static foreach (op; EnumMembers!BinOp)
    mixin("alias " ~ to!string(op) ~ "=partial!(bin,BinOp." ~ to!string(op) ~ ");");

Expr* un(UnOp op, Expr* expr)
{
    return new Expr(UnExpr(op, expr));
}

static foreach (op; EnumMembers!UnOp)
    mixin("alias " ~ to!string(op) ~ "=partial!(un,UnOp." ~ to!string(op) ~ ");");

Expr* call(ItemRefExpr* item, Expr*[] args)
{
    return new Expr(CallExpr(item, args));
}

Expr* call(T...)(ItemRefExpr* item, T args)
{
    return call(item, [args]);
}

Expr* call(T...)(ItemExpr* item, T args)
{
    import std.sumtype : get;
    return call(&(*item).get!(ItemRefExpr), args);
}

Expr* call(T...)(Expr* item, T args)
{
    import std.sumtype : get;
    return call(&(*item).get!(ItemExpr), args);
}

Stmt* stmt(Expr* x)
{
    return new Stmt(x);
}

Stmt* assign(ItemExpr* item, Expr* expr)
{
    return new Stmt(AssignStmt(item, expr));
}

Stmt* assign(Expr* itemExpr, Expr* expr)
{
    import std.sumtype : get;
    return assign(&(*itemExpr).get!(ItemExpr), expr);
}

Mod mod(T...)(T stmts)
if (allSatisfy!(isType!(Stmt*), T))
{
    return [stmts];
}

Mod mod(T...)(T stmts)
if (anySatisfy!(isType!(Expr*), T))
{
    Mod m;
    static foreach (s; stmts)
        static if (is(typeof(s) == Expr*))
            m ~= s.stmt();
        else
            m ~= s;
    return m;
}

private string pprint(ItemRefExpr* item, uint indent)
{
    import std.format : format;
    return "ItemExpr(name: %s)".format(item.name);
}

private string pprint(ItemExpr* item, uint indent)
{
    import std.sumtype : match;
    import std.format : format;
    return (*item).match!(
        (TempItemExpr item) => "TempItemExpr(num: %s)".format(item.num),
        (ItemRefExpr item) => pprint(&item, indent),
    );
}

private string pprint(Expr* expr, uint indent)
{
    import std.sumtype : match;
    import std.format : format;
    import std.algorithm : map, joiner;
    import std.range : repeat, only;
    import std.array : join;
    return (*expr).match!(
        (LitExpr lit) => "LitExpr(%s)".format(lit.match!(
            (IntLitExpr lit) => "IntLitExpr(%d)".format(lit.val),
        )),
        (ItemExpr item) => pprint(&item, indent),
        (UnExpr un) => "UnExpr(\n%3$sop: %1$s,\n%3$sexpr: %2$s\n%4$s)".format(
            un.op,
            pprint(un.expr, indent + 1),
            repeat("  ", indent + 1).join,
            repeat("  ", indent).join,
        ),
        (BinExpr bin) => "BinExpr(\n%4$sop: %1$s,\n%4$slhs: %2$s,\n%4$srhs: %3$s\n%5$s)".format(
            bin.op,
            pprint(bin.lhs, indent + 1),
            pprint(bin.rhs, indent + 1),
            repeat("  ", indent + 1).join,
            repeat("  ", indent).join,
        ),
        (CallExpr call) => "CallExpr(\n%3$sfunc: %1$s,\n%3$sargs: [\n%5$s%2$s\n%3$s]\n%4$s)".format(
            pprint(call.func, indent + 1),
            call.args.map!(a => pprint(a, indent + 2)).joiner(",\n" ~ repeat("  ", indent + 2).join),
            repeat("  ", indent + 1).join,
            repeat("  ", indent).join,
            repeat("  ", indent + 2).join,
        )
    );
}

private string pprint(Stmt* stmt, uint indent)
{
    import std.sumtype : match;
    import std.format : format;
    import std.range : repeat;
    import std.array : join;
    return (*stmt).match!(
        (Expr* expr) => "Expr(%s)".format(pprint(expr, indent)),
        (AssignStmt assign) => "AssignStmt(\n%3$svar: %1$s,\n%3$sexpr: %2$s\n%4$s)".format(
            pprint(assign.var, indent + 1),
            pprint(assign.expr, indent + 1),
            repeat("  ", indent + 1).join,
            repeat("  ", indent).join,
        ),
    );
}

string pprint(Mod mod)
{
    import std.format : format;
    import std.algorithm : map, joiner;
    return "Mod([\n  %s\n])".format(
        mod.map!(a => pprint(a, 1)).joiner(",\n  "),
    );
}

string pprint(T)(T expr_or_stmt)
if(is(T == Expr) || is(T == Stmt))
{
    return pprint(expr_or_stmt, 0);
}

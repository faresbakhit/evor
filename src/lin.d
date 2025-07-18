/**
 * Defines a linear AST for the Evor language where operands are
 * restricted to atomic expressions and handles the transformation
 * of Evor's AST to it.
 */

module evorc.lin;

import evorc.span;
import evorc.ast;

import std.bigint;
import std.sumtype;
import std.typecons;

alias LinProgram = LinProgramItem[];
alias LinProgramItem = SumType!(LinFuncDecl, LinFunc);
alias LinFuncDecl = Tuple!(string, "name", LinType*, "retType", LinParam[], "params");
alias LinParam = Tuple!(LinType*, "type", Nullable!LinVar, "name");
alias LinFunc = Tuple!(LinFuncDecl, "decl", LinBlock, "block");
alias LinBlock = LinStmt*[];
alias LinStmt = SumType!(
    Tuple!(LinExpr, "cond", This*[], "ifBlock", This*[], "elseBlock"), // LinIf
    LinAssign,
    LinReturn,
    LinExpr,
);
alias LinIf = LinStmt.Types[0];
alias LinAssign = Tuple!(LValue, "lhs", LinExpr, "rhs");
alias LValue = SumType!(LinVar, Deref);
alias Deref = Tuple!(LinVar, "ref_");
alias LinReturn = Tuple!(Nullable!LinExpr, "expr");
alias LinExpr = SumType!(LinBool, LinInt, LinVar, LinUn, LinBin, LinCall);
alias Atom = SumType!(LinBool, LinInt, LinVar);
alias LinBool = Tuple!(LinType*, "type", bool, "value");
alias LinInt = Tuple!(LinType*, "type", BigInt, "value");
alias LinVar = Tuple!(LinType*, "type", uint, "id");
alias LinType = SumType!(
    Tuple!(This*, "pointee"), // LinPointer
    PrimitiveType,
);
alias LinPointer = LinType.Types[0];
alias LinUn = Tuple!(LinType*, "type", UnOp, "op", Atom, "expr");
alias LinBin = Tuple!(LinType*, "type", BinOp, "op", Atom, "lhs", Atom, "rhs");
alias LinCall = Tuple!(LinType*, "type", string, "func", Atom[], "args");

public import evorc.utils.sumtype : type = firstField;

import std.algorithm;
import std.array;
import std.conv;

alias Err = Tuple!(string, "message", Span, "span");

import evorc.utils.result : ResultWith, collect;

alias Result = ResultWith!(Err);
auto result(T)(T ok) => Result!T(ok);
auto result(T)(Err err) => Result!T(err);

import evorc.utils.unit;

Result!LinProgram lin(Program prog)
{
    return prog.map!(match!lin).collect;
};

Result!LinProgramItem lin(Func fn)
{
    alias T = LinProgramItem;
    VarRecord rec;
    auto fnDecl = lin(fn.decl, rec);
    auto block = lin(fn.block, rec);
    if (block.isErr) return block.err.result!T;
    return LinProgramItem(LinFunc(fnDecl, block.get)).result;
}

Result!LinProgramItem lin(FuncDecl fnDecl)
{
    VarRecord rec;
    return LinProgramItem(lin(fnDecl, rec)).result;
}

LinFuncDecl lin(FuncDecl fnDecl, ref VarRecord rec)
{
    LinParam[] params;
    foreach (param; fnDecl.params)
    {
        if (!param.ident.isNull)
        {
            auto var = rec.put(param.ident.get.name, lin(param.type));
            params ~= LinParam(var.type, var.nullable);
        }
        else
        {
            params ~= LinParam(lin(param.type), Nullable!LinVar.init);
        }
    }
    return LinFuncDecl(fnDecl.ident.name, new LinType(PrimitiveType.int_), params);
}

LinType* lin(Type* type) => (*type).match!(
    (Pointer ptr) => new LinType(LinPointer(lin(ptr.pointee))),
    (Primitive primitive) => new LinType(primitive.type),
);

Result!LinBlock lin(Block block, ref VarRecord rec)
{
    alias T = LinBlock;
    LinBlock linBlock;
    foreach (stmt; block)
    {
        auto res = lin(stmt, linBlock, rec);
        if (res.isErr) return res.err.result!T;
    }
    return linBlock.result;
}

Result!Unit lin(Stmt* stmt, ref LinBlock block, ref VarRecord rec)
{
    return (*stmt).match!(s => lin(s, block, rec));
}

Result!Unit lin(If if_, ref LinBlock block, ref VarRecord rec)
{
    return unit.result;
}

Result!Unit lin(Return ret, ref LinBlock block, ref VarRecord rec)
{
    alias T = Unit;
    if (ret.expr.isNull) return unit.result;
    auto expr = lin(ret.expr.bitCast!(Expr*), block, rec);
    if (expr.isErr) return expr.err.result!T;
    block ~= new LinStmt(LinReturn(expr.get.nullable));
    return unit.result;
}

Result!Unit lin(VarDecl varDecl, ref LinBlock block, ref VarRecord rec)
{
    alias T = Unit;
    auto var = rec.put(varDecl.ident.name, lin(varDecl.type));
    if (varDecl.def.isNull) return unit.result;
    auto expr = lin(varDecl.def.bitCast!(Expr*), block, rec);
    if (expr.isErr) return expr.err.result!T;
    block ~= new LinStmt(LinAssign(LValue(var), expr.get));
    return unit.result;
}

Result!Unit lin(Assign assign, ref LinBlock block, ref VarRecord rec)
{
    alias T = Unit;
    auto lhsExpr = *assign.lhs;
    if (lhsExpr.has!Ident)
    {
        auto var = rec.get(lhsExpr.get!Ident);
        if (var.isErr) return var.err.result!T;
        auto rhs = lin(assign.rhs, block, rec);
        if (rhs.isErr) return rhs.err.result!T;
        block ~= new LinStmt(LinAssign(LValue(var.get), rhs.get));
    }
    else if (lhsExpr.has!Un && lhsExpr.get!Un.op == UnOp.pointerDereference)
    {
        auto expr = *lhsExpr.get!Un.expr;
        if (expr.has!Ident)
        {
            auto var = rec.get(expr.get!Ident);
            if (var.isErr) return var.err.result!T;
            auto rhs = lin(assign.rhs, block, rec);
            if (rhs.isErr) return rhs.err.result!T;
            block ~= new LinStmt(LinAssign(LValue(Deref(var.get)), rhs.get));
        }
        else
        {
            auto rLinExpr = lin(lhsExpr.get!Un.expr, block, rec);
            if (rLinExpr.isErr) return rLinExpr.err.result!T;
            auto linExpr = rLinExpr.get;
            auto var = rec.next(linExpr.type);
            block ~= new LinStmt(LinAssign(LValue(var), linExpr));
            auto rhs = lin(assign.rhs, block, rec);
            if (rhs.isErr) return rhs.err.result!T;
            block ~= new LinStmt(LinAssign(LValue(Deref(var)), rhs.get));
        }
    }
    else
    {
        return Err("expression must be an lvalue", assign.lhs.span).result!T;
    }
    return unit.result;
}

Result!Unit lin(StmtExpr stmtExpr, ref LinBlock block, ref VarRecord rec)
{
    alias T = Unit;
    if (stmtExpr.isNull) return unit.result;
    auto expr = lin(stmtExpr.bitCast!(Expr*), block, rec);
    if (expr.isErr) return expr.err.result!T;
    block ~= new LinStmt(expr.get);
    return unit.result;
}


Result!LinExpr lin(Expr* expr, ref LinBlock block, ref VarRecord rec)
{
    return (*expr).match!(e => lin(e, block, rec));
}

Result!LinExpr lin(Bool bool_, ref LinBlock block, ref VarRecord rec)
{
    return LinExpr(LinBool(new LinType(PrimitiveType.bool_), bool_.value)).result;
}

Result!LinExpr lin(Int int_, ref LinBlock block, ref VarRecord rec)
{
    return LinExpr(LinInt(new LinType(PrimitiveType.int_), int_.value)).result;
}

Result!LinExpr lin(Ident ident, ref LinBlock block, ref VarRecord rec)
{
    alias T = LinExpr;
    auto var = rec.get(ident);
    if (var.isErr) return var.err.result!T;
    return LinExpr(var.get).result;
}

Result!LinExpr lin(Un un, ref LinBlock block, ref VarRecord rec)
{
    alias T = LinExpr;
    return (*un.expr).match!(
        (Bool bool_)
        {
            auto type = new LinType(PrimitiveType.bool_);
            return LinExpr(LinUn(un.op.on(type), un.op, Atom(LinBool(type, bool_.value)))).result;
        },
        (Int int_)
        {
            auto type = new LinType(PrimitiveType.int_);
            return LinExpr(LinUn(un.op.on(type), un.op, Atom(LinInt(type, int_.value)))).result;
        },
        (Ident ident)
        {
            auto var = rec.get(ident);
            if (var.isErr) return var.err.result!T;
            return LinExpr(LinUn(un.op.on(var.get.type), un.op, Atom(var.get))).result;
        },
        (_)
        {
            auto expr = lin(un.expr, block, rec);
            if (expr.isErr) return expr.err.result!T;
            auto var = rec.next(type(expr.get));
            block ~= new LinStmt(LinAssign(LValue(var), expr.get));
            return LinExpr(LinUn(un.op.on(var.type), un.op, Atom(var))).result;
        }
    );
}

Result!LinExpr lin(Bin bin, ref LinBlock block, ref VarRecord rec)
{
    alias T = LinExpr;
    bool lhsIsAtomic = bin.lhs.isAtomic();
    bool rhsIsAtomic = bin.rhs.isAtomic();
    if (lhsIsAtomic && rhsIsAtomic)
    {
        auto rLeft = bin.lhs.atom(rec);
        if (rLeft.isErr) return rLeft.err.result!T;
        auto left = rLeft.get;

        auto rRight = bin.rhs.atom(rec);
        if (rRight.isErr) return rRight.err.result!T;
        auto right = rRight.get;

        auto type = bin.op.on(left.type, right.type);
        return LinExpr(LinBin(type, bin.op, left, right)).result;
    }
    else if (lhsIsAtomic)
    {
        auto rLeft = bin.lhs.atom(rec);
        if (rLeft.isErr) return rLeft.err.result!T;
        auto left = rLeft.get;

        auto rRightExpr = lin(bin.rhs, block, rec);
        if (rRightExpr.isErr) return rRightExpr.err.result!T;
        auto rightExpr = rRightExpr.get;
        auto rightVar = rec.next(rightExpr.type);
        block ~= new LinStmt(LinAssign(LValue(rightVar), rightExpr));

        auto type = bin.op.on(left.type, rightVar.type);
        return LinExpr(LinBin(type, bin.op, left, Atom(rightVar))).result;
    }
    else if (rhsIsAtomic)
    {
        auto rLeftExpr = lin(bin.lhs, block, rec);
        if (rLeftExpr.isErr) return rLeftExpr.err.result!T;
        auto leftExpr = rLeftExpr.get;
        auto leftVar = rec.next(leftExpr.type);
        block ~= new LinStmt(LinAssign(LValue(leftVar), leftExpr));

        auto rRight = bin.rhs.atom(rec);
        if (rRight.isErr) return rRight.err.result!T;
        auto right = rRight.get;

        auto type = bin.op.on(right.type, leftVar.type);
        return LinExpr(LinBin(type, bin.op, Atom(leftVar), right)).result;
    }
    else
    {
        auto rLeftExpr = lin(bin.lhs, block, rec);
        if (rLeftExpr.isErr) return rLeftExpr.err.result!T;
        auto leftExpr = rLeftExpr.get;
        auto leftVar = rec.next(leftExpr.type);
        block ~= new LinStmt(LinAssign(LValue(leftVar), leftExpr));

        auto rRightExpr = lin(bin.rhs, block, rec);
        if (rRightExpr.isErr) return rRightExpr.err.result!T;
        auto rightExpr = rRightExpr.get;
        auto rightVar = rec.next(rightExpr.type);
        block ~= new LinStmt(LinAssign(LValue(rightVar), rightExpr));

        auto type = bin.op.on(rightVar.type, leftVar.type);
        return LinExpr(LinBin(type, bin.op, Atom(leftVar), Atom(rightVar))).result;
    }
}

Result!LinExpr lin(Call call, ref LinBlock block, ref VarRecord rec)
{
    return assert(0);
}

private Result!Atom atom(Expr* expr, ref VarRecord rec) => (*expr).match!(
    (Bool bool_) => Atom(LinBool(new LinType(PrimitiveType.bool_), bool_.value)).result,
    (Int int_) => Atom(LinInt(new LinType(PrimitiveType.bool_), int_.value)).result,
    (Ident ident)
    {
        alias T = Atom;
        auto var = rec.get(ident);
        if (var.isErr) return var.err.result!T;
        return Atom(var.get).result;
    },
    _ => assert(0, "expression not atomic"),
);

private bool isAtomic(const(Expr*) expr)
{
    return (*expr).match!(
        (Bool _) => true,
        (Int _) => true,
        (Ident _) => true,
        _ => false,
    );
}

private LinType* on(BinOp binOp, LinType* lhsType, LinType* rhsType)
{
    with (BinOp) final switch (binOp)
    {
    case logicalOr:
    case logicalAnd:
    case equalTo:
    case notEqualTo:
    case lessThan:
    case greaterThan:
    case lessThanOrEqualTo:
    case greaterThanOrEqualTo:
        return new LinType(PrimitiveType.bool_);
    case bitwiseOr:
    case bitwiseXor:
    case bitwiseAnd:
    case bitwiseLeftShift:
    case bitwiseRightShift:
    case add:
    case sub:
    case mul:
    case div:
    case rem:
        return lhsType;//FIXME promotions yada yada
    case arraySubscript:
        return null;
    case memberAccess:
    case memberAccessThroughPointer:
        return rhsType;
    }
}

private LinType* on(UnOp unOp, LinType* type)
{
    with (UnOp) final switch (unOp)
    {
    case plus:
    case minus:
    case bitwiseNot:
        return type;
    case logicalNot:
        return new LinType(PrimitiveType.bool_);
    case pointerDereference:
        return (*type).match!(
            (PrimitiveType primitiveType) => assert(0, "pointer derference on a primitive type"),
            (LinPointer pointer) => pointer.pointee,
        );
    case addressOf:
        return new LinType(LinPointer(type));
    }
}

struct VarRecord
{
    LinVar put(string name, LinType* type)
    {
        auto var = LinVar(type, count++);
        m[name] = var;
        return var;
    }

    Nullable!LinVar get(string name)
    {
        auto var = name in m;
        if (var !is null) return nullable(*var);
        return Nullable!LinVar.init;
    }

    Result!LinVar get(Ident ident)
    {
        alias T = LinVar;
        auto var = get(ident.name);
        if (var.isNull) return Err("variable not defined", ident.span).result!T;
        return var.get.result;
    }

    LinVar next(LinType* type) => LinVar(type, count++);

    private LinVar[string] m;
    private uint count = 0;
}

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

alias Err = Tuple!(string, "message", Span, "span");

import evorc.utils.result : ResultWith, collect;

alias Result = ResultWith!(Err);
auto result(T)(T ok) => Result!T(ok);
auto result(T)(Err err) => Result!T(err);

import evorc.utils.unit;
import evorc.utils.sumtype;
import evorc.display;

import std.algorithm;
import std.array;
import std.conv;
import std.format;

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
    return LinFuncDecl(fnDecl.ident.name, lin(fnDecl.retType), params);
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
    alias T = Unit;
    auto exprEx = lin(if_.cond, block, rec);
    if (exprEx.isErr) return exprEx.err.result!T;
    auto expr = exprEx.get;
    auto exprType = expr.type;
    if (!exprType.contains(PrimitiveType.bool_))
        return Err("expected `bool`, found `%s`".format(display(exprType)), if_.cond.span).result!T;
    auto ifBlock = lin(if_.ifBlock, rec);
    if (ifBlock.isErr) return ifBlock.err.result!T;
    auto elseBlock = lin(if_.elseBlock, rec);
    if (elseBlock.isErr) return elseBlock.err.result!T;
    block ~= new LinStmt(LinIf(expr, ifBlock.get, elseBlock.get));
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
    auto def = varDecl.def.bitCast!(Expr*);
    auto maybeExpr = lin(def, block, rec);
    if (maybeExpr.isErr) return maybeExpr.err.result!T;
    auto expr = maybeExpr.get;
    if (!expr.type.equalTo(var.type))
        return Err("`%s` not assignable to `%s`"
                   .format(expr.type.display, var.type.display), def.span)
               .result!T;
    block ~= new LinStmt(LinAssign(LValue(var), expr));
    return unit.result;
}

Result!Unit lin(Assign assign, ref LinBlock block, ref VarRecord rec)
{
    alias T = Unit;
    auto lhsExpr = *assign.lhs;
    if (lhsExpr.has!Ident)
    {
        auto maybeVar = rec.get(lhsExpr.get!Ident);
        if (maybeVar.isErr) return maybeVar.err.result!T;
        auto var = maybeVar.get;

        auto maybeExpr = lin(assign.rhs, block, rec);
        if (maybeExpr.isErr) return maybeExpr.err.result!T;
        auto expr = maybeExpr.get;

        if (!var.type.equalTo(expr.type)) return Err("`%s` not assignable to `%s`"
                                                     .format(expr.type.display, var.type.display), assign.rhs.span)
                                                 .result!T;

        block ~= new LinStmt(LinAssign(LValue(var), expr));
    }
    else if (lhsExpr.has!Un && lhsExpr.get!Un.op == UnOp.pointerDereference)
    {
        auto lhsDerefExpr = *lhsExpr.get!Un.expr;
        if (lhsDerefExpr.has!Ident)
        {
            auto maybeVar = rec.get(lhsDerefExpr.get!Ident);
            if (maybeVar.isErr) return maybeVar.err.result!T;
            auto var = maybeVar.get;

            if (!(*var.type).has!LinPointer)
                return Err("dereference of primitive type `%s`"
                           .format(var.type.display), assign.lhs.span)
                       .result!T;

            auto maybeExpr = lin(assign.rhs, block, rec);
            if (maybeExpr.isErr) return maybeExpr.err.result!T;
            auto expr = maybeExpr.get;

            auto pointeeType = (*var.type).get!LinPointer.pointee;
            if (!pointeeType.equalTo(expr.type))
                return Err("`%s` not assignable to `%s`"
                           .format(expr.type.display, pointeeType.display), assign.rhs.span)
                       .result!T;

            block ~= new LinStmt(LinAssign(LValue(Deref(var)), expr));
        }
        else
        {
            auto maybeTempExpr = lin(lhsExpr.get!Un.expr, block, rec);
            if (maybeTempExpr.isErr) return maybeTempExpr.err.result!T;
            auto tempExpr = maybeTempExpr.get;

            auto var = rec.next(tempExpr.type);
            block ~= new LinStmt(LinAssign(LValue(var), tempExpr));

            if (!(*var.type).has!LinPointer)
                return Err("dereference of primitive type `%s`"
                           .format(var.type.display), assign.lhs.span)
                       .result!T;

            auto maybeExpr = lin(assign.rhs, block, rec);
            if (maybeExpr.isErr) return maybeExpr.err.result!T;
            auto expr = maybeExpr.get;

            auto pointeeType = (*var.type).get!LinPointer.pointee;
            if (!pointeeType.equalTo(expr.type))
                return Err("`%s` not assignable to `%s`"
                           .format(expr.type.display, pointeeType.display), assign.rhs.span)
                       .result!T;

            block ~= new LinStmt(LinAssign(LValue(Deref(var)), expr));
        }
    }
    else
    {
        return Err("expression is not assignable", assign.lhs.span).result!T;
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
            auto exprType = new LinType(PrimitiveType.bool_);
            auto unExprType = un.op.on(exprType, un.span);
            if (unExprType.isErr) return unExprType.err.result!T;
            return LinExpr(LinUn(unExprType.get, un.op, Atom(LinBool(exprType, bool_.value)))).result;
        },
        (Int int_)
        {
            auto exprType = new LinType(PrimitiveType.int_);
            auto unExprType = un.op.on(exprType, un.span);
            if (unExprType.isErr) return unExprType.err.result!T;
            return LinExpr(LinUn(unExprType.get, un.op, Atom(LinInt(exprType, int_.value)))).result;
        },
        (Ident ident)
        {
            auto var = rec.get(ident);
            if (var.isErr) return var.err.result!T;
            auto type = un.op.on(var.get.type, un.span);
            if (type.isErr) return type.err.result!T;
            return LinExpr(LinUn(type.get, un.op, Atom(var.get))).result;
        },
        (_)
        {
            auto expr = lin(un.expr, block, rec);
            if (expr.isErr) return expr.err.result!T;
            auto var = rec.next(type(expr.get));
            auto type = un.op.on(var.type, un.span);
            if (type.isErr) return type.err.result!T;
            block ~= new LinStmt(LinAssign(LValue(var), expr.get));
            return LinExpr(LinUn(type.get, un.op, Atom(var))).result;
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

        auto type = bin.op.on(left.type, right.type, bin.lhs.span, bin.rhs.span);
        if (type.isErr) return type.err.result!T;
        return LinExpr(LinBin(type.get, bin.op, left, right)).result;
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

        auto type = bin.op.on(left.type, rightVar.type, bin.lhs.span, bin.rhs.span);
        if (type.isErr) return type.err.result!T;
        return LinExpr(LinBin(type.get, bin.op, left, Atom(rightVar))).result;
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

        auto type = bin.op.on(right.type, leftVar.type, bin.lhs.span, bin.rhs.span);
        if (type.isErr) return type.err.result!T;
        return LinExpr(LinBin(type.get, bin.op, Atom(leftVar), right)).result;
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

        auto type = bin.op.on(rightVar.type, leftVar.type, bin.lhs.span, bin.rhs.span);
        if (type.isErr) return type.err.result!T;
        return LinExpr(LinBin(type.get, bin.op, Atom(leftVar), Atom(rightVar))).result;
    }
}

Result!LinExpr lin(Call call, ref LinBlock block, ref VarRecord rec)
{
    return assert(0);
}

private Result!Atom atom(Expr* expr, ref VarRecord rec) => (*expr).match!(
    (Bool bool_) => Atom(LinBool(new LinType(PrimitiveType.bool_), bool_.value)).result,
    (Int int_) => Atom(LinInt(new LinType(PrimitiveType.int_), int_.value)).result,
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

private Result!(LinType*) on(BinOp binOp, LinType* lhsType, LinType* rhsType, Span lhsSpan, Span rhsSpan)
{
    alias T = LinType*;
    with (BinOp) final switch (binOp)
    {
    case logicalOr:
    case logicalAnd:
        auto lhsIsBool = lhsType.contains(PrimitiveType.bool_);
        auto rhsIsBool = rhsType.contains(PrimitiveType.bool_);
        if (!(lhsIsBool && rhsIsBool))
        {
            return Err(
                "expected `bool` %s `bool`, found `%s` %s `%s`".format(
                binOp.display,
                lhsType.display,
                binOp.display,
                rhsType.display,
                ),
                lhsSpan.joinSpans(rhsSpan)).result!T;
        }
        goto case;
    case equalTo:
    case notEqualTo:
        return result(new LinType(PrimitiveType.bool_));
    case lessThan:
    case greaterThan:
    case lessThanOrEqualTo:
    case greaterThanOrEqualTo:
        auto lhsIsInt = lhsType.contains(PrimitiveType.int_);
        auto rhsIsInt = rhsType.contains(PrimitiveType.int_);
        auto lhsIsPointer = (*lhsType).has!LinPointer;
        auto rhsIsPointer = (*rhsType).has!LinPointer;
        if ((lhsIsPointer && rhsIsPointer)
            || (lhsIsInt && rhsIsInt))
            return result(new LinType(PrimitiveType.bool_));
        return Err(
            "operation `%s` not permitted on `%s` and `%s`".format(
            binOp.display,
            lhsType.display, rhsType.display,
            ),
            lhsSpan.joinSpans(rhsSpan)).result!T;
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
        auto lhsIsBool = lhsType.contains(PrimitiveType.bool_);
        auto rhsIsBool = rhsType.contains(PrimitiveType.bool_);
        auto lhsIsInt = lhsType.contains(PrimitiveType.int_);
        auto rhsIsInt = rhsType.contains(PrimitiveType.int_);
        auto lhsIsPointer = (*lhsType).has!LinPointer;
        auto rhsIsPointer = (*rhsType).has!LinPointer;
        if ((rhsIsPointer && lhsIsInt)
            || (lhsIsPointer && rhsIsInt)
            || (lhsIsInt && rhsIsInt))
            return result(lhsType);
        return Err(
            "operation `%s` not permitted on `%s` and `%s`".format(
            binOp.display,
            lhsType.display, rhsType.display,
            ),
            lhsSpan.joinSpans(rhsSpan)).result!T;
    case arraySubscript:
    case memberAccess:
    case memberAccessThroughPointer:
        return assert(0, "not implemented");
    }
}

private Result!(LinType*) on(UnOp unOp, LinType* type, Span span)
{
    alias T = LinType*;
    with (UnOp) final switch (unOp)
    {
    case plus:
    case minus:
    case bitwiseNot:
        if (!type.contains(PrimitiveType.int_))
        {
            return Err("expected %s`int`, found %s`%s`"
                       .format(unOp.display,
                               unOp.display,
                               type.display), span)
                   .result!T;
        }
        return type.result;
    case logicalNot:
        if (!type.contains(PrimitiveType.bool_))
        {
            return Err("expected %s`bool`, found %s`%s`"
                       .format(unOp.display,
                               unOp.display,
                               type.display), span)
                   .result!T;
        }
        return result(new LinType(PrimitiveType.bool_));
    case pointerDereference:
        return (*type).match!(
            (PrimitiveType primitiveType)
            {
                return Err("dereference of primitive type `%s`"
                           .format(type.display), span)
                       .result!T;
            },
            (LinPointer pointer) => pointer.pointee.result,
        );
    case addressOf:
        return result(new LinType(LinPointer(type)));
    }
}

bool equalTo(LinType* a, LinType* b) => match!(
    (LinPointer x, LinPointer y) => x.pointee.equalTo(y.pointee),
    (PrimitiveType x, PrimitiveType y) => x == y,
    (_1, _2) => false,
)(*a, *b);

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

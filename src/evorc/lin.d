/**
 * Defines a linear AST for the Evor language where operands are
 * restricted to atomic expressions and handles the transformation
 * of Evor's AST to it.
 */

module evorc.lin;

public import evorc.ast : UnOp, BinOp, PrimitiveType;
public import evorc.span : Span;
public import std.bigint : BigInt;

import evorc.utils.result : ResultWith;
import evorc.utils.sumtype : firstField;
import std.sumtype : SumType, This;
import std.typecons : Tuple, Nullable;

alias LinProgram = LinFunc[];
alias LinFunc = Tuple!(string, "name", LinType*, "retType", LinVar[], "params", LinBlock, "block");
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
alias LinExpr = SumType!(Atom, LinUn, LinBin, LinCall);
alias Atom = SumType!(LinBool, LinInt, LinVar);
alias LinBool = Tuple!(LinType*, "type", bool, "value");
alias LinInt = Tuple!(LinType*, "type", BigInt, "value");
alias LinVar = Tuple!(LinType*, "type", VarId, "id");
alias LinType = SumType!(
    Tuple!(This*, "pointee"), // LinPointer
    PrimitiveType,
);
alias LinPointer = LinType.Types[0];
alias VarId = uint;
alias LinUn = Tuple!(LinType*, "type", UnOp, "op", Atom, "atom");
alias LinBin = Tuple!(LinType*, "type", BinOp, "op", Atom, "lhs", Atom, "rhs");
alias LinCall = Tuple!(LinType*, "type", string, "func", Atom[], "args");

alias type = firstField;

alias Err = Tuple!(string, "message", Span, "span");
alias Result = ResultWith!(Err);

Result!LinProgram lin(Program prog)
{
    Record rec;
    foreach (item; prog)
    {
        item.match!(
            (FuncDecl decl)
            {
                rec.putFuncSpec(decl.ident.name,
                                lin(decl.retType),
                                decl.params
                                    .map!(p => lin(p.type))
                                    .array);
            },
            (Func func)
            {
                rec.putFuncSpec(func.decl.ident.name,
                                lin(func.decl.retType),
                                func.decl.params
                                         .map!(p => lin(p.type))
                                         .array);
            }
        );
    }
    return prog.filter!(item => item.has!Func)
               .map!(item => lin(item.get!Func, rec))
               .collect;
};

private
{
import evorc.ast;
import evorc.display;
import evorc.span;
import evorc.utils.result;
import evorc.utils.sumtype;
import evorc.utils.unit;
import std.algorithm;
import std.array;
import std.conv;
import std.format;
import std.range;
import std.sumtype;
import std.typecons;

auto result(T)(T ok) => Result!T(ok);
auto result(T)(Err err) => Result!T(err);

Result!LinFunc lin(Func fn, ref Record rec)
{
    Record fnRec = rec.dup;
    auto retType = lin(fn.decl.retType);
    auto params = fn.decl.params
        .map!(param =>
            param.ident.isNull
                ? fnRec.next(lin(param.type))
                : fnRec.put(param.ident.get.name, lin(param.type))
        )
        .array;
    auto block = lin(fn.block, fnRec)?;
    return LinFunc(fn.decl.ident.name, retType, params, block).result;
}

LinType* lin(Type* type) => (*type).match!(
    (Pointer ptr) => new LinType(LinPointer(lin(ptr.pointee))),
    (Primitive primitive) => new LinType(primitive.type),
);

Result!LinBlock lin(Block block, ref Record rec)
{
    LinBlock linBlock;
    foreach (stmt; block)
    {
        lin(stmt, linBlock, rec)?;
    }
    return linBlock.result;
}

Result!Unit lin(Stmt* stmt, ref LinBlock block, ref Record rec)
{
    return (*stmt).match!(s => lin(s, block, rec));
}

Result!Unit lin(If if_, ref LinBlock block, ref Record rec)
{
    auto expr = lin(if_.cond, block, rec)?;
    auto exprType = expr.type;
    if (!exprType.contains(PrimitiveType.bool_))
        return Err("expected `bool`, found `%s`".format(display(exprType)), if_.cond.span).result;
    Record ifBlockRec = rec.dup, elseBlockRec = rec.dup;
    auto ifBlock = lin(if_.ifBlock, ifBlockRec)?;
    auto elseBlock = lin(if_.elseBlock, elseBlockRec)?;
    block ~= new LinStmt(LinIf(expr, ifBlock, elseBlock));
    return unit.result;
}

Result!Unit lin(Return ret, ref LinBlock block, ref Record rec)
{
    if (ret.expr.isNull)
    {
        block ~= new LinStmt(LinReturn(Nullable!LinExpr.init));
        return unit.result;
    }
    auto expr = lin(ret.expr.bitCast!(Expr*), block, rec)?;
    block ~= new LinStmt(LinReturn(expr.nullable));
    return unit.result;
}

Result!Unit lin(VarDecl varDecl, ref LinBlock block, ref Record rec)
{
    auto var = rec.put(varDecl.ident.name, lin(varDecl.type));
    if (var.type.contains(PrimitiveType.void_))
        return Err("variable declared of type `void`",  varDecl.ident.span).result;
    if (varDecl.def.isNull) return unit.result;
    auto def = varDecl.def.bitCast!(Expr*);
    auto expr = lin(def, block, rec)?;
    if (!expr.type.equalTo(var.type))
        return Err("expression of type `%s` not assignable to variable '%s' of type `%s`"
                   .format(expr.type.display,
                           varDecl.ident.name,
                           var.type.display), def.span)
               .result;
    block ~= new LinStmt(LinAssign(LValue(var), expr));
    return unit.result;
}

Result!Unit lin(Assign assign, ref LinBlock block, ref Record rec)
{
    auto lhsExpr = *assign.lhs;
    auto binOp = binOp(assign.mod);
    if (lhsExpr.has!Ident)
    {
        auto ident = lhsExpr.get!Ident;
        auto var = rec.get(ident)?;
        auto expr = lin(assign.rhs, block, rec)?;
        if (!binOp.isNull)
        {
            auto op = binOp.get;
            auto type = op.on(var.type, expr.type, ident.span, assign.rhs.span)?;
            auto atom = atomize(expr, block, rec);
            expr = LinExpr(LinBin(type, op, Atom(var), atom));
        }
        if (!var.type.equalTo(expr.type))
            return Err("expression of type `%s` not assignable to variable '%s' of type `%s`"
                       .format(expr.type.display,
                               ident.name,
                               var.type.display), assign.rhs.span).result;
        block ~= new LinStmt(LinAssign(LValue(var), expr));
    }
    else if (lhsExpr.has!Un && lhsExpr.get!Un.op == UnOp.pointerDereference)
    {
        auto lhsDerefExpr = *lhsExpr.get!Un.expr;
        if (lhsDerefExpr.has!Ident)
        {
            auto ident = lhsDerefExpr.get!Ident;
            auto var = rec.get(ident)?;
            if (!(*var.type).has!LinPointer)
                return Err("dereferencing variable '%s' of primitive type `%s`"
                           .format(ident.name, var.type.display), assign.lhs.span)
                       .result;
            auto expr = lin(assign.rhs, block, rec)?;
            auto pointeeType = (*var.type).get!LinPointer.pointee;
            if (!binOp.isNull)
            {
                auto op = binOp.get;
                auto type = op.on(pointeeType, expr.type, assign.lhs.span, assign.rhs.span)?;
                auto atom = atomize(expr, block, rec);
                auto tempVar = rec.next(type);
                auto tempExpr = LinExpr(LinUn(pointeeType, UnOp.pointerDereference, Atom(var)));
                block ~= new LinStmt(LinAssign(LValue(tempVar), tempExpr));
                expr = LinExpr(LinBin(type, op, Atom(tempVar), atom));
            }
            if (!pointeeType.equalTo(expr.type))
                return Err("expression of type `%s` not assignable to variable '%s' dereference of type `%s`"
                           .format(expr.type.display, ident.name, pointeeType.display), assign.rhs.span)
                       .result;
            block ~= new LinStmt(LinAssign(LValue(Deref(var)), expr));
        }
        else
        {
            auto tempLhsExpr = lin(lhsExpr.get!Un.expr, block, rec)?;
            auto tempLhsVar = rec.next(tempLhsExpr.type);
            block ~= new LinStmt(LinAssign(LValue(tempLhsVar), tempLhsExpr));
            if (!(*tempLhsVar.type).has!LinPointer)
                return Err("dereferencing an expression of primitive type `%s`"
                           .format(tempLhsVar.type.display), assign.lhs.span)
                       .result;
            auto expr = lin(assign.rhs, block, rec)?;
            auto pointeeType = (*tempLhsVar.type).get!LinPointer.pointee;
            if (!binOp.isNull)
            {
                auto op = binOp.get;
                auto type = op.on(pointeeType, expr.type, assign.lhs.span, assign.rhs.span)?;
                auto atom = atomize(expr, block, rec);
                auto tempRhsVar = rec.next(type);
                auto tempRhsExpr = LinExpr(LinUn(pointeeType, UnOp.pointerDereference, Atom(tempLhsVar)));
                block ~= new LinStmt(LinAssign(LValue(tempRhsVar), tempRhsExpr));
                expr = LinExpr(LinBin(type, op, Atom(tempRhsVar), atom));
            }
            if (!pointeeType.equalTo(expr.type))
                return Err("expression of type `%s` not assignable to expression dereference of type `%s`"
                           .format(expr.type.display, pointeeType.display), assign.rhs.span)
                       .result;
            block ~= new LinStmt(LinAssign(LValue(Deref(tempLhsVar)), expr));
        }
    }
    else
    {
        return Err("not an lvalue expression", assign.lhs.span).result;
    }
    return unit.result;
}

Result!Unit lin(StmtExpr stmtExpr, ref LinBlock block, ref Record rec)
{
    if (stmtExpr.isNull) return unit.result;
    auto expr = lin(stmtExpr.bitCast!(Expr*), block, rec)?;
    block ~= new LinStmt(expr);
    return unit.result;
}

Result!LinExpr lin(Expr* expr, ref LinBlock block, ref Record rec)
{
    return (*expr).match!(e => lin(e, block, rec));
}

Result!LinExpr lin(Bool bool_, ref LinBlock block, ref Record rec)
{
    return LinExpr(Atom(LinBool(new LinType(PrimitiveType.bool_), bool_.value))).result;
}

Result!LinExpr lin(Int int_, ref LinBlock block, ref Record rec)
{
    return LinExpr(Atom(LinInt(new LinType(PrimitiveType.int_), int_.value))).result;
}

Result!LinExpr lin(Ident ident, ref LinBlock block, ref Record rec)
{
    auto var = rec.get(ident)?;
    return LinExpr(Atom(var)).result;
}

Result!LinExpr lin(Un un, ref LinBlock block, ref Record rec)
{
    return (*un.expr).match!(
        (Bool bool_)
        {
            if (un.op == UnOp.addressOf)
            {
                return Err("cannot take address of rvalue expression", un.span).result;
            }
            auto exprType = new LinType(PrimitiveType.bool_);
            auto unExprType = un.op.on(exprType, un.span)?;
            return LinExpr(LinUn(unExprType, un.op, Atom(LinBool(exprType, bool_.value)))).result;
        },
        (Int int_)
        {
            if (un.op == UnOp.addressOf)
            {
                return Err("cannot take address of rvalue expression", un.span).result;
            }
            auto exprType = new LinType(PrimitiveType.int_);
            auto unExprType = un.op.on(exprType, un.span)?;
            return LinExpr(LinUn(unExprType, un.op, Atom(LinInt(exprType, int_.value)))).result;
        },
        (Ident ident)
        {
            auto var = rec.get(ident)?;
            auto type = un.op.on(var.type, un.span)?;
            return LinExpr(LinUn(type, un.op, Atom(var))).result;
        },
        (_)
        {
            auto expr = lin(un.expr, block, rec)?;
            if (un.op == UnOp.addressOf)
            {
                if (!(expr.has!LinUn && expr.get!LinUn.op == UnOp.pointerDereference))
                    return Err("cannot take address of rvalue expression", un.span).result;
            }
            auto var = rec.next(type(expr));
            auto type = un.op.on(var.type, un.span)?;
            block ~= new LinStmt(LinAssign(LValue(var), expr));
            return LinExpr(LinUn(type, un.op, Atom(var))).result;
        }
    );
}

Result!LinExpr lin(Bin bin, ref LinBlock block, ref Record rec)
{
    auto left = bin.lhs.atomize(block, rec)?;
    auto right = bin.rhs.atomize(block, rec)?;
    auto type = bin.op.on(left.type, right.type, bin.lhs.span, bin.rhs.span)?;
    return LinExpr(LinBin(type, bin.op, left, right)).result;
}

Result!LinExpr lin(Call call, ref LinBlock block, ref Record rec)
{
    auto args = call.args.map!(arg => arg.atomize(block, rec)).collect?;
    auto funcSpec = rec.getFuncSpec(call.ident)?;
    if (args.length != funcSpec.argTypes.length)
        return Err("function `%s` expects %s argument%s, but %s%s %s given"
                   .format(call.ident.name,
                           funcSpec.argTypes.length,
                           funcSpec.argTypes.length == 1 ? "" : "s",
                           args.length < funcSpec.argTypes.length ? "only " : "",
                           args.length,
                           args.length == 1 ? "was" : "were"), call.span).result;
    foreach (index, a, b; lockstep(args.map!type, funcSpec.argTypes))
    {
        if (!a.equalTo(b))
        {
            return Err("function `%s` expects argument %s to be of type `%s`, found `%s`"
                       .format(call.ident.name,
                               index + 1,
                               b.display,
                               a.display), call.args[index].span).result;
        }
    }
    return LinExpr(LinCall(funcSpec.retType, call.ident.name, args)).result;
}

Atom atomize(LinExpr expr, ref LinBlock block, ref Record rec) => expr.match!(
    (Atom atom) => atom,
    (_)
    {
        auto tempVar = rec.next(expr.type);
        block ~= new LinStmt(LinAssign(LValue(tempVar), expr));
        return Atom(tempVar);
    },
);

Result!Atom atomize(Expr* expr, ref LinBlock block, ref Record rec) => (*expr).match!(
    (Bool bool_) => Atom(LinBool(new LinType(PrimitiveType.bool_), bool_.value)).result,
    (Int int_) => Atom(LinInt(new LinType(PrimitiveType.int_), int_.value)).result,
    (Ident ident)
    {
        auto var = rec.get(ident)?;
        return Atom(var).result;
    },
    (_)
    {
        auto tempExpr = lin(expr, block, rec)?;
        auto var = rec.next(tempExpr.type);
        block ~= new LinStmt(LinAssign(LValue(var), tempExpr));
        return Atom(var).result;
    },
);

Result!(LinType*) on(BinOp binOp, LinType* lhsType, LinType* rhsType, Span lhsSpan, Span rhsSpan)
{
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
                lhsSpan.joinSpans(rhsSpan)).result;
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
            lhsSpan.joinSpans(rhsSpan)).result;
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
            lhsSpan.joinSpans(rhsSpan)).result;
    case arraySubscript:
    case memberAccess:
    case memberAccessThroughPointer:
        return assert(0, "not implemented");
    }
}

Result!(LinType*) on(UnOp unOp, LinType* type, Span span)
{
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
                   .result;
        }
        return type.result;
    case logicalNot:
        if (!type.contains(PrimitiveType.bool_))
        {
            return Err("expected %s`bool`, found %s`%s`"
                       .format(unOp.display,
                               unOp.display,
                               type.display), span)
                   .result;
        }
        return result(new LinType(PrimitiveType.bool_));
    case pointerDereference:
        return (*type).match!(
            (PrimitiveType primitiveType)
            {
                return Err("dereference of primitive type `%s`"
                           .format(type.display), span)
                       .result;
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

struct Record
{
    Record dup()
    {
        return Record(funcSpecs.dup, vars.dup, varCount);
    }

    // vars

    LinVar put(string name, LinType* type)
    {
        auto var = LinVar(type, varCount++);
        vars[name] = var;
        return var;
    }

    Nullable!LinVar get(string name)
    {
        auto var = name in vars;
        if (var !is null) return nullable(*var);
        return Nullable!LinVar.init;
    }

    Result!LinVar get(Ident ident)
    {
        auto var = get(ident.name);
        if (var.isNull) return Err("undefined variable", ident.span).result;
        return var.get.result;
    }

    LinVar next(LinType* type) => LinVar(type, varCount++);

    // functions

    void putFuncSpec(string name, LinType* retType, LinType*[] argTypes)
    {
        funcSpecs[name] = FuncSpec(retType, argTypes);
    }

    Nullable!FuncSpec getFuncSpec(string name)
    {
        auto spec = name in funcSpecs;
        if (spec !is null) return nullable(*spec);
        return Nullable!FuncSpec.init;
    }

    Result!FuncSpec getFuncSpec(Ident ident)
    {
        auto spec = getFuncSpec(ident.name);
        if (spec.isNull) return Err("undefined function", ident.span).result;
        return spec.get.result;
    }

private:
    FuncSpec[string] funcSpecs;
    LinVar[string] vars;
    uint varCount = 0;
}

struct FuncSpec
{
    LinType* retType;
    LinType*[] argTypes;
}

Nullable!BinOp binOp(AssignMod assignMod)
{
    with (BinOp) with (AssignMod) final switch (assignMod)
    {
    case none: return Nullable!BinOp.init;
    case add: return BinOp.add.nullable;
    case sub: return BinOp.sub.nullable;
    case mul: return BinOp.mul.nullable;
    case div: return BinOp.div.nullable;
    case rem: return BinOp.rem.nullable;
    case bitwiseAnd: return BinOp.bitwiseAnd.nullable;
    case bitwiseOr: return BinOp.bitwiseOr.nullable;
    case bitwiseXor: return BinOp.bitwiseXor.nullable;
    case bitwiseLeftShift: return BinOp.bitwiseLeftShift.nullable;
    case bitwiseRightShift: return BinOp.bitwiseRightShift.nullable;
    }
}
} // private

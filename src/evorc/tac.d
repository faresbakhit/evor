/**
 * Defines the three-address code of the Evor language and a
 * function that transforms the abstract syntax tree into it.
 *
 * FE3: Semantic analysis
 */

module evorc.tac;

public import evorc.ast : BinOp, UnOp, PrimitiveType;
public import evorc.span : Span;

import evorc.utils.result : ResultWith;
import evorc.utils.sumtype : firstField;
import std.sumtype : SumType;
import std.typecons : Tuple;

alias Program = Tuple!(Func[], "funcs", Record, "rec");

alias Func = Tuple!(FuncId, "id", Type, "retType", Var[], "params", Block, "instrs");
alias FuncId = uint;
alias Block = Inst[];

alias Inst = SumType!(Label, Jmp, Jcc, Bin, Un, Assign, Load, Store, Param, Call, Return, Leave);
alias Label = Tuple!(LabelId, "id");
alias LabelId = uint;
alias Jmp = Tuple!(Label, "label");
alias Jcc = Tuple!(Atom, "cond", Label, "label");
alias Bin = Tuple!(Var, "dest", BinOp, "op", Atom, "lhs", Atom, "rhs");
alias Un = Tuple!(Var, "dest", UnOp, "op", Atom, "src");
alias Assign = Tuple!(Var, "dest", Atom, "src");
alias Load = Tuple!(Var, "dest", Atom, "srcPtr");
alias Store = Tuple!(Var, "destPtr", Atom, "src");
alias Param = Tuple!(Atom, "arg");
alias Call = Tuple!(Var, "dest", FuncId, "funcId");
alias Return = Tuple!(Atom, "val");
alias Leave = Tuple!();

alias Atom = SumType!(Var, Int, Bool);
alias Var = Tuple!(Type, "type", VarId, "id");
alias VarId = uint;
alias Int = Tuple!(Type, "type", int, "value");
alias Bool = Tuple!(Type, "type", bool, "value");
alias type = firstField;

alias Type = SumType!(Primitive, Struct, Pointer);
alias TypeId = uint;
alias Primitive = PrimitiveType;
alias Struct = TypeId[];
alias Pointer = Tuple!(TypeId, "base");

alias Err = Tuple!(string, "message", Span, "span");
alias Result = ResultWith!(Err);

Result!Program tac(a.Program prog)
{
    Record rec;
    return tac(prog, rec);
}

Result!Program tac(a.Program prog, ref Record rec)
{
    rec.varsStack.reserve(16);
    foreach (item; prog)
    {
        item.match!(
            (a.FuncDecl decl)
            {
                rec.putFuncSpec(decl.ident.name,
                                tac(decl.retType, rec),
                                decl.params
                                    .map!(p => tac(p.type, rec))
                                    .array);
            },
            (a.Func func)
            {
                rec.putFuncSpec(func.decl.ident.name,
                                tac(func.decl.retType, rec),
                                func.decl.params
                                         .map!(p => tac(p.type, rec))
                                         .array);
            }
        );
    }
    auto funcs = prog.filter!(item => item.has!(a.Func)).map!(item => tac(item.get!(a.Func), rec)).collect?;
    return Program(funcs, rec).result;
};


struct Record
{
    Type getType(TypeId typeId) const
    {
        return types[typeId];
    }

    string getFuncName(FuncId funcId) const
    {
        return funcs[funcId];
    }

private:
    Var[string][] varsStack;
    uint varCount;

    FuncSpec[string] funcSpecs;
    string[] funcs;

    TypeId[Type] typesMap;
    Type[] types;

    uint labelCount;

    void scopeEnter()
    {
        varsStack ~= (Var[string]).init;
    }

    void scopeExit()
    {
        varsStack.popBack();
    }

    Nullable!Var getVar(string name)
    {
        foreach_reverse (varsTable; varsStack)
        {
            auto var = name in varsTable;
            if (var !is null) return (*var).nullable;
        }
        return Nullable!Var.init;
    }

    Result!Var getVar(a.Ident ident)
    {
        auto var = getVar(ident.name);
        if (var.isNull) return Err("undefined variable", ident.span).result;
        return var.get.result;
    }

    Result!Var putVar(Type type, string name, lazy Err onExists)
    {
        if (name in varsStack[$-1])
            return onExists().result;
        auto var = varsStack[$-1][name] = Var(type, varCount++);
        auto r = var.result;
        return r;
    }

    Var putVar(Type type, string name)
    {
        return varsStack[$-1][name] = Var(type, varCount++);
    }

    Var putVar(Type type)
    {
        return Var(type, varCount++);
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

    void putFuncSpec(string name, Type retType, Type[] paramTypes)
    {
        auto id = cast(uint)funcs.length;
        funcs ~= name;
        funcSpecs[name] = FuncSpec(id, retType, paramTypes);
    }

    TypeId getTypeId(Type type)
    {
        auto id = type in typesMap;
        if (id !is null)
            return *id;
        auto typeId = cast(TypeId)(types.length);
        typesMap[type] = typeId;
        types ~= type;
        return typeId;
    }

    Label nextLabel()
    {
        return Label(labelCount++);
    }
}

private
{
import a = evorc.ast;
import evorc.ast : span, Ident;
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

Result!Func tac(a.Func fn, ref Record rec)
{
    rec.scopeEnter();
    auto retType = tac(fn.decl.retType, rec);
    auto paramsResult = fn.decl.params
        .map!(
            param => param.ident.isNull
                ? rec.putVar(tac(param.type, rec)).result!(Var)
                : rec.putVar(
                        tac(param.type, rec),
                        param.ident.get.name,
                        Err("parameter with name already defined", param.ident.get.span)),
        )
        .collect;
    auto params = paramsResult?;
    Inst[] instrs;
    foreach (stmt; fn.block)
    {
        tac(stmt, instrs, rec)?;
    }
    rec.scopeExit();
    auto id = rec.getFuncSpec(fn.decl.ident.name).get.funcId;
    return Func(id, retType, params, instrs).result;
}

Result!Unit tac(a.Block block, ref Inst[] instrs, ref Record rec)
{
    rec.scopeEnter();
    foreach (stmt; block)
    {
        tac(stmt, instrs, rec)?;
    }
    rec.scopeExit();
    return unit.result;
}

Result!Unit tac(a.Stmt* stmt, ref Block block, ref Record rec)
{
    return (*stmt).match!(s => tac(s, block, rec));
}

Result!Unit tac(a.If if_, ref Block block, ref Record rec)
{
    auto cond = atom(if_.cond, block, rec)?;
    if (!cond.type.contains(Primitive.bool_))
        return Err("expected expression of type `bool`, found `%s`"
                   .format(cond.type.display(rec)), if_.cond.span)
               .result;
    auto thenLabel = rec.nextLabel();
    auto endLabel = rec.nextLabel();
    block ~= Inst(Jcc(cond, thenLabel));
    tac(if_.elseBlock, block, rec)?;
    block ~= Inst(Jmp(endLabel));
    block ~= Inst(thenLabel);
    tac(if_.ifBlock, block, rec)?;
    block ~= Inst(endLabel);
    return unit.result;
}

Result!Unit tac(a.Return ret, ref Block block, ref Record rec)
{
    if (ret.expr.isNull)
    {
        block ~= Inst(Leave());
        return unit.result;
    }
    auto atom = atom(ret.expr.bitCast!(a.Expr*), block, rec)?;
    block ~= Inst(Return(atom));
    return unit.result;
}

Result!Unit tac(a.VarDecl varDecl, ref Block block, ref Record rec)
{
    auto var = rec.putVar(tac(varDecl.type, rec), varDecl.ident.name);
    if (var.type.contains(PrimitiveType.void_))
        return Err("variable declared of type `void`",  varDecl.ident.span).result;
    if (varDecl.def.isNull) return unit.result;
    tac(varDecl.ident, Nullable!BinOp.init, varDecl.def.bitCast!(a.Expr*), block, rec)?;
    return unit.result;
}

Result!Unit tac(a.Assign assign, ref Block block, ref Record rec)
{
    auto lhsExpr = *assign.lhs;
    auto modBinOp = binOp(assign.mod);
    return lhsExpr.match!(
        (a.Ident ident) => tac(ident, modBinOp, assign.rhs, block, rec),
        (a.Un un) => tac(un.expr, modBinOp, assign.rhs, block, rec),
        _ => Err("not an lvalue expression", assign.lhs.span).result,
    );
}

Result!Unit tac(Ident ident, Nullable!BinOp modBinOp, a.Expr* expr, ref Block block, ref Record rec)
{
    auto dest = rec.getVar(ident)?;
    auto src = atom(expr, block, rec)?;
    if (!modBinOp.isNull)
    {
        // dest += src
        //
        // #1 = dest + src
        // dest = src
        auto op = modBinOp.get;
        auto tempType = op.on(dest.type, src.type, ident.span, expr.span, rec)?;
        auto temp = rec.putVar(tempType);
        block ~= Inst(Bin(temp, op, Atom(dest), src));
        src = Atom(temp);
    }
    if (dest.type != src.type)
        return Err("expression of type `%s` not assignable to variable '%s' of type `%s`"
                   .format(src.type.display(rec),
                           ident.name,
                           dest.type.display(rec)), expr.span).result;
    block ~= Inst(Assign(dest, src));
    return unit.result;
}

Result!Unit tac(a.Expr* destExpr, Nullable!BinOp modBinOp, a.Expr* srcExpr, ref Block block, ref Record rec)
{
    auto dest = atom(destExpr, block, rec)?;
    if (!dest.type.has!Pointer)
        return Err("dereferencing an expression of non-pointer type `%s`"
                   .format(dest.type.display(rec)), destExpr.span)
               .result;
    auto src = atom(srcExpr, block, rec)?;
    auto pointeeType = rec.getType(dest.type.get!Pointer.base);
    if (!modBinOp.isNull)
    {
        // *dest += src
        //
        // #1 = *dest
        // #2 = #1 + src
        // *dest = #2
        auto op = modBinOp.get;
        auto t2Type = op.on(pointeeType, src.type, destExpr.span, srcExpr.span, rec)?;
        auto t1 = rec.putVar(pointeeType);
        auto t2 = rec.putVar(t2Type);
        block ~= [
            Inst(Load(t1, dest)),
            Inst(Bin(t2, op, Atom(t1), src)),
        ];
        src = Atom(t2);
    }
    if (src.type != pointeeType)
        return Err("expression of type `%s` not assignable to expression dereference of type `%s`"
                   .format(src.type.display(rec), pointeeType.display(rec)), srcExpr.span)
               .result;
    block ~= Inst(Store(dest.get!Var, src));
    return unit.result;
}

Result!Unit tac(a.StmtExpr stmtExpr, ref Block block, ref Record rec)
{
    if (stmtExpr.isNull) return unit.result;
    atom(stmtExpr.bitCast!(a.Expr*), block, rec)?;
    return unit.result;
}

Type tac(a.Type* type, ref Record rec) => (*type).match!(
    (a.Pointer ptr) => Type(Pointer(rec.getTypeId(tac(ptr.pointee, rec)))),
    (a.Primitive primitive) => Type(primitive.type),
);

Result!Atom atom(a.Expr* expr, ref Block block, ref Record rec)
{
    return (*expr).match!(e => atom(e, block, rec));
}

Result!Atom atom(a.Bool bool_, ref Block block, ref Record rec)
{
    return Atom(Bool(Type(Primitive.bool_), bool_.value)).result;
}

Result!Atom atom(a.Int int_, ref Block block, ref Record rec)
{
    if (int_.value < int.min || int_.value > int.max)
    {
        return Err("literal out of range for type `int`", int_.span).result;
    }
    return Atom(Int(Type(Primitive.int_), int_.value.toInt())).result;
}

Result!Atom atom(a.Ident ident, ref Block block, ref Record rec)
{
    auto var = rec.getVar(ident)?;
    return Atom(var).result;
}

Result!Atom atom(a.Un un, ref Block block, ref Record rec)
{
    if (un.op == UnOp.addressOf && !isLValue(un.expr))
    {
        return Err("cannot take address of rvalue expression", un.span).result;
    }
    auto atom = atom(un.expr, block, rec)?;
    auto type = un.op.on(atom.type, un.span, rec)?;
    auto temp = rec.putVar(type);
    block ~= Inst(Un(temp, un.op, atom));
    return Atom(temp).result;
}

Result!Atom atom(a.Bin bin, ref Block block, ref Record rec)
{
    auto lhs = atom(bin.lhs, block, rec)?;
    auto rhs = atom(bin.rhs, block, rec)?;
    auto type = bin.op.on(lhs.type, rhs.type, bin.lhs.span, bin.rhs.span, rec)?;
    auto temp = rec.putVar(type);
    block ~= Inst(Bin(temp, bin.op, lhs, rhs));
    return Atom(temp).result;
}

Result!Atom atom(a.Call call, ref Block block, ref Record rec)
{
    auto args = call.args.map!(arg => atom(arg, block, rec)).collect?;
    auto funcSpec = rec.getFuncSpec(call.ident)?;
    if (args.length != funcSpec.paramTypes.length)
        return Err("function `%s` expects %s argument%s, but %s%s %s given"
                   .format(call.ident.name,
                           funcSpec.paramTypes.length,
                           funcSpec.paramTypes.length == 1 ? "" : "s",
                           args.length < funcSpec.paramTypes.length ? "only " : "",
                           args.length,
                           args.length == 1 ? "was" : "were"), call.span).result;
    foreach (index, a, b; lockstep(args.map!type, funcSpec.paramTypes))
    {
        if (a != b)
        {
            return Err("function `%s` expects argument %s to be of type `%s`, found `%s`"
                       .format(call.ident.name,
                               index + 1,
                               b.display(rec),
                               a.display(rec)), call.args[index].span).result;
        }
        block ~= Inst(Param(args[index]));
    }
    auto temp = rec.putVar(funcSpec.retType);
    block ~= Inst(Call(temp, funcSpec.funcId));
    return Atom(temp).result;
}

bool isLValue(a.Expr* expr) => (*expr).match!(
    (a.Ident ident) => true,
    (a.Un un) => un.op == UnOp.pointerDereference,
    _ => false,
);

Result!Type on(BinOp binOp, Type lhsType, Type rhsType, Span lhsSpan, Span rhsSpan, ref Record rec)
{
    with (BinOp) final switch (binOp)
    {
    case logicalOr:
    case logicalAnd:
        auto lhsIsBool = lhsType.contains(Primitive.bool_);
        auto rhsIsBool = rhsType.contains(Primitive.bool_);
        if (!(lhsIsBool && rhsIsBool))
        {
            return Err(
                "expected `bool` %s `bool`, found `%s` %s `%s`".format(
                binOp.display,
                lhsType.display(rec),
                binOp.display,
                rhsType.display(rec),
                ),
                lhsSpan.joinSpans(rhsSpan)).result;
        }
        goto case;
    case equalTo:
    case notEqualTo:
        return result(Type(Primitive.bool_));
    case lessThan:
    case greaterThan:
    case lessThanOrEqualTo:
    case greaterThanOrEqualTo:
        auto lhsIsInt = lhsType.contains(Primitive.int_);
        auto rhsIsInt = rhsType.contains(Primitive.int_);
        auto lhsIsPointer = lhsType.has!Pointer;
        auto rhsIsPointer = rhsType.has!Pointer;
        if ((lhsIsPointer && rhsIsPointer)
            || (lhsIsInt && rhsIsInt))
            return result(Type(PrimitiveType.bool_));
        return Err(
            "operation `%s` not permitted on `%s` and `%s`".format(
            binOp.display,
            lhsType.display(rec), rhsType.display(rec),
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
        auto lhsIsPointer = lhsType.has!Pointer;
        auto rhsIsPointer = rhsType.has!Pointer;
        if ((rhsIsPointer && lhsIsInt)
            || (lhsIsPointer && rhsIsInt)
            || (lhsIsInt && rhsIsInt))
            return result(lhsType);
        return Err(
            "operation `%s` not permitted on `%s` and `%s`".format(
            binOp.display,
            lhsType.display(rec), rhsType.display(rec),
            ),
            lhsSpan.joinSpans(rhsSpan)).result;
    case arraySubscript:
    case memberAccess:
    case memberAccessThroughPointer:
        return assert(0, "not implemented");
    }
}

Result!Type on(UnOp unOp, Type type, Span span, ref Record rec)
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
                               type.display(rec)), span)
                   .result;
        }
        return type.result;
    case logicalNot:
        if (!type.contains(Primitive.bool_))
        {
            return Err("expected %s`bool`, found %s`%s`"
                       .format(unOp.display,
                               unOp.display,
                               type.display(rec)), span)
                   .result;
        }
        return result(Type(Primitive.bool_));
    case pointerDereference:
        if (!type.has!Pointer)
        {
            return Err("dereference of non-pointer type `%s`"
                       .format(type.display(rec)), span)
                   .result;
        }
        return rec.getType(type.get!Pointer.base).result;
    case addressOf:
        return result(Type(Pointer(rec.getTypeId(type))));
    }
}

Nullable!BinOp binOp(a.AssignMod assignMod)
{
    with (BinOp) with (a.AssignMod) final switch (assignMod)
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

struct FuncSpec
{
    uint funcId;
    Type retType;
    Type[] paramTypes;
}
} // private

/**
 * Defines the abstract syntax tree of the Evor language and handles
 * syntactical analysis of a valid or invalid Evor program.
 */

module evorc.ast;

import evorc.span;
import std.typecons;
import std.sumtype;
import std.bigint;

alias Program = ProgramItem[];
alias ProgramItem = SumType!(FuncDecl, Func);
alias FuncDecl = Tuple!(Span, "span", Ident, "ident", Type*, "retType", Param[], "params");
alias Ident = Tuple!(Span, "span", string, "name");
alias Type = SumType!(
    Tuple!(Span, "span", This*, "pointee"), // Pointer
    Primitive
);
alias Pointer = Type.Types[0];
alias Primitive = Tuple!(Span, "span", PrimitiveType, "type");
alias Param = Tuple!(Span, "span", Type*, "type", Nullable!Ident, "ident");
alias Func = Tuple!(FuncDecl, "decl", Block, "block");
alias Block = Stmt*[];
alias Stmt = SumType!(
    Tuple!(Expr*, "cond", This*[], "ifBlock", This*[], "elseBlock"), // If
    Tuple!(Span, "span", NullableRef!Expr, "expr"), // Return
    VarDecl,
    Assign,
    StmtExpr,
);
alias If = Stmt.Types[0];
alias Return = Stmt.Types[1];
alias VarDecl = Tuple!(Span, "span", Type*, "type", Ident, "ident", NullableRef!Expr, "def");
alias Assign = Tuple!(Span, "span", AssignMod, "mod", Expr*, "lhs", Expr*, "rhs");
alias StmtExpr = NullableRef!Expr;
alias Expr = SumType!(
    Tuple!(Span, "span", UnOp, "op", This*, "expr"), // Un
    Tuple!(Span, "span", BinOp, "op", This*, "lhs", This*, "rhs"), // Bin
    Tuple!(Span, "span", Ident, "ident", This*[], "args"), // Call
    Bool,
    Int,
    Ident,
);
alias Un = Expr.Types[0];
alias Bin = Expr.Types[1];
alias Call = Expr.Types[2];
alias Bool = Tuple!(Span, "span", bool, "value");
alias Int = Tuple!(Span, "span", BigInt, "value");
alias Args = Expr*[];

public import evorc.utils.sumtype : span = firstField;

enum BinOp {
    add,
    sub,
    mul,
    div,
    rem,
    bitwiseAnd,
    bitwiseOr,
    bitwiseXor,
    bitwiseLeftShift,
    bitwiseRightShift,
    logicalAnd,
    logicalOr,
    equalTo,
    notEqualTo,
    lessThan,
    greaterThan,
    lessThanOrEqualTo,
    greaterThanOrEqualTo,
    arraySubscript,
    memberAccess,
    memberAccessThroughPointer,
}

enum UnOp {
    plus,
    minus,
    bitwiseNot,
    logicalNot,
    pointerDereference,
    addressOf,
}

enum AssignMod
{
    none,
    add,
    sub,
    mul,
    div,
    rem,
    bitwiseAnd,
    bitwiseOr,
    bitwiseXor,
    bitwiseLeftShift,
    bitwiseRightShift,
}

enum PrimitiveType
{
    int_,
    bool_,
    void_,
}

alias Err = Tuple!(string, "message", Tok, "tok");

import evorc.utils.result : ResultWith;

alias Result = ResultWith!(Err);
auto result(T)(T ok) => Result!T(ok);
auto result(T)(Err err) => Result!T(err);

import evorc.tok;
private alias IdentTok = evorc.tok.Ident;

Result!Program parse(Range)(auto ref Range toks)
if (isTokRange!Range)
{
    Program prog;
    while (!toks.front.has!Eof)
    {
        auto item = parseProgramItem(toks)?;
        prog ~= item;
    }
    return prog.result;
}

private Result!ProgramItem parseProgramItem(Range)(auto ref Range toks)
if (isTokRange!Range)
{
    auto retType = parseType(toks)?;
    auto ident = parseIdent(toks)?;
    auto params = parseParams(toks)?;
    auto declEndSpan = toks.pop.span;
    auto declSpan = retType.span.joinSpans(declEndSpan);
    auto decl = FuncDecl(declSpan, ident, retType, params);
    if (toks.nextIf!";")
        return ProgramItem(decl).result;
    auto block = parseBlock(toks)?;
    return ProgramItem(Func(decl, block)).result;
}

private Result!(Type*) parseType(Range)(auto ref Range toks)
if (isTokRange!Range)
{
    Tok tok = toks.pop;
    if (!tok.has!IdentTok) return Err("expected type, found `%s`", tok).result;
    Type* type;
    switch (tok.get!IdentTok.name)
    {
    case "int": type = new Type(Primitive(tok.span, PrimitiveType.int_)); break;
    case "bool": type = new Type(Primitive(tok.span, PrimitiveType.bool_)); break;
    case "void": type = new Type(Primitive(tok.span, PrimitiveType.void_)); break;
    default: return Err("expected type, found %s", tok).result;
    }
    tok = toks.front;
    auto span = tok.span;
    while (toks.nextIf!"*")
    {
        span = joinSpans(span, tok.span);
        type = new Type(Pointer(span, type));
        tok = toks.front;
    }
    return type.result;
}

// NOTE: Doesn't consume the last sym!")" token
private Result!(Param[]) parseParams(Range)(auto ref Range toks)
if (isTokRange!Range)
{
    auto err = toks.expect!"(";
    if (!err.isNull) return err.get.result;
    if (toks.nextIs!")") return [].result;
    Param[] params;
    auto p = parseParam(toks)?;
    params ~= p;
    while (!toks.nextIs!")")
    {
        err = toks.expect!",";
        if (!err.isNull) return Err("expected `,` or `)`, found %s", err.get.tok).result;
        p = parseParam(toks)?;
        params ~= p;
    }
    return params.result;
}

private Result!Param parseParam(Range)(auto ref Range toks)
if (isTokRange!Range)
{
    auto type = parseType(toks)?;
    if (!toks.front.has!IdentTok)
        return Param(type.span, type, Nullable!Ident.init).result;
    auto ident = parseIdent(toks)?;
    auto span = type.span.joinSpans(ident.span);
    return Param(span, type, ident.nullable).result;
}

private Result!Block parseBlock(Range)(auto ref Range toks)
if (isTokRange!Range)
{
    auto err = toks.expect!"{";
    if (!err.isNull) return err.get.result;
    Block block;
    while (!toks.nextIf!"}")
    {
        auto stmt = parseStmt(toks)?;
        block ~= stmt;
    }
    return block.result;
}

private Result!(Stmt*) parseStmt(Range)(auto ref Range toks)
if (isTokRange!Range)
{
    if (toks.nextIf!";")
    {
        return result(new Stmt(NullableRef!Expr.init));
    }
    if (toks.nextIf!"if")
    {
        // auto err = toks.expect!"(";
        // if (!err.isNull) return err.get.result;
        auto cond = parseExpr(toks)?;
        // err = toks.expect!")";
        // if (!err.isNull) return err.get.result;
        Block ifBlock;
        if (toks.nextIs!"{") {
            ifBlock = parseBlock(toks)?;
        } else {
            auto stmt = parseStmt(toks)?;
            ifBlock = [stmt];
        }
        if (toks.nextIf!"else")
        {
            Block elseBlock;
            if (toks.nextIs!"{") {
                elseBlock = parseBlock(toks)?;
            } else {
                auto stmt = parseStmt(toks)?;
                elseBlock = [stmt];
            }
            return result(new Stmt(If(cond, ifBlock, elseBlock)));
        }
        return result(new Stmt(If(cond, ifBlock, [])));
    }
    if (toks.nextIs!"return")
    {
        auto retSpan = toks.pop.span;
        if (toks.nextIf!";")
            return result(new Stmt(Return(retSpan, NullableRef!Expr.init)));
        auto expr = parseExpr(toks)?;
        auto err = toks.expect!";";
        if (!err.isNull) return err.get.result;
        auto span = retSpan.joinSpans(expr.span);
        return result(new Stmt(Return(span, expr.nullableRef)));
    }
    Stmt* stmt;
    Range toksSave = toks.save;
    auto varDecl = parseVarDecl(toks);
    if (varDecl.isErr)
    {
        toks = toksSave;
        auto lhs = parseExpr(toks)?;
        if (toks.front.has!Sym)
        {
            auto assignMod = assignMod(toks.front.get!Sym);
            if (!assignMod.isNull)
            {
                toks.popFront;
                auto rhs = parseExpr(toks)?;
                auto span = lhs.span.joinSpans(rhs.span);
                stmt = new Stmt(Assign(span, assignMod.get, lhs, rhs));
                goto expectSemicolon;
            }
        }
        stmt = new Stmt(lhs.nullableRef);
        goto expectSemicolon;
    }
    stmt = new Stmt(varDecl.get);
expectSemicolon:
    auto err = toks.expect!";";
    if (!err.isNull) return err.get.result;
    return stmt.result;
}

private Result!(Expr*) parseExpr(Range)(auto ref Range toks)
if (isTokRange!Range)
{
    return parseExpr(toks, 0);
}

private Result!(Expr*) parseExpr(Range)(auto ref Range toks, ushort min_bp)
if (isTokRange!Range)
{
    Tok tok = toks.pop;
    auto res = tok.type.match!(
        (Sym symbol)
        {
            if (symbol == sym!"(")
            {
                auto lhs = parseExpr(toks);
                auto exprEndSpan = toks.front.span;
                auto err = toks.expect!")";
                if (!err.isNull) return err.get.result;
                if (!lhs.isErr)
                    lhs.get.span = tok.span.joinSpans(exprEndSpan);
                return lhs;
            }
            auto op = unOp(symbol);
            if (op.isNull) return Err("expected expression, found `%s`", tok).result;
            auto rhsBP = unOpBindingPower(op.get);
            auto rhs = parseExpr(toks, rhsBP)?;
            auto span = tok.span.joinSpans(rhs.span);
            return result(new Expr(Un(span, op.get, rhs)));
        },
        (evorc.tok.Bool bool_) => result(new Expr(Bool(tok.span, bool_.value))),
        (evorc.tok.Int int_) => result(new Expr(Int(tok.span, int_.value))),
        (evorc.tok.Ident ident)
        {
            if (!toks.nextIf!"(")
                return result(new Expr(Ident(tok.span, ident.name)));
            auto func = Ident(tok.span, ident.name);
            if (toks.nextIs!")")
            {
                auto callEndSpan = toks.pop.span;
                return result(new Expr(Call(tok.span.joinSpans(callEndSpan), func, [])));
            }
            Args args;
            auto expr = parseExpr(toks)?;
            args ~= expr;
            while (!toks.nextIs!")")
            {
                auto err = toks.expect!",";
                if (!err.isNull) return err.get.result;
                expr = parseExpr(toks)?;
                args ~= expr;
            }
            auto span = tok.span.joinSpans(toks.pop.span);
            return result(new Expr(Call(span, func, args)));
        },
        _ => Err("expected expression, found `%s`", tok).result!(Expr*),
    );
    if (res.isErr) return res;
    auto lhs = res.get;
    while (toks.front.has!Sym)
    {
        auto op = binOp(toks.front.get!Sym);
        if (op.isNull)
            break;
        auto bp = binOpBindingPower(op.get);
        if (bp.lhs < min_bp)
            break;
        toks.popFront;
        auto rhs = parseExpr(toks, bp.rhs)?;
        auto span = lhs.span.joinSpans(rhs.span);
        lhs = new Expr(Bin(span, op.get, lhs, rhs));
    }
    return lhs.result;
}

private Result!VarDecl parseVarDecl(Range)(auto ref Range toks)
if (isTokRange!Range)
{
    auto type = parseType(toks)?;
    auto ident = parseIdent(toks)?;
    if (toks.nextIf!"=")
    {
        auto expr = parseExpr(toks)?;
        auto span = type.span.joinSpans(expr.span);
        return VarDecl(span, type, ident, expr.nullableRef).result;
    }
    auto span = type.span.joinSpans(ident.span);
    return VarDecl(span, type, ident, NullableRef!Expr.init).result;
}

private Result!Ident parseIdent(Range)(auto ref Range toks)
if (isTokRange!Range)
{
    Tok tok = toks.pop;
    if (tok.has!IdentTok) return Ident(tok.span, tok.get!IdentTok.name).result;
    return Err("expected identifier, found `%s`", tok).result;
}

private Nullable!UnOp unOp(Sym symbol)
{
    with (UnOp) with (Sym) switch (symbol)
    {
    case plus:  return UnOp.plus.nullable;
    case minus: return UnOp.minus.nullable;
    case tilde: return bitwiseNot.nullable;
    case not:   return logicalNot.nullable;
    case star:  return pointerDereference.nullable;
    case and:   return addressOf.nullable;
    default:    return Nullable!UnOp.init;
    }
}

private Nullable!BinOp binOp(Sym symbol)
{
    with (Sym) with (BinOp) switch (symbol)
    {
    case plus:         return add.nullable;
    case minus:        return sub.nullable;
    case star:         return mul.nullable;
    case slash:        return div.nullable;
    case percent:      return rem.nullable;
    case and:          return bitwiseAnd.nullable;
    case bar:          return bitwiseOr.nullable;
    case caret:        return bitwiseXor.nullable;
    case shiftLeft:    return bitwiseLeftShift.nullable;
    case shiftRight:   return bitwiseRightShift.nullable;
    case andAnd:       return logicalAnd.nullable;
    case barBar:       return logicalOr.nullable;
    case equalEqual:   return equalTo.nullable;
    case notEqual:     return notEqualTo.nullable;
    case less:         return lessThan.nullable;
    case lessEqual:    return lessThanOrEqualTo.nullable;
    case greater:      return greaterThan.nullable;
    case greaterEqual: return greaterThanOrEqualTo.nullable;
    case dot:          return memberAccess.nullable;
    case arrow:        return memberAccessThroughPointer.nullable;
    default:           return Nullable!BinOp.init;
    }
}

private Nullable!AssignMod assignMod(Sym symbol)
{
    with (Sym) with (AssignMod) switch (symbol)
    {
    case equal:           return none.nullable;
    case plusEqual:       return add.nullable;
    case minusEqual:      return sub.nullable;
    case starEqual:       return mul.nullable;
    case slashEqual:      return div.nullable;
    case percentEqual:    return rem.nullable;
    case andEqual:        return bitwiseAnd.nullable;
    case barEqual:        return bitwiseOr.nullable;
    case caretEqual:      return bitwiseXor.nullable;
    case shiftLeftEqual:  return bitwiseLeftShift.nullable;
    case shiftRightEqual: return bitwiseRightShift.nullable;
    default:              return Nullable!AssignMod.init;
    }
}

private alias BindingPower = Tuple!(ushort, "lhs", ushort, "rhs");

private BindingPower binOpBindingPower(BinOp binOp)
{
    with (BinOp) final switch (binOp)
    {
    case logicalOr: return BindingPower(1, 2);
    case logicalAnd: return BindingPower(3, 4);
    case bitwiseOr: return BindingPower(5, 6);
    case bitwiseXor: return BindingPower(7, 8);
    case bitwiseAnd: return BindingPower(9, 10);
    case equalTo:
    case notEqualTo:
        return BindingPower(11, 12);
    case lessThan:
    case greaterThan:
    case lessThanOrEqualTo:
    case greaterThanOrEqualTo:
        return BindingPower(13, 14);
    case bitwiseLeftShift:
    case bitwiseRightShift:
        return BindingPower(15, 16);
    case add:
    case sub:
        return BindingPower(17, 18);
    case mul:
    case div:
    case rem:
        return BindingPower(19, 20);
    case arraySubscript:
    case memberAccess:
    case memberAccessThroughPointer:
        return BindingPower(22, 23);
    }
}

private ushort unOpBindingPower(UnOp unOp)
{
    with (UnOp) final switch (unOp)
    {
    case plus:
    case minus:
    case bitwiseNot:
    case logicalNot:
    case pointerDereference:
    case addressOf:
        return 21;
    }
}

private Nullable!Err expect(string symbol, Range)(auto ref Range toks)
{
    Tok tok = toks.pop;
    if (!tok.contains(sym!symbol))
    {
        enum message = "expected `" ~ symbol ~ "`, found %s";
        return Err(message, tok).nullable;
    }
    return Nullable!Err.init;
}

private bool nextIf(string symbol, Range)(auto ref Range toks)
{
    if (toks.nextIs!symbol)
    {
        toks.popFront;
        return true;
    }
    return false;
}

private bool nextIs(string symbol, Range)(auto ref Range toks)
{
    return toks.front.contains(sym!symbol);
}

private Tok pop(Range)(auto ref Range toks)
{
    Tok tok = toks.front;
    toks.popFront;
    return tok;
}

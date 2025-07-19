module evorc.display;

public import std.format : format;

import evorc;
import std;

string display(BinOp binOp)
{
    with (BinOp) final switch (binOp)
    {
    case add: return "+";
    case sub: return "-";
    case mul: return "*";
    case div: return "/";
    case rem: return "%";
    case bitwiseAnd: return "&";
    case bitwiseOr: return "|";
    case bitwiseXor: return "^";
    case bitwiseLeftShift: return "<<";
    case bitwiseRightShift: return ">>";
    case logicalAnd: return "&&";
    case logicalOr: return "||";
    case equalTo: return "==";
    case notEqualTo: return "!=";
    case lessThan: return "<";
    case greaterThan: return ">";
    case lessThanOrEqualTo: return "<=";
    case greaterThanOrEqualTo: return ">=";
    case arraySubscript: return "[]";
    case memberAccess: return ".";
    case memberAccessThroughPointer: return "->";
    }
}

string display(UnOp unOp)
{
    with (UnOp) final switch (unOp)
    {
    case plus: return "+";
    case minus: return "-";
    case bitwiseNot: return "~";
    case logicalNot: return "!";
    case pointerDereference: return "*";
    case addressOf: return "&";
    }
}

string display(PrimitiveType primitiveType)
{
    with (PrimitiveType) final switch (primitiveType)
    {
    case int_: return "int";
    case bool_: return "bool";
    case void_: return "void";
    }
}

string display(LinType* type) => (*type).match!(
    (LinPointer ptr) => display(ptr.pointee) ~ "*",
    (PrimitiveType primitiveType) => primitiveType.display,
);

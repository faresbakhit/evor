module evorc.x86;

import evorc.lin;
import std.algorithm;
import std.array;
import std.conv;
import std.format;
import std.functional;
import std.sumtype;
import std.typecons;

string x86(LinProgram prog)
{
    auto app = appender!string();
    app.intel_syntax();
    app.text();
    foreach (func; prog) x86(app, func);
    app.section(".note.GNU-stack", BitFlags!Flag(), Type.progbits);
    return app.data;
}

private {

void x86(Writer)(scope ref Writer sink, LinFunc func)
{
    sink.global(func.name);
    sink.type(func.name, TypeDescription.function_);
    sink.label(func.name);

    // function prologue
    sink.push(Reg.rbp);
    sink.mov(Reg.rbp, Reg.rsp);
    sink.sub(Reg.rsp, nextMultipleOf16(4*func.varCount));

    if (func.params.length > 0) sink.mov(mem(Reg.rbp, -4), Reg.edi);
    if (func.params.length > 1) sink.mov(mem(Reg.rbp, -8), Reg.esi);
    if (func.params.length > 2) sink.mov(mem(Reg.rbp, -12), Reg.edx);
    if (func.params.length > 3) sink.mov(mem(Reg.rbp, -16), Reg.ecx);

    foreach (stmt; func.block) (*stmt).match!(partial!(x86, sink));
}

void x86(Writer)(scope ref Writer sink, LinIf if_)
{
}

void x86(Writer)(scope ref Writer sink, LinAssign assign)
{
    assign.rhs.match!(
        (Atom atom)
        {
            atom.match!(
                (LinBool bool_)
                {
                    assign.lhs.match!(
                        (LinVar var)
                        {
                            sink.mov(var, bool_);
                        },
                        (Deref deref)
                        {
                        },
                    );
                },
                (LinInt int_)
                {
                    assign.lhs.match!(
                        (LinVar var)
                        {
                            sink.mov(var, int_);
                        },
                        (Deref deref)
                        {
                        },
                    );
                },
                (LinVar var)
                {
                    assign.lhs.match!(
                        (LinVar assigneeVar)
                        {
                            sink.mov(var, assigneeVar);
                        },
                        (Deref deref)
                        {
                        },
                    );
                },
            );
        },
        (LinUn un)
        {
        },
        (LinBin bin)
        {
            with (BinOp) final switch (bin.op)
            {
            case add:
                break;
            case sub:
                break;
            case mul:
                break;
            case div:
                break;
            case rem:
                break;
            case bitwiseAnd:
                break;
            case bitwiseOr:
                break;
            case bitwiseXor:
                break;
            case bitwiseLeftShift:
                break;
            case bitwiseRightShift:
                break;
            case logicalAnd:
                break;
            case logicalOr:
                break;
            case equalTo:
                break;
            case notEqualTo:
                break;
            case lessThan:
                break;
            case greaterThan:
                break;
            case lessThanOrEqualTo:
                break;
            case greaterThanOrEqualTo:
                break;
            case arraySubscript:
                break;
            case memberAccess:
                break;
            case memberAccessThroughPointer:
                break;
            }
        },
        (LinCall call)
        {
        },
    );
}

void x86(Writer)(scope ref Writer sink, LinReturn ret)
{
    if (ret.expr.isNull)
    {
        // function epilogue
        sink.leave();
        sink.ret();
        return;
    }
    auto expr = ret.expr.get;
    ret.expr.get.match!(
        (Atom atom)
        {
            atom.match!(a => sink.mov(Reg.eax, a));
        },
        (LinUn un)
        {
        },
        (LinBin bin)
        {
        },
        (LinCall call)
        {
        },
    );
    sink.leave();
    sink.ret();
}

void x86(Writer)(scope ref Writer sink, LinExpr expr)
{
    expr.match!(partial!(x86, sink));
}

void x86(Writer)(scope ref Writer sink, Atom atom)
{
    atom.match!(partial!(x86, sink));
}

void x86(Writer)(scope ref Writer sink, LinUn un)
{
}

void x86(Writer)(scope ref Writer sink, LinBin bin)
{
}

void x86(Writer)(scope ref Writer sink, LinCall call)
{
}

void x86(Writer)(scope ref Writer sink, LinBool bool_)
{
}

void x86(Writer)(scope ref Writer sink, LinInt int_)
{
}

void x86(Writer)(scope ref Writer sink, LinVar var)
{
}

void leave(Writer)(scope ref Writer sink)
{
    sink.formattedWrite("\tleave\n");
}

void ret(Writer)(scope ref Writer sink)
{
    sink.formattedWrite("\tret\n");
}

uint nextMultipleOf16(uint n)
{
    return ((n >> 4) + 1) << 4;
}

void sub(Writer)(scope ref Writer sink, Reg reg, uint n)
{
    sink.formattedWrite("\tsub\t%s, %s\n", reg.x86(), n);
}

void mov(Writer, U, V)(scope ref Writer sink, U src, V dest)
if (!(is(U : Mem) && is(V : Mem)))
{
    sink.formattedWrite("\tmov\t%s, %s\n", src.x86(), dest.x86());
}

string x86(LinVar var)
{
    return (*var.type).match!(
        (PrimitiveType type)
        {
            with (PrimitiveType) final switch (type)
            {
            case int_:
                return "dword ptr %s".format(mem(Reg.rbp, -4 * (var.id + 1)).x86());
            case bool_:
                return "byte ptr %s".format(mem(Reg.rbp, -4 * (var.id + 1)).x86());
            case void_:
                return "NOTIMPLEMENTED";
            }
        },
        (LinPointer pointer)
        {
            return "NOTIMPLEMENTED";
        }
    );
}

string x86(LinInt int_)
{
    return "%s".format(int_.value);
}

string x86(LinBool bool_)
{
    return "%s".format(cast(int)bool_.value);
}

struct Mem
{
    Reg base;
    Nullable!Reg index;
    uint scale;
    int displacement;

    string x86()
    {
        string basePart = base.x86();
        string indexPart = "";
        string dispPart = "";

        if (!index.isNull)
        {
            if (scale == 1)
            {
                indexPart = format("+%s", index.get.x86(), scale);
            }
            else
            {
                indexPart = format("+%s*%s", index.get.x86(), scale);
            }
        }

        if (displacement != 0)
        {
            dispPart = format("%+d", displacement);
        }

        return format("[%s%s%s]", basePart, indexPart, dispPart);
    }
}

Mem mem(Reg base)
{
    return Mem(base);
}

Mem mem(Reg base, int displacement)
{
    return Mem(base, Nullable!Reg.init, 0, displacement);
}

void push(Writer)(scope ref Writer sink, Reg reg)
{
    sink.formattedWrite("\tpush\t%s\n", reg.x86());
}

void label(Writer)(scope ref Writer sink, string name)
{
    sink.formattedWrite("%s:\n", name);
}

void intel_syntax(Writer)(scope ref Writer sink)
{
    sink.formattedWrite("\t.intel_syntax noprefix\n");
}

void type(Writer)(scope ref Writer sink, string name, TypeDescription typeDescription)
{
    sink.formattedWrite("\t.type\t%s, @%s\n", name, typeDescription.x86());
}

void global(Writer)(scope ref Writer sink, string symbol)
{
    sink.formattedWrite("\t.global\t%s\n", symbol);
}

void text(Writer)(scope ref Writer sink)
{
    sink.put("\t.text\n");
}

void section(Writer)(scope ref Writer sink, string name, BitFlags!Flag flags, Type type)
{
    sink.formattedWrite("\t.section\t%s,\"%s\",@%s\n", name, flags.x86(), type.x86());
}

enum TypeDescription
{
    function_,
}

string x86(TypeDescription typeDescription)
{
    with (TypeDescription) final switch (typeDescription)
    {
    case function_: return "function";
    }
}

enum Reg
{
    rax,
    rbx,
    rcx,
    rdx,
    rdi,
    rsi,
    rbp,
    rsp,
    eax,
    ebx,
    ecx,
    edx,
    edi,
    esi,
    ebp,
    esp,
}

string x86(Reg reg)
{
    return to!string(reg);
}

enum Flag
{
    none = 0,
    writable   = 1 << 0,
    executable = 1 << 1,
}

string x86(BitFlags!Flag flags)
{
    if (!flags) return "";

    auto result = appender!string();
    if (flags & Flag.writable)   result.put("w");
    if (flags & Flag.executable) result.put("x");
    return result.data;
}

enum Type
{
    progbits,
    nobits,
}

string x86(Type type)
{
    return to!string(type);
}

} // private

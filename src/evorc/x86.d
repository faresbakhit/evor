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
    
    // function epilogue
    sink.leave();
    sink.ret();
}

void x86(Writer)(scope ref Writer sink, LinIf if_)
{
}

void x86(Writer)(scope ref Writer sink, LinAssign assign)
{
}

void x86(Writer)(scope ref Writer sink, LinReturn ret)
{
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
    sink.formattedWrite("\tsub\t%s, %s\n", reg.toString(), n);
}

void mov(Writer, U, V)(scope ref Writer sink, U src, V dest)
if (!(is(U : Mem) && is(V : Mem)))
{
    sink.formattedWrite("\tmov\t%s, %s\n", src.toString(), dest.toString());
}

struct Mem
{
    Reg base;
    Nullable!Reg index;
    uint scale;
    int displacement;

    string toString()
    {
        string basePart = base.toString();
        string indexPart = "";
        string dispPart = "";

        if (!index.isNull)
        {
            if (scale == 1)
            {
                indexPart = format("+%s", index.get.toString(), scale);
            }
            else
            {
                indexPart = format("+%s*%s", index.get.toString(), scale);
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
    sink.formattedWrite("\tpush\t%s\n", reg.toString());
}

void label(Writer)(scope ref Writer sink, string name)
{
    sink.formattedWrite("%s:\n", name);
}

void intel_syntax(Writer)(scope ref Writer sink)
{
    sink.formattedWrite("\t.intel-syntax\n");
}

void type(Writer)(scope ref Writer sink, string name, TypeDescription typeDescription)
{
    sink.formattedWrite("\t.type\t%s, @%s\n", name, typeDescription.toString());
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
    sink.formattedWrite("\t.section\t%s,\"%s\",@%s\n", name, flags.toString(), type.toString());
}

enum TypeDescription
{
    function_,
}

string toString(TypeDescription typeDescription)
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

string toString(Reg reg)
{
    return to!string(reg);
}

enum Flag
{
    none = 0,
    writable   = 1 << 0,
    executable = 1 << 1,
}

string toString(BitFlags!Flag flags)
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

string toString(Type type)
{
    return to!string(type);
}

} // private

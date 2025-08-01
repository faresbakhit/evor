/**
 * Generates AMD x86-64 assembly targeting the GNU Assembler
 * from Evor's three-address code representation.
 *
 * BEX861: Code generation
 */

module evorc.x86;

string x86(Program prog)
{
    auto app = appender!string();
    x86(prog, app);
    return app.data;
}

void x86(Writer)(Program prog, auto ref Writer sink)
{
    sink.directive("intel_syntax", "noprefix");
    sink.directive("text");
    foreach (func; prog.funcs)
    {
        x86(sink, prog.rec, func);
    }
    sink.directive("section", ".note.GNU-stack", `""`, "@progbits");
}

private
{
import evorc.tac;
import std.algorithm;
import std.array;
import std.conv;
import std.format;
import std.functional;
import std.sumtype;
import std.traits;
import std.typecons;

void x86(W)(auto ref W s, const ref Record rec, Func func)
{
    auto funcName = rec.getFuncName(func.id);

    s.directive("global", funcName);
    s.directive("type", funcName, "@function");
    s.label(funcName);

    s.instr(Instr.push, Reg.rbp);
    s.instr(Instr.mov, Reg.rbp, Reg.rsp);

    Stack stack;
    auto ss = appender!string();

    void paramFrom(Reg srcReg)(Var param) {
        auto stackAddr = stack.addrOf(param);
        ss.instr(Instr.mov, stackAddr, reg(stackAddr.size, srcReg));
    }

    if (func.params.length > 0) paramFrom!(Reg.rdi)(func.params[0]);
    if (func.params.length > 1) paramFrom!(Reg.rsi)(func.params[1]);
    if (func.params.length > 2) paramFrom!(Reg.rdx)(func.params[2]);
    if (func.params.length > 3) paramFrom!(Reg.rcx)(func.params[3]);
    if (func.params.length > 4) paramFrom!(Reg.r8)(func.params[4]);
    if (func.params.length > 5) paramFrom!(Reg.r9)(func.params[5]);

    foreach (inst; func.instrs)
    {
        x86(ss, stack, inst);
    }

    auto nextMultipleOf16 = (int n) => (n + 15) & ~15;
    s.instr(Instr.sub, Reg.rsp, nextMultipleOf16(stack.size));
    s.put(ss[]);
    s.directive("size", funcName, ".-" ~ funcName);
}

void x86(W)(auto ref W s, ref Stack stack, Inst inst)
{
    inst.match!(i => x86(s, stack, i));
}

void x86(W)(auto ref W s, ref Stack stack, Label label)
{
    s.label(label);
}

void x86(W)(auto ref W s, ref Stack stack, Jmp jmp)
{
    s.instr(Instr.jmp, jmp.label);
}

void x86(W)(auto ref W s, ref Stack stack, Jcc jcc)
{
    jcc.cond.match!(
        (Var var)
        {
            s.instr(Instr.cmp, stack.addrOf(var), 0);
            s.instr(Instr.jne, jcc.label);
        },
        (Bool bool_)
        {
            if (bool_.value == true)
            {
                s.instr(Instr.jmp, jcc.label);
            }
        },
        _ => assert(0),
    );
}

void x86(W)(auto ref W s, ref Stack stack, Bin bin)
{
    auto dest = stack.addrOf(bin.dest);

    template additive(alias instr)
    {
        alias additive = match!(
            (Var var1, Var var2)
            {
                s.instr(Instr.mov, Reg.eax, stack.addrOf(var1));
                s.instr(Instr.add, Reg.eax, stack.addrOf(var2));
                s.instr(Instr.mov, dest, Reg.eax);
            },
            (Var var, atom)
            {
                s.instr(Instr.mov, Reg.eax, stack.addrOf(var));
                s.instr(Instr.add, Reg.eax, atom);
                s.instr(Instr.mov, dest, Reg.eax);
            },
            (atom, Var var)
            {
                s.instr(Instr.mov, Reg.eax, atom);
                s.instr(Instr.add, Reg.eax, stack.addrOf(var));
                s.instr(Instr.mov, dest, Reg.eax);
            },
            (atom1, atom2)
            {
                s.instr(Instr.mov, Reg.eax, atom1);
                s.instr(Instr.add, Reg.eax, atom2);
                s.instr(Instr.mov, dest, Reg.eax);
            },
        );
    }

    template cmp(alias instr, alias invInstr)
    {
        alias cmp = match!(
            (Var var1, Var var2)
            {
                auto var1Mem = stack.addrOf(var1);
                auto var2Mem = stack.addrOf(var2);
                auto temp = reg(var1Mem.size, Reg.rax);
                s.instr(Instr.mov, temp, var1Mem);
                s.instr(Instr.cmp, temp, var2Mem);
                s.instr(instr, dest);
            },
            (Var var, i)
            {
                s.instr(Instr.cmp, stack.addrOf(var), i);
                s.instr(instr, dest);
            },
            (i, Var var)
            {
                s.instr(Instr.cmp, stack.addrOf(var), i);
                s.instr(invInstr, dest);
            },
            (i1, i2)
            {
                auto temp = reg(sizeof(i1.type), Reg.rax);
                s.instr(Instr.mov, temp, i1);
                s.instr(Instr.cmp, temp, i2);
                s.instr(instr, dest);
            },
        );
    }

    with (BinOp) final switch (bin.op)
    {
    case add:
        additive!(Instr.add)(bin.lhs, bin.rhs);
        break;
    case sub:
        additive!(Instr.sub)(bin.lhs, bin.rhs);
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
        cmp!(Instr.sete, Instr.setne)(bin.lhs, bin.rhs);
        break;
    case notEqualTo:
        cmp!(Instr.setne, Instr.sete)(bin.lhs, bin.rhs);
        break;
    case lessThan:
        cmp!(Instr.setl, Instr.setge)(bin.lhs, bin.rhs);
        break;
    case greaterThan:
        cmp!(Instr.setg, Instr.setle)(bin.lhs, bin.rhs);
        break;
    case lessThanOrEqualTo:
        cmp!(Instr.setle, Instr.setg)(bin.lhs, bin.rhs);
        break;
    case greaterThanOrEqualTo:
        cmp!(Instr.setge, Instr.setl)(bin.lhs, bin.rhs);
        break;
    case arraySubscript:
        break;
    case memberAccess:
        break;
    case memberAccessThroughPointer:
        break;
    }
}

void x86(W)(auto ref W s, ref Stack stack, Un un)
{
}

void x86(W)(auto ref W s, ref Stack stack, Assign assign)
{
    auto dest = stack.addrOf(assign.dest);
    assign.src.match!(
        (Var var)
        {
            auto size = sizeof(var.type);
            auto src = stack.addrOf(var);
            auto temp = reg(size, Reg.rax);
            s.instr(Instr.mov, temp, src);
            s.instr(Instr.mov, dest, temp);
        },
        atom => s.instr(Instr.mov, dest, atom),
    );
}

void x86(W)(auto ref W s, ref Stack stack, Load load)
{
}

void x86(W)(auto ref W s, ref Stack stack, Store store)
{
}

void x86(W)(auto ref W s, ref Stack stack, Param param)
{
}

void x86(W)(auto ref W s, ref Stack stack, Call call)
{
}

void x86(W)(auto ref W s, ref Stack stack, Return ret)
{
    ret.val.match!(
        (Var var)
        {
            auto varMem = stack.addrOf(var);
            auto rax = reg(varMem.size, Reg.rax);
            s.instr(Instr.mov, rax, varMem);
        },
        (atom)
        {
            auto rax = reg(sizeof(atom.type), Reg.rax);
            s.instr(Instr.mov, rax, atom);
        }
    );
    s.instr(Instr.leave);
    s.instr(Instr.ret);
}

void x86(W)(auto ref W s, ref Stack stack, Leave leave)
{
    s.instr(Instr.leave);
    s.instr(Instr.ret);
}

struct Stack
{
    Mem addrOf(Var var)
    {
        auto varSize = sizeof(var.type);
        auto offset = offsets.require(var.id, size += varSize);
        return mem(varSize, Reg.rbp, -offset);
    }

    uint size;
    uint[VarId] offsets;
}

void label(W)(auto ref W s, string name)
{
    s.formattedWrite("%s:\n", name);
}

void label(W)(auto ref W s, Label label)
{
    s.formattedWrite(".L%d:\n", label.id);
}

void directive(W, Name, Args...)(auto ref W s, Name name, Args args)
{
    static if (Args.length == 0)
        s.formattedWrite("\t.%s\n", name);
    else
        s.formattedWrite("\t.%s\t%-(%s, %)\n", name, [args]);
}

enum Instr
{
    mov, lea, add, sub, and, or, xor, cmp,
    sete, setne, setl, setg, setle, setge,
    push, neg, not, imul, idiv, call, jmp,
    jne,
    leave, ret, cdq,
}

void instr(W)(auto ref W s, Instr instr)
{
    s.formattedWrite("\t%s\n", instr);
}

void instr(W, Op)(auto ref W s, Instr instr, Op op)
{
    s.formattedWrite("\t%s\t%s\n", instr, arg(op));
}

void instr(W, Src, Dest)(auto ref W s, Instr instr, Src src, Dest dest)
{
    s.formattedWrite("\t%s\t%s, %s\n", instr, arg(src), arg(dest));
}

void instr(W, Aux, Src, Dest)(auto ref W s, Instr instr, Aux aux, Src src, Dest dest)
{
    s.formattedWrite("\t%s\t%s, %s, %s\n", instr, arg(aux), arg(src), arg(dest));
}

enum Reg
{
     rax,    rbx,    rcx,    rdx,  rsi, rdi, rsp, rbp, rip, r8,  r9,  r10,  r11,  r12,  r13,  r14,  r15,  // 64-bits
     eax,    ebx,    ecx,    edx,  esi, edi, esp, ebp, eip, r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d, // 32-bits
      ax,     bx,     cx,     dx,   si,  di,  sp,  bp,  ip, r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w, // 16-bits
    ah, al, bh, bl, ch, cl, dh, dl, sil, dil              , r8b, r9b, r10b, r11b, r12b, r13b, r14b, r15b, // 8-bits
}

Reg reg(uint size, Reg genReg)
{
    with (Reg) switch (genReg)
    {
    case rax:
        switch (size)
        {
        case 1: return al;
        case 2: return ax;
        case 4: return eax;
        default: return rax;
        }
    case rbx:
        switch (size)
        {
        case 1: return bl;
        case 2: return bx;
        case 4: return ebx;
        default: return rbx;
        }
    case rcx:
        switch (size)
        {
        case 1: return cl;
        case 2: return cx;
        case 4: return ecx;
        default: return rcx;
        }
    case rdx:
        switch (size)
        {
        case 1: return dl;
        case 2: return dx;
        case 4: return edx;
        default: return rdx;
        }
    case rsi:
        switch (size)
        {
        case 1: return sil;
        case 2: return si;
        case 4: return esi;
        default: return rsi;
        }
    case rdi:
        switch (size)
        {
        case 1: return dil;
        case 2: return di;
        case 4: return edi;
        default: return rdi;
        }
    case rsp:
        switch (size)
        {
        case 2: return sp;
        case 4: return esp;
        default: return rsp;
        }
    case rbp:
        switch (size)
        {
        case 2: return bp;
        case 4: return ebp;
        default: return rbp;
        }
    case rip:
        switch (size)
        {
        case 2: return ip;
        case 4: return eip;
        default: return rip;
        }
    case r8:
        switch (size)
        {
        case 1: return r8b;
        case 2: return r8w;
        case 4: return r8d;
        default: return r8;
        }
    case r9:
        switch (size)
        {
        case 1: return r9b;
        case 2: return r9w;
        case 4: return r9d;
        default: return r9;
        }
    case r10:
        switch (size)
        {
        case 1: return r10b;
        case 2: return r10w;
        case 4: return r10d;
        default: return r10;
        }
    case r11:
        switch (size)
        {
        case 1: return r11b;
        case 2: return r11w;
        case 4: return r11d;
        default: return r11;
        }
    case r12:
        switch (size)
        {
        case 1: return r12b;
        case 2: return r12w;
        case 4: return r12d;
        default: return r12;
        }
    case r13:
        switch (size)
        {
        case 1: return r13b;
        case 2: return r13w;
        case 4: return r13d;
        default: return r13;
        }
    case r14: 
        switch (size)
        {
        case 1: return r14b;
        case 2: return r14w;
        case 4: return r14d;
        default: return r14;
        }
    case r15:
        switch (size)
        {
        case 1: return r15b;
        case 2: return r15w;
        case 4: return r15d;
        default: return r15;
        }
    default: return rax;
    }
}

string arg(Reg reg)
{
    return to!string(reg);
}

string arg(Mem mem)
{
    string size;
    string basePart = arg(mem.base);
    string indexPart;
    string dispPart;

    if (mem.size != 0)
    {
        switch (mem.size)
        {
        case 1: size = "byte ptr "; break;
        case 2: size = "word ptr "; break;
        case 4: size = "dword ptr "; break;
        case 8: size = "qword ptr "; break;
        default: break;
        }
    }

    if (!mem.index.isNull)
    {
        if (mem.scale == 1)
        {
            indexPart = format("+%s", mem.index.get, mem.scale);
        }
        else
        {
            indexPart = format("+%s*%s", mem.index.get, mem.scale);
        }
    }

    if (mem.displacement != 0)
    {
        dispPart = format("%+d", mem.displacement);
    }

    return format("%s[%s%s%s]", size, basePart, indexPart, dispPart);
}

string arg(Int)(Int int_)
if (isIntegral!(Int))
{
    return to!string(int_);
}

string arg(Label label)
{
    return format(".L%d", label.id);
}

string arg(Int int_)
{
    return to!string(int_.value);
}

string arg(Bool bool_)
{
    return to!string(cast(int)bool_.value);
}

struct Mem
{
    Reg base;
    Nullable!Reg index;
    uint scale;
    int displacement;
    uint size;
}

Mem mem(uint size, Reg base)
{
    return Mem(base, Nullable!Reg.init, 0, 0, size);
}

Mem mem(Reg base)
{
    return Mem(base);
}

Mem mem(uint size, Reg base, int displacement)
{
    return Mem(base, Nullable!Reg.init, 0, displacement, size);
}

Mem mem(Reg base, int displacement)
{
    return Mem(base, Nullable!Reg.init, 0, displacement);
}

uint sizeof(Type type) => type.match!(
    (Primitive primitive)
    {
        with (Primitive) final switch (primitive)
        {
        case int_: return 4;
        case bool_: return 1;
        case void_: return 0;
        }
    },
    (Struct struct_) => 16,
    (Pointer pointer) => 8,
);
}

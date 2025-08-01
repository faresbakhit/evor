module evorc.x64;

string x64(LinProgram prog)
{
    auto app = appender!string();
    x64(prog, app);
    return app.data;
}

void x64(Writer)(LinProgram prog, auto ref Writer sink)
{
    sink.directive("intel_syntax", "noprefix");
    sink.directive("text");
    foreach (func; prog) sink.x64(func);
    sink.directive("section", ".note.GNU-stack", `""`, "@progbits");
}

private
{
import evorc.lin;
import std.algorithm;
import std.array;
import std.conv;
import std.format;
import std.functional;
import std.sumtype;
import std.traits;
import std.typecons;

void x64(W)(auto ref W s, LinFunc func)
{
    s.directive("global", func.name);
    s.directive("type", func.name, "@function");
    s.label(func.name);

    s.instr(Instr.push, Reg.rbp);
    s.instr(Instr.mov, Reg.rbp, Reg.rsp);

    auto ss = appender!string();
    uint stackSize;
    uint[VarId] stackOffsets;

    Mem mem(LinVar var)
    {
        auto varSize = sizeof(var.type);
        auto varOffset = stackOffsets.require(var.id, stackSize += varSize);
        return .mem(varSize, Reg.rbp, -varOffset);
    }

    void paramFrom(Reg srcReg)(LinVar param) {
        auto size = sizeof(param.type);
        auto offset = stackOffsets.require(param.id, stackSize += size);
        ss.instr(Instr.mov, .mem(size, Reg.rbp, -offset), reg(size, srcReg));
    }

    if (func.params.length > 0) paramFrom!(Reg.rdi)(func.params[0]);
    if (func.params.length > 1) paramFrom!(Reg.rsi)(func.params[1]);
    if (func.params.length > 2) paramFrom!(Reg.rdx)(func.params[2]);
    if (func.params.length > 3) paramFrom!(Reg.rcx)(func.params[3]);
    if (func.params.length > 4) paramFrom!(Reg.r8)(func.params[4]);
    if (func.params.length > 5) paramFrom!(Reg.r9)(func.params[5]);

    foreach (stmt; func.block)
    {
        (*stmt).match!(
            (LinIf if_) {},
            (LinAssign assign)
            {
                assign.lhs.match!(
                    (LinVar var)
                    {
                        auto size = sizeof(var.type);
                        auto offset = stackOffsets.require(var.id, stackSize += size);
                        auto dest = .mem(size, Reg.rbp, -offset);
                        auto mov(Flag!"destIsRax" destIsRax = No.destIsRax)(Atom atom, Instr instr = Instr.mov) {
                            static if (destIsRax)
                                auto dest = reg(size, Reg.rax);
                            else
                                auto dest = .mem(size, Reg.rbp, -offset);
                            atom.match!(
                                (LinBool bool_)
                                {
                                    ss.instr(instr, dest, bool_);
                                },
                                (LinInt int_)
                                {
                                    ss.instr(instr, dest, int_);
                                },
                                (LinVar assignedVar)
                                {
                                    auto assignedSize = sizeof(assignedVar.type);
                                    auto assignedOffset = stackOffsets.require(assignedVar.id, stackSize += size);
                                    ss.instr(Instr.mov, reg(assignedSize, Reg.rax), .mem(assignedSize, Reg.rbp, -assignedOffset));
                                    ss.instr(instr, dest, reg(size, Reg.rax));
                                },
                            );
                        }
                        assign.rhs.match!(
                            mov!(No.destIsRax),
                            (LinUn un)
                            {
                                with (UnOp) final switch (un.op)
                                {
                                case plus:
                                    mov(un.atom);
                                    break;
                                case minus:
                                    mov(un.atom);
                                    ss.instr(Instr.neg, dest);
                                    break;
                                case bitwiseNot:
                                    mov(un.atom);
                                    ss.instr(Instr.not, dest);
                                    break;
                                case logicalNot:
                                    mov(un.atom);
                                    ss.instr(Instr.xor, dest, 1);
                                    break;
                                case pointerDereference:
                                    auto lval = un.atom.get!LinVar;
                                    auto lvalSize = sizeof(lval.type);
                                    auto lvalOffset = stackOffsets.require(lval.id, stackSize += size);
                                    auto l = Reg.rax;
                                    auto a = reg(size, Reg.rax);
                                    ss.instr(Instr.mov, l, .mem(Reg.rbp, -lvalOffset));
                                    ss.instr(Instr.mov, a, .mem(l));
                                    ss.instr(Instr.mov, dest, a);
                                    break;
                                case addressOf:
                                    auto lval = un.atom.get!LinVar;
                                    auto lvalSize = sizeof(lval.type);
                                    auto lvalOffset = stackOffsets.require(lval.id, stackSize += size);
                                    auto l = Reg.rax;
                                    ss.instr(Instr.lea, l, .mem(Reg.rbp, -lvalOffset));
                                    ss.instr(Instr.mov, dest, l);
                                    break;
                                }
                            },
                            (LinBin bin)
                            {
                                template cmp(alias instr, alias invInstr)
                                {
                                    alias cmp = match!(
                                        (LinVar var1, LinVar var2)
                                        {
                                            auto a = reg(sizeof(var1.type), Reg.rax);
                                            ss.instr(Instr.mov, a, mem(var1));
                                            ss.instr(Instr.cmp, a, mem(var2));
                                            ss.instr(instr, dest);
                                        },
                                        (LinVar var, i)
                                        {
                                            ss.instr(Instr.cmp, mem(var), i);
                                            ss.instr(instr, dest);
                                        },
                                        (i, LinVar var)
                                        {
                                            ss.instr(Instr.cmp, mem(var), i);
                                            ss.instr(invInstr, dest);
                                        },
                                        (i1, i2)
                                        {
                                            auto a = reg(sizeof(i1.type), Reg.rax);
                                            ss.instr(Instr.mov, a, i1);
                                            ss.instr(Instr.cmp, a, i2);
                                            ss.instr(instr, dest);
                                        },
                                    );
                                }

                                template additive(alias instr)
                                {
                                    alias additive = match!(
                                        (LinVar var1, LinVar var2)
                                        {
                                            auto a = reg(sizeof(var1.type), Reg.rax);
                                            ss.instr(Instr.mov, a, mem(var1));
                                            ss.instr(instr, a, mem(var2));
                                            ss.instr(Instr.mov, dest, a);
                                        },
                                        (LinVar var, i)
                                        {
                                            ss.instr(Instr.mov, Reg.eax, mem(var));
                                            ss.instr(instr, Reg.eax, i);
                                            ss.instr(Instr.mov, dest, Reg.eax);
                                        },
                                        (i, LinVar var)
                                        {
                                            ss.instr(Instr.mov, Reg.eax, i);
                                            ss.instr(instr, Reg.eax, mem(var));
                                            ss.instr(Instr.mov, dest, Reg.eax);
                                        },
                                        (i1, i2)
                                        {
                                            ss.instr(Instr.mov, Reg.eax, i1);
                                            ss.instr(instr, Reg.eax, i2);
                                            ss.instr(Instr.mov, dest, Reg.eax);
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
                                    mov(bin.lhs);
                                    mov!(Yes.destIsRax)(bin.rhs, Instr.mov);
                                    ss.instr(Instr.imul, .mem(size, Reg.rbp, -offset));
                                    ss.instr(Instr.mov, .mem(size, Reg.rbp, -offset), reg(size, Reg.rax));
                                    break;
                                case div:
                                    mov!(Yes.destIsRax)(bin.lhs, Instr.mov);
                                    mov(bin.rhs);
                                    ss.instr(Instr.cdq);
                                    ss.instr(Instr.idiv, .mem(size, Reg.rbp, -offset));
                                    ss.instr(Instr.mov, .mem(size, Reg.rbp, -offset), reg(size, Reg.rax));
                                    break;
                                case rem:
                                    mov!(Yes.destIsRax)(bin.lhs, Instr.mov);
                                    mov(bin.rhs);
                                    ss.instr(Instr.cdq);
                                    ss.instr(Instr.idiv, .mem(size, Reg.rbp, -offset));
                                    ss.instr(Instr.mov, .mem(size, Reg.rbp, -offset), reg(size, Reg.rdx));
                                    break;
                                case bitwiseAnd:
                                case logicalAnd:
                                    additive!(Instr.and)(bin.lhs, bin.rhs);
                                    break;
                                case bitwiseOr:
                                case logicalOr:
                                    additive!(Instr.or)(bin.lhs, bin.rhs);
                                    break;
                                case bitwiseXor:
                                    additive!(Instr.xor)(bin.lhs, bin.rhs);
                                    break;
                                case bitwiseLeftShift:
                                    break;
                                case bitwiseRightShift:
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
                            },
                            (LinCall call)
                            {
                                auto argInto(Reg destReg)(Atom atom) {
                                    atom.match!(
                                        (LinBool bool_)
                                        {
                                            ss.instr(Instr.mov, reg(1, destReg), bool_);
                                        },
                                        (LinInt int_)
                                        {
                                            ss.instr(Instr.mov, reg(4, destReg), int_);
                                        },
                                        (LinVar var)
                                        {
                                            ss.instr(Instr.mov, reg(size, destReg), mem(var));
                                        },
                                    );
                                }
                                if (call.args.length > 0) argInto!(Reg.rdi)(call.args[0]);
                                if (call.args.length > 1) argInto!(Reg.rsi)(call.args[1]);
                                if (call.args.length > 2) argInto!(Reg.rdx)(call.args[2]);
                                if (call.args.length > 3) argInto!(Reg.rcx)(call.args[3]);
                                if (call.args.length > 4) argInto!(Reg.r8)(call.args[4]);
                                if (call.args.length > 5) argInto!(Reg.r9)(call.args[5]);
                                ss.instr(Instr.call, call.func);
                                ss.instr(Instr.mov, dest, reg(size, Reg.rax));
                            },
                        );
                    },
                    (Deref deref) {},
                );
            },
            (LinReturn ret)
            {
                if (ret.expr.isNull)
                {
                    ss.instr(Instr.leave);
                    ss.instr(Instr.ret);
                    return;
                }
                auto expr = ret.expr.get;
                expr.match!(
                    (Atom atom)
                    {
                        atom.match!(
                            (LinBool bool_)
                            {
                                ss.instr(Instr.mov, Reg.al, bool_);
                            },
                            (LinInt int_)
                            {
                                ss.instr(Instr.mov, Reg.eax, int_);
                            },
                            (LinVar var)
                            {
                                ss.instr(Instr.mov, reg(sizeof(var.type), Reg.rax), mem(var));
                            },
                        );
                    },
                    (LinUn un) {},
                    (LinBin bin) {},
                    (LinCall call) {},
                );
                ss.instr(Instr.leave);
                ss.instr(Instr.ret);
            },
            (LinExpr expr)
            {
                if (!expr.has!LinCall) return;
                auto mov(Reg destReg)(Atom atom) {
                    atom.match!(
                        (LinBool bool_)
                        {
                            ss.instr(Instr.mov, reg(1, destReg), bool_);
                        },
                        (LinInt int_)
                        {
                            ss.instr(Instr.mov, reg(4, destReg), int_);
                        },
                        (LinVar var)
                        {
                            ss.instr(Instr.mov, reg(sizeof(var.type), destReg), mem(var));
                        },
                    );
                }
                auto call = expr.get!LinCall;
                if (call.args.length > 0) mov!(Reg.rdi)(call.args[0]);
                if (call.args.length > 1) mov!(Reg.rsi)(call.args[1]);
                if (call.args.length > 2) mov!(Reg.rdx)(call.args[2]);
                if (call.args.length > 3) mov!(Reg.rcx)(call.args[3]);
                if (call.args.length > 4) mov!(Reg.r8)(call.args[4]);
                if (call.args.length > 5) mov!(Reg.r9)(call.args[5]);
                ss.instr(Instr.call, call.func);
            },
        );
    }

    s.instr(Instr.sub, Reg.rsp, nextMultipleOf16(stackSize));
    s.put(ss[]);
    s.directive("size", func.name, ".-" ~ func.name);
}

int nextMultipleOf16(int n)
{
    return (n + 15) & ~15;
}

void label(W, Name)(auto ref W s, Name name)
{
    s.formattedWrite("%s:\n", name);
}

void directive(W, Name, Args...)(auto ref W s, Name name, Args args)
{
    static if (Args.length == 0)
        s.formattedWrite("\t.%s\n", name);
    else
        s.formattedWrite("\t.%s\t%-(%s, %)\n", name, [args]);
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

enum Instr
{
    mov, lea, add, sub, and, or, xor, cmp,
    sete, setne, setl, setg, setle, setge,
    push, neg, not, imul, idiv, call,
    leave, ret, cdq, 
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

string arg(Reg reg)
{
    return to!string(reg);
}

string arg(LinInt int_)
{
    import std.bigint : toDecimalString;
    return toDecimalString(int_.value);
}

string arg(Int)(Int int_)
if (isIntegral!(Int))
{
    return to!string(int_);
}

string arg(bool bool_)
{
    return to!string(cast(int)bool_);
}

string arg(LinBool bool_)
{
    return to!string(cast(int)bool_.value);
}

string arg(string s)
{
    return s;
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

uint sizeof(LinType* type) => (*type).match!(
    (LinPointer pointer) => 8,
    (PrimitiveType type)
    {
        with (PrimitiveType) final switch (type)
        {
        case int_: return 4;
        case bool_: return 1;
        case void_: return 0;
        }
    }
);
} // private

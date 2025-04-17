import traits;

import std.functional : partial;
import std.sumtype : SumType, match;
import std.typecons : Tuple, Nullable, nullable;
import std.traits : EnumMembers;
import std.meta : allSatisfy;
import std.conv : to;

enum Reg
{
    rax,
    rbx,
    rcx,
    rdx,
    rsi,
    rdi,
    rsp,
    rbp,
    r8,
    r9,
    r10,
    r11,
    r12,
    r13,
    r14,
    r15
}

enum UnInstrType
{
    negq,
    callq,
}

enum BinInstrType
{
    movq,
    addq,
    subq,
}

alias Imm = Tuple!(long, "val");
alias Label = Tuple!(string, "name");
alias Deref = Tuple!(Reg, "reg", Nullable!long, "offset");
alias Operand = SumType!(Reg, Imm, Label, Deref);
alias UnInstr = Tuple!(UnInstrType, "type", Operand*, "dst");
alias BinInstr = Tuple!(BinInstrType, "type", Operand*, "src", Operand*, "dst");
alias Instr = SumType!(UnInstr, BinInstr);
alias X86 = string;

struct Program
{
    Instr*[] instrs;
    ulong stackSize;
    alias instrs this;
}

Operand* imm(long val) {
    return new Operand(Imm(val));
}

Operand* label(string name) {
    return new Operand(Label(name));
}

Operand* deref(Reg reg, long offset) {
    return new Operand(Deref(reg, offset.nullable));
}

Operand* deref(Reg reg) {
    return new Operand(Deref(reg, Nullable!long()));
}

Operand* op(Reg reg) {
    return new Operand(reg);
}

Instr* unInstr(UnInstrType type, Operand* dst)
{
    return new Instr(UnInstr(type, dst));
}

static foreach (instrType; EnumMembers!UnInstrType)
    mixin("alias " ~ to!string(instrType) ~ "=partial!(unInstr,UnInstrType." ~ to!string(instrType) ~ ");");

Instr* binInstr(BinInstrType type, Operand* src, Operand* dst)
{
    return new Instr(BinInstr(type, src, dst));
}

static foreach (instrType; EnumMembers!BinInstrType)
    mixin("alias " ~ to!string(instrType) ~ "=partial!(binInstr,BinInstrType." ~ to!string(instrType) ~ ");");

string genCode(Operand* op)
{
    import std.conv : to;
    return (*op).match!(
        (Reg reg)
        {
            final switch (reg)
            {
                static foreach (r; EnumMembers!Reg)
                {
                    case r:
                        return mixin("\"%" ~ to!string(r) ~ "\"");
                }
            }
        },
        (Imm im) => "$" ~ to!string(im.val),
        (Label label) => label.name,
        (Deref deref)
        {
            if (deref.offset.isNull())
            {
                return "(" ~ deref.reg.op.genCode() ~ ")";
            }
            else
            {
                
                return to!string(deref.offset) ~ "(" ~ deref.reg.op.genCode() ~ ")";
            }
        }
    );
}

string genCode(Instr *instr)
{
    return (*instr).match!(
        (UnInstr un)
        {
            final switch (un.type)
            {
                static foreach (t; EnumMembers!UnInstrType)
                {
                    case t:
                        return to!string(t) ~ "\t" ~ un.dst.genCode();
                }
            }
        },
        (BinInstr bin)
        {
            final switch (bin.type)
            {
                static foreach (t; EnumMembers!BinInstrType)
                {
                    case t:
                        return to!string(t) ~ "\t" ~ bin.src.genCode() ~ "," ~ bin.dst.genCode();
                }
            }
        },
    );
}

string genCode(ref Program prog)
{
    import std.array : join;
    import std.algorithm : map;
    if (prog.stackSize % 16 != 0)
    {
        prog.stackSize += prog.stackSize % 16;
    }
    return "	.text
	.global	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$" ~ to!string(prog.stackSize) ~ ", %rsp
	"
	~
    prog.map!genCode.join("\n\t")
    ~ "
	xorl	%eax, %eax
	leaveq
	retq
	.type	print_int, @function
print_int:
	movq	%rdi, %rsi
	leaq	.L.str.0(%rip), %rdi
	xorl	%eax, %eax
	jmp	printf@PLT
	.type	read_int, @function
read_int:
	pushq	%rax
	leaq	.L.str.1(%rip), %rdi
	xorl	%eax, %eax
	callq	printf@PLT
	leaq	.L.str.2(%rip), %rdi
	movq	%rsp, %rsi
	xorl	%eax, %eax
	callq	scanf@PLT
	movq	(%rsp), %rax
	popq	%rcx
	retq
	.section	.rodata
.L.str.0:
	.string	\"%ld\\n\"
.L.str.1:
	.string	\"Enter an integer: \"
.L.str.2:
	.string	\"%ld\"
"
    ;
}

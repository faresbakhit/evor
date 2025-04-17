import ast;
import x86;
import interp;
import lowering;
import codegen;

import std.file : write;
import std.stdio : stdout, stderr, writeln, File;
import std.meta : allSatisfy;
import std.traits : allSameType;
import std.process : pipeProcess, wait, Redirect, ProcessPipes, spawnProcess;
import std.algorithm : each;
import std.getopt;

void getoptFormatter(Output)(Output output, Option[] opt)
{
    import std.algorithm.comparison : min, max;
    import std.format.write : formattedWrite;

    output.formattedWrite("evorc 0.1.0\nFares A. Bakhit <faresa.bakhit@gmail.com>\n\n");

    size_t ls, ll;
    bool hasRequired = false;
    foreach (it; opt)
    {
        ls = max(ls, it.optShort.length);
        ll = max(ll, it.optLong.length);

        hasRequired = hasRequired || it.required;
    }

    string style = "    %*s %*s  %s\n";

    output.formattedWrite("Required options:\n");
    foreach (it; opt)
    {
        if (it.required)
        {
            output.formattedWrite(style, ls, it.optShort, ll, it.optLong, it.help);
        }
    }

    output.formattedWrite("\nOptions:\n");
    foreach (it; opt)
    {
        if (!it.required)
        {
            output.formattedWrite(style, ls, it.optShort, ll, it.optLong, it.help);
        }
    }
}

int gcc(string[] args, string stdin)
{
    int status;
    ProcessPipes pipe;
    {
        pipe = pipeProcess("gcc" ~ args, Redirect.stdin | Redirect.stderr);
        pipe.stdin.write(stdin);
        pipe.stdin.close();
        scope(exit) status = wait(pipe.pid);
        pipe.stderr.byLine.each!writeln;
    }
    return status;
    
}

int gcc(string[] args)
{
    int status;
    ProcessPipes pipe;
    {
        pipe = pipeProcess("gcc" ~ args, Redirect.stderr);
        scope(exit) status = wait(pipe.pid);
        pipe.stderr.byLine.each!writeln;
    }
    return status;
}

int main(string[] args)
{
    auto m = mod(
        assign(item("x"), add(lit(42), minus(lit(10)))),
        call(item("print"), add(call(item("input_int")), minus(item("x")))),
    );
    Program prog = compile(flatten(m));
    X86 code = genCode(prog);
    string outFile;
    bool compileOnly;
    bool compileAndAssembleOnly;
    bool saveTemps;
    string[] ccFlags;
    GetoptResult helpInformation;

    try
    {
        helpInformation = getopt(args,
            std.getopt.config.required,
            "o", "Write output to file.", &outFile,
            "S", "Compile only; do not assemble or link.", &compileOnly,
            "c", "Compile and assemble, but do not link.", &compileAndAssembleOnly,
            "v", "Save intermediate compilation results.", &saveTemps,
            "X", "Pass an option to the link and assemble driver (gcc).", &ccFlags,
        );
    }
    catch (GetOptException)
    {
        stderr.writeln("Error: Missing output file");
        return 2;
    }

    if (helpInformation.helpWanted)
    {
        getoptFormatter(stdout.lockingTextWriter(), helpInformation.options);
        return 0;
    }

    string asmFile = outFile ~ ".s";
    string objFile = outFile ~ ".o";

    if (compileOnly || saveTemps)
    {
        asmFile.write(code);
    }

    if (compileOnly)
    {
        return 0;
    }

    if (compileAndAssembleOnly || saveTemps)
    {
        if (gcc(["-x", "assembler", "-c", "-o", objFile, "-"] ~ ccFlags, code) != 0)
        {
            return 3;
        }
    }

    if (compileAndAssembleOnly)
    {
        return 0;
    }

    if (saveTemps)
    {
        if (gcc([objFile, "-o", outFile] ~ ccFlags) != 0)
        {
            return 3;
        }
    }
    else
    {
        if (gcc(["-x", "assembler", "-o", outFile, "-"] ~ ccFlags, code) != 0)
        {
            return 3;
        }
    }

    return 0;
}

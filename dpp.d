#!/usr/bin/env rdmd

module dpp;

import std;
import std.uni : isWhite;
import core.exception;

auto funcRegex = ctRegex!`^(?:private\s)?Result!\(?(.*?)\)?\s`;

void preprocess(Input, Output)(Input lineReader, Output textWriter)
{
    ulong i = 0;
    string currType = "T";
    foreach (line; lineReader)
    {
        auto isStmt = line.length > 0 && isWhite(line[0]);
        auto s = line.strip();
        if (isStmt && s.endsWith(`?;`))
        {
            char[] v, vMaybe;
            bool assignToV = true;
            string op = `=`;
            if (s.startsWith(`auto`))
            {
                auto vStart = s.countUntil!isWhite + 1;
                auto vEnd = vStart + s[vStart..$].countUntil!(x => x == '=' || isWhite(x));
                v = `auto ` ~ s[vStart..vEnd];
                vMaybe = s[vStart..vEnd] ~ `MaybeDpp` ~ to!string(++i);
            }
            else if (s.canFind(`=`))
            {
                auto vEnd = s.countUntil!isWhite;
                v = s[0..vEnd];
                vMaybe = v ~ `MaybeDpp` ~ to!string(++i);
                if (s.canFind(`~=`))
                    op = `~=`;
            }
            else
            {
                vMaybe = v ~ `MaybeDpp` ~ to!string(++i);
                assignToV = false;
            }
            auto eStart = s.countUntil('=') + 1;
            auto eEnd = s.countUntil('?');
            auto e = s[eStart..eEnd];
            textWriter.put(
                `auto ` ~ vMaybe ~ `=` ~ e ~ `;`
                ~ `if (` ~ vMaybe ~ `.isErr()) return ` ~ vMaybe ~ `.err.result!(` ~ currType ~ `);`
            );
            if (assignToV)
                textWriter.put(v ~ op ~ vMaybe ~ `.get;` ~ '\n');
            else
                textWriter.put('\n');
        }
        else if (isStmt && s.endsWith(`result;`))
        {
            auto end = `result;`;
            textWriter.put(s[0..($-end.length)] ~ `result!(` ~ currType ~ ");\n");
        }
        else
        {
            auto captures = matchFirst(s, funcRegex);
            if (captures.length == 2)
            {
                currType = captures[1].idup;
            }
            textWriter.put(line);
            continue;
        }
    }
}

immutable usage = "Usage: dpp infile [[-o] outfile]";

immutable help = usage ~ "

Options:
    -o <file>   Write output to <file>.
    -h, --help  This help information.

Fares A. Bakhit <faresa.bakhit@gmail.com>";

auto lineReader(string filename)
{
    if (filename == "-") return stdin.byLine(Yes.keepTerminator);
    return File(filename, "r").byLine(Yes.keepTerminator);
}

auto textWriter(string filename)
{
    if (filename == "-") return stdout.lockingTextWriter();
    return File(filename, "w").lockingTextWriter();
}

int main(string[] args)
{
    Nullable!string maybeOutFile;
    bool helpWanted;
    try
    {
        void outFileHandler(string option, string value)
        {
            maybeOutFile = value;
        }
        helpWanted = getopt(args, "o", &outFileHandler).helpWanted;
    }
    catch (GetOptException exc)
    {
        stderr.writefln("dpp: %s", exc.msg);
        stderr.writefln(usage);
        return 2;
    }
    if (helpWanted)
    {
        stdout.writefln(help);
        return 0;
    }
    string inFile = "-";
    if (args.length > 1)
    {
        inFile = args[1];
    }
    if (args.length > 2)
    {
        if (maybeOutFile.isNull && args.length < 4)
        {
            maybeOutFile = args[2];
        }
        else
        {
            stderr.writefln("dpp: Unexpected argument %s.", args[$-1]);
            stderr.writefln(usage);
            return 2;
        }
    }
    if (maybeOutFile.isNull)
    {
        maybeOutFile = "-";
    }
    string outFile = maybeOutFile.get;
    try
    {
        auto input = lineReader(inFile);
        auto output = textWriter(outFile);
        preprocess(input, output);
    }
    catch (ErrnoException exc)
    {
        stderr.writefln("dpp: %s.", exc.msg);
        return 1;
    }
    catch (StdioException exc)
    {
        stderr.writefln("dpp: %s.", exc.msg);
        return 1;
    }
    catch (UnicodeException exc)
    {
        stderr.writefln("dpp: %s.", exc.msg);
        return 1;
    }
    return 0;
}

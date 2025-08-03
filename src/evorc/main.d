module evorc.main;

import evorc;
import std.stdio;
import std.file;

int main(string[] args)
{
    string source;
    if (args.length > 1) {
        source = readText(args[1]);
    } else {
        source = readText("/dev/stdin");
    }
    return source
        .tokenize
        .parse
        .match!(
            (evorc.ast.Program prog)
            {
                return prog.tac.match!(
                    (evorc.tac.Program prog)
                    {
                        x86(prog, stdout.lockingTextWriter());
                        return 0;
                    },
                    (evorc.tac.Err err)
                    {
                        stderr.writeln(err.dbg);
                        return 1;
                    }
                );
            },
            (evorc.ast.Err err)
            {
                stderr.writeln(err.dbg);
                return 1;
            }
        );
}

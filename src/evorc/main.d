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
            (evorc.ast.Program prog) { prog.tac.dbg.writeln; return 0; },
            // (Program prog) => prog.lin.match!(
            //     (LinProgram prog)
            //     {
            //         x64(prog, stdout.lockingTextWriter());
            //         return 0;
            //     },
            //     (evorc.lin.Err err)
            //     {
            //         stderr.writeln(err.dbg);
            //         return 1;
            //     }
            // ),
            (evorc.ast.Err err)
            {
                stderr.writeln(err.dbg);
                return 1;
            }
        );
}

module evorc.main;

import evorc;
import std;

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
            (Program prog)
            {
                return prog
                    .lin
                    .match!(
                        (LinProgram prog)
                        {
                            x64(prog, stdout.lockingTextWriter());
                            return 0;
                        },
                        (evorc.lin.Err err)
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

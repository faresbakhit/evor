module app;

import evorc;
import std;

void main(string[] args)
{
    string source;
    if (args.length > 1) {
        source = readText(args[1]);
    } else {
        source = readText("/dev/stdin");
    }
    source
        .tokenize
        .parse
        .dbg
        .writeln;
}

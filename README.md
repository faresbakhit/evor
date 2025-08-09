# Evor

My attempt(s) at designing and creating a programming language and its compiler from scratch without using LLVM or any other [compiler compiler](https://www.gnu.org/software/bison/manual/html_node/Yacc.html)

## Trial 2 (Current)

*Aug 9, 2025*

## Trail 1

*Jul 17, 2025 to Aug 3, 2025 (17 days)*

Trial 1 is written in the [D programming language](https://dlang.org/) and includes a lexer, parser, three-address code transformer, and finally, an x86-64 assembly code generator that conforms to the SysV ABI.

```
// isqrt.evor

i32 main() {
    return isqrt(16);
}

i32 isqrt(i32 n) {
    i32 odd = 1;
    i32 count = 0;
    while n >= odd {
        n = n - odd;
        odd += 2;
        count += 1;
    }
    return count;
}
```

```sh
$ make && ./evorc hello.evor >hello.s && gcc -o hello hello.s && ./hello
```

- Control flow constructs: `if`, `while`, and function calls.
- Types: `void`, `bool`, `i32`, and pointers.

and while the language's feature set is very narrow, the sharp eye would notice that it doesn't, as a proper language, need [forward declarations](https://en.wikipedia.org/wiki/Forward_declaration).

## Resources

- https://compilers.cs.uni-saarland.de/papers/bbhlmz13cc.pdf
- https://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf
- https://www.clear.rice.edu/comp512/Lectures/Papers/1971-allen-catalog.pdf
- https://www.cole-k.com/2023/07/24/e-graphs-primer/

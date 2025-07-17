module evorc.dbg;

import evorc;

mixin Debug!(
    // evorc.tok
    "Bool", evorc.tok.Bool,
    "Int", evorc.tok.Int,
    "Ident", "Unknown", "Eof",
    // evorc.ast
    "FuncDecl", "Var", "Pointer", "Primitive", "Param", "Func", "If",
    "Return", "VarDecl", "Assign", "Un", "Bin", "Call",
    "Bool", evorc.ast.Bool,
    "Int", evorc.ast.Int,
);

import std;
import evorc.utils.result : Result;

private mixin template Debug(Specs...)
{
    private immutable INDENT_UNIT = "  ";
    private string indent(uint n) => INDENT_UNIT.repeat(n).join;

    static foreach (aliasSpec; parseSpecs!Specs)
    {
        string dbg(ref const aliasSpec.Type value) => dbg(value, 0);

        private string dbg(ref const aliasSpec.Type value, uint level)
        {
            alias aliasName = aliasSpec.name;
            alias T = aliasSpec.Type;

            static if (isTuple!T)
            {
                enum header = aliasName ~ "(",
                     mapper = ": ",
                     footer = ")",
                     separator = ",",
                     newline = "\n";
                static if (T.Types.length == 0)
                {
                     return header ~ footer;       
                }
                static if (T.Types.length == 1)
                {
                    string inner = dbg(value[0], level + 1);
                    if (!inner.canFind(newline))
                    {
                        static if (T.fieldNames[0] == "")
                            return header ~ inner;
                        else
                            return header
                                   ~ T.fieldNames[0]
                                   ~ mapper
                                   ~ inner
                                   ~ footer;
                    }
                }
                string indent = indent(level + 1);
                string r = header ~ newline ~ indent;
                static foreach (i, field; T.fieldNames)
                    static if (i == T.Types.length - 1)
                        static if (field == "")
                            r ~= dbg(value[i], level + 1)
                                 ~ newline
                                 ~ indent[INDENT_UNIT.length .. $]
                                 ~ footer;
                        else
                            r ~= field ~ mapper
                                 ~ dbg(value[i], level + 1)
                                 ~ newline
                                 ~ indent[INDENT_UNIT.length .. $]
                                 ~ footer;
                    else
                        static if (field == "")
                            r ~= dbg(value[i], level + 1)
                                 ~ separator
                                 ~ newline
                                 ~ indent;
                        else
                            r ~= field ~ mapper
                                 ~ dbg(value[i], level + 1)
                                 ~ separator
                                 ~ newline
                                 ~ indent;
                return r;
            }
            else
            {
                return aliasName ~ "(" ~ dbgImpl(value, level) ~ ")";
            }
        }
    }

    string dbg(T)(auto ref const T value)
        => dbgImpl(value, 0);

    private string dbg(T)(auto ref const T value, uint level)
        => dbgImpl(value, level);

    private string dbgImpl(T)(auto ref const T value, uint level)
    {
        static if (__traits(isSame, TemplateOf!T, Nullable) || __traits(isSame, TemplateOf!T, NullableRef))
        {
            if (value.isNull) return "null";
            return value.get.dbg(level);
        }
        else static if (isSumType!T)
        {
            return value.match!((auto ref const v) => dbg(v, level));
        }
        else static if (__traits(isSame, TemplateOf!T, Result))
        {
            if (value.isErr) return value.err.dbg(level);
            return value.get.dbg(level);
        }
        else static if (is(T == struct) && !hasMember!(T, "toString"))
        {
            enum header = T.stringof ~ " {",
                 mapper = ": ",
                 footer = "}",
                 separator = ",",
                 newline = "\n";
            static if (Fields!T.length == 0)
            {
                 return header ~ footer;
            }
            static if (Fields!T.length == 1)
            {
                enum fieldName = FieldNameTuple!T[0];
                string inner = dbg(__traits(getMember, value, fieldName), level + 1);
                if (!inner.canFind(newline))
                {
                    return header ~ " "
                           ~ fieldName
                           ~ mapper
                           ~ inner ~ " "
                           ~ footer;
                }
            }
            string indent = indent(level + 1);
            string r = header ~ newline ~ indent;
            static foreach (i, field; FieldNameTuple!T)
                static if (i == Fields!T.length - 1)
                    r ~= field ~ mapper
                         ~ dbg(__traits(getMember, value, field), level + 1)
                         ~ newline
                         ~ indent[INDENT_UNIT.length .. $]
                         ~ footer;
                else
                    r ~= field ~ mapper
                         ~ dbg(__traits(getMember, value, field), level + 1)
                         ~ separator
                         ~ newline
                         ~ indent;
            return r;
        }
        // else static if (is(T == enum))
        // {
        //     return T.stringof ~ "." ~ format!"%s"(value);
        // }
        else static if (isInputRange!T && !isSomeString!T)
        {
            if (value.empty) return "[]";
            string indent = indent(level + 1);
            string sep = ",\n" ~ indent;
            return "[\n" ~ indent
                   ~ value.map!(v => dbg(v, level + 1)).join(sep)
                   ~ "\n" ~ indent[INDENT_UNIT.length .. $] ~ "]";
        }
        else static if (isPointer!T)
        {
            if (value is null) return "null";
            return dbg(*value, level);
            // return format!"[0x%x] "(value) ~ dbg(*value, level);
        }
        else
        {
            // From std.typecons.Tuple.toString:
            //     Among other things, using "only" causes string-fields to be inside quotes in the result
            return format!"%(%s%)"(only(value));
        }
    }
}

private template parseSpecs(Specs...)
{
    static if (Specs.length == 0)
    {
        alias parseSpecs = AliasSeq!();
    }
    else static if (Specs.length > 1 && !is(typeof(Specs[1]) : string))
    {
        alias parseSpecs =
            AliasSeq!(AliasSpec!(Specs[0 .. 2]),
                      parseSpecs!(Specs[2 .. $]));
    }
    else static if (is(typeof(Specs[0]) : string))
    {
        alias parseSpecs =
            AliasSeq!(AliasSpec!(Specs[0], mixin(Specs[0])),
                      parseSpecs!(Specs[1 .. $]));
    }
    else
    {
        static assert(0, "Attempted to instantiate Debug mixin with an "
                        ~"invalid argument: "~ Specs[0].stringof);
    }
}

private template AliasSpec(string s, T)
{
    alias name = s;
    alias Type = T;
}

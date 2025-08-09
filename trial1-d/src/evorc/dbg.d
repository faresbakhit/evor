module evorc.dbg;

import evorc;

mixin Debug!(
    evorc.tok,
    "Bool", "Int", "Str", "Char", "Ident", "Err",
    evorc.ast,
    "FuncDecl", "Pointer", "Primitive", "Param", "Func", "If", "Return",
    "VarDecl", "Assign", "Un", "Bin", "Call", "Ident", "Bool", "Int", "Err",
    evorc.tac,
    "Func", "Label", "Jmp", "Jcc", "Bin", "Un", "Assign", "Load", "Store",
    "Call", "Return", "Var", "I32", "Bool", "Struct", "Pointer", "Err",
);

import evorc.utils.result : Result;
import std.algorithm;
import std.format;
import std.meta;
import std.range;
import std.sumtype;
import std.traits;
import std.typecons;

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
    else static if (isModule!(Specs[0]))
    {
        static if (Specs.length == 1)
        {
            alias parseSpecs = AliasSeq!();
        }
        else static if (isModule!(Specs[1]))
        {
            alias parseSpecs = parseSpecs!(Specs[1..$]);
        }
        else static if (Specs.length > 2 && !is(typeof(Specs[1]) : string))
        {
            alias parseSpecs =
                AliasSeq!(AliasSpec!(Specs[1 .. 3]),
                          parseSpecs!(Specs[0], Specs[3 .. $]));
        }
        else static if (is(typeof(Specs[1]) : string))
        {
            alias parseSpecs = AliasSeq!(
                AliasSpec!(Specs[1], mixin("Specs[0]" ~ "." ~ Specs[1])),
                parseSpecs!(Specs[0], Specs[2..$])
            );
        }
        else
        {
            static assert(0, "Attempted to instantiate Debug mixin with an "
                            ~"invalid argument: "~ Specs[1].stringof);
        }
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

enum bool isModule(alias T) = T.stringof.startsWith("module ");

private template AliasSpec(string s, T)
{
    alias name = s;
    alias Type = T;
}

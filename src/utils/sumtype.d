module evorc.utils.sumtype;

import std.sumtype;
import std.typecons;
import std.traits;

/**
 * Checks whether the given `SumType` contains a value equal to `value`
 * of a given or deducted type.
 *
 * The types must match exactly, without implicit conversions.
 */
bool contains(T, SumType)(auto ref SumType sumType, auto ref T value)
if (isSumType!SumType)
{
    return sumType.has!T && sumType.get!T == value;

    // One can also write,
    //
    //     import std.sumtype : match;
    //     return sumType.match!((T actualValue) => value == actualValue, _ => false);
    // 
    // which makes type qualifiers insignificant but carrying them is annoying
    // so I avoid them altogether.
}

/// Basic usage
unittest
{
    import std.sumtype : SumType;

    SumType!(string, double) example1 = "hello";
    SumType!(string, double) example2 = 3.14;

    assert(example1.contains("hello"));
    assert(!example1.contains(2.17));
    assert(example2.contains(3.14));
    assert(!example1.contains("3.14"));
}

// With type qualifiers
unittest
{
    import std.sumtype : SumType;

    alias Example = SumType!(string, double);
    Example m = "mutable";
    const(Example) c = "const";
    immutable(Example) i = "immutable";

    assert(m.contains("mutable"));
    assert(!c.contains("const"));
    assert(c.contains!(const(string))("const"));
    assert(!i.contains("immutable"));
    assert(i.contains!(immutable(string))("immutable"));
}

/**
 * Extracts the first field from a `SumType` assuming all underlying types
 * share the same first field type and layout.
 */
auto ref firstField(SumType)(auto ref SumType sumType)
{
    static if (isPointer!SumType)
        return firstFieldImpl(*sumType);
    else
        return firstFieldImpl(sumType);
}

///
unittest
{
    import std.typecons : Tuple;
    import std.sumtype : SumType;

    alias A = SumType!(Tuple!(int, string), Tuple!(int, double));
    A a = Tuple!(int, string)(123, "abc");
    A b = Tuple!(int, double)(456, 7.89);

    assert(a.firstField == 123);
    assert(b.firstField == 456);
}

private auto ref firstFieldImpl(SumType)(auto ref SumType sumType)
if (isSumType!SumType && SumType.Types.length > 0 && allSameFirstType!SumType)
{
    alias First = FirstTypeOf!(SumType.Types[0]);
    return *cast(First*)(&sumType);
}

private alias allSameFirstType(SumType) = allSameType!(staticMap!(FirstTypeOf, SumType.Types));

private template FirstTypeOf(Tuple)
{
    import std.typecons : isTuple;
    static if (isTuple!Tuple)
        alias FirstTypeOf = Tuple.Types[0];
    else static if (__traits(compiles, Tuple.tupleof))
        alias FirstTypeOf = typeof(Tuple.tupleof[0]);
    else
        alias FirstTypeOf = Tuple;
}

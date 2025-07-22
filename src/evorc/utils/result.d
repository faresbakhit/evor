module evorc.utils.result;

import std.traits;

/**
 * Also called a "Failable" type but naming it `Result` is more
 * appropriate in an imperative language.
 */
struct Result(T, E)
{
    alias Ok = T;
    alias Err = E;

    private bool _isErr;

    private union
    {
        Ok _ok;
        Err _err;
    }

    this(Ok ok)
    {
        _isErr = false;
        _ok = ok;
    }

    this(Err err)
    {
        _isErr = true;
        _err = err;
    }

    @property bool isErr() const @safe pure nothrow
    {
        return _isErr;
    }

    @property ref inout(Ok) get() inout pure nothrow
    {
        enum message = "Called `get' on error Result!(" ~ T.stringof ~ ", " ~ E.stringof ~ ").";
        assert(!_isErr, message);
        return _ok;
    }

    @property ref inout(Err) err() inout pure nothrow
    {
        enum message = "Called `err' on non-error Result!(" ~ T.stringof ~ ", " ~ E.stringof ~ ").";
        assert(_isErr, message);
        return _err;
    }

    auto ref match(handlers...)()
    {
        import std.sumtype : canMatch;
        static assert (handlers.length == 2, "Must have exactly two handlers: one for " ~ T.stringof ~ " and the other for " ~ E.stringof ~ ".");
        static if (canMatch!(handlers[0], T))
        {
            static assert(canMatch!(handlers[1], E), "handlers[1] must match " ~ E.stringof);
            if (!_isErr)
                return handlers[0](_ok);
            return handlers[1](_err);
        }
        else static if (canMatch!(handlers[0], E))
        {
            static assert(canMatch!(handlers[1], T), "handlers[1] must match " ~ T.stringof);
            if (_isErr)
                return handler[0](_err);
            return handlers[1](_ok);
        }
        else
        {
            static assert(0, "handlers[0]" ~ " must match one of " ~ T.stringof ~ " or " ~ E.stringof ~ ".");
        }
    }
}


/**
 * Create a [Result] template with `E` as the error type.
 */
template ResultWith(E)
{
    alias ResultWith(T) = Result!(T, E);
}

/**
 * Wrap an ok value in a [Result].
 *
 * Aliasing would eliminate type deduction, write your own instead:
 * ---
 * alias Result = ResultWith!(Err);
 * auto result(T)(T ok) => Result!T(ok);
 * auto result(T)(Err err) => Result!T(err);
 * ---
 */
auto result(T, E)(T ok) => Result!(T, E)(ok);

/**
 * Wrap an error value in a [Result].
 */
auto result(T, E)(E err) => Result!(T, E)(err);

/**
  * Collect `Ok` values from a range of `Result`s into a single `Result`.
  *
  * Iteration proceeds left‑to‑right. On the first `Err`, return that error
  * immediately. If no error is seen, return `Ok` with an array of all
  * successful payloads.
  *
  * Params:
  *     r  = input range whose element type is `Result!(T, E)`
  *
  * Returns: `Result!(T[], E)`
  */
Result!(ForeachType!Range.Ok[], ForeachType!Range.Err) collect(Range)(Range r)
if (isIterable!(Range) && isInstanceOf!(Result, ForeachType!Range))
{
    import std.array : appender;
    alias R = ForeachType!Range;
    auto a = appender!(R.Ok[])();
    foreach (e; r)
    {
        if (e.isErr)
            return Result!(R.Ok[], R.Err)(e.err);
        a.put(e.get);
    }
    return Result!(R.Ok[], R.Err)(a.data);
}

///
unittest
{
    import std.algorithm;
    import std.conv;

    Result!(int, string) parse(string s)
    {
        try {
            return to!int(s).result!(int, string);
        } catch (ConvException) {
            return "error: bad value".result!(int, string);
        }
    }

    auto r1 = ["1", "2", "3"].map!parse.collect;
    assert(!r1.isErr);
    assert(r1.get == [1, 2, 3]);

    auto r2 = ["1", "a", "3"].map!parse.collect;
    assert(r2.isErr);
}

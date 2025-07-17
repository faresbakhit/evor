module evorc.utils.result;

import std.algorithm.mutation : move;
import std.traits : isCopyable;
import std.exception : enforce;

/**
 * Also called a "Failable" type but naming it `Result` is more
 * appropriate in an imperative language.
 */
struct Result(T, E)
{
    private bool _isErr;

    private union
    {
        T _ok;
        E _err;
    }

    this(T ok)
    {
        _isErr = false;
        _ok = ok;
    }

    this(E err)
    {
        _isErr = true;
        _err = err;
    }

    @property bool isErr() const @safe pure nothrow
    {
        return _isErr;
    }

    @property bool isOk() const @safe pure nothrow
    {
        return !_isErr;
    }

    @property ref inout(T) get() inout pure nothrow
    {
        enum message = "Called `get' on error Result!(" ~ T.stringof ~ ", " ~ E.stringof ~ ").";
        assert(!_isErr, message);
        return _ok;
    }

    @property ref inout(E) err() inout pure nothrow
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
 * Wrap an ok value in a [Result].
 *
 * Aliasing would eliminate type deduction, write your own instead:
 * ---
 * alias Result = ResultWith!(Err);
 * auto result(T)(T ok) => Result!T(ok);
 * auto result(T)(Err err) => Result!T(err);
 * ---
 */
auto result(T, E)(T ok) => Result!(T, E)(_ok);

/**
 * Wrap an error value in a [Result].
 */
auto result(T, E)(E err) => Result!(T, E)(_err);

/**
 * Create a [Result] template with `E` as the error type.
 */
template ResultWith(E)
{
    alias ResultWith(T) = Result!(T, E);
}

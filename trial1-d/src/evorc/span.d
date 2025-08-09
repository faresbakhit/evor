module evorc.span;

/**
 * We need a way to say: Hey! this $(I thing) is represented by that
 * $(B span) of the source code for error reporting, so that span is
 * attached to every IR of the source code and should be cheap.
 *
 * One way is a struct with two `size_t`s like (start,end) or (pos,len)
 * and while that is correct, this becomes a nightmare during debugging.
 *
 * Instead, we use D slices that are essentially a (ptr,len) but they
 * actually carry the information a span conveys.
 *
 * NOTE: A function that takes some source code string and returns
 *       some data structure that contains a Span must guarantee
 *       that the span is a slice of the source code string.
 */
alias Span = string;
// Yep that's it, no crazy struct with 20 methods.

// not used currently, but it's here if needed
immutable Span invalidSpan = (cast(char*)(null))[0..0];

public import evorc.utils.array : joinSpans = joinSlices;
// Eh, we kind of really need this

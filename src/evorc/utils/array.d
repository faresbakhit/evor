module evorc.utils.array;

/**
 * Given two slices slice1 and sliceN where `sliceN.front` doesn't
 * come before `slice1.front` in the array they are both part of, return
 * a slice that have the same head as `slice1` and same tail as `sliceN`.
 * In other words,
 * joinSlices`(`$(I a)`[`$(I p)..$(I q)`]`, $(I a)`[`$(I r)..$(I s)`])`
 * is the same as $(I a)`[`$(I p)..$(I s)`]` given that `p <= r`. Caller
 * must guarantee that slice1 and sliceN are slices of the same array and
 * that `slice1.ptr <= slice2.ptr`
 */
T[] joinSlices(T)(T[] slice1, T[] sliceN)
{
    assert(slice1.ptr <= sliceN.ptr,
        "sliceN must not start before slice1 in memory (same array assumed)");
    auto totalLength = (sliceN.ptr - slice1.ptr) + sliceN.length;
    return slice1.ptr[0 .. totalLength];
}

///
unittest
{
    import std.array;
    auto arr = [5, 9, 6, 3, 2, 1, 4, 8, 7];
    auto slice1 = arr[1..3]; // [9, 6]
    auto sliceN = arr[4..7]; // [2, 1, 4]
    auto slice1ToN = joinSlices(slice1, sliceN); // [9, 6, 3, 2, 1, 4]
    assert(slice1ToN == arr[1..7]);
    assert(sameHead(slice1ToN, slice1));
    assert(sameTail(slice1ToN, sliceN));
}

/// Overlapping
unittest
{
    import std.array;
    auto arr = [1, 8, 3, 5, 2, 9, 7, 6, 4];
    auto slice1 = arr[2..6]; // [3, 5, 2, 9]
    auto slice2 = arr[3..9]; // [   5, 2, 9, 7, 6, 4]
    auto sliceN = arr[4..8]; // [      2, 9, 7, 6]
    //                          [3, 5, 2, 9, 7, 6]
    auto slice1ToN = joinSlices(slice1, sliceN); 
    //                          [   5, 2, 9, 7, 6]
    auto slice2ToN = joinSlices(slice2, sliceN);
    assert(slice1ToN == arr[2..8]);
    assert(sameHead(slice1ToN, slice1));
    assert(sameTail(slice1ToN, sliceN));
    assert(slice2ToN == arr[3..8]);
    assert(sameHead(slice2ToN, slice2));
    assert(sameTail(slice2ToN, sliceN));
}

/// Checks if the value is an ASCII decimal digit:
/// U+0030 '0' ..= U+0039 '9'.
pub inline fn isAsciiDigit(value: anytype) bool {
    return switch (value) {
        '0'...'9' => true,
        else => false,
    };
}

/// Checks if the value is within the ASCII range.
pub inline fn isAscii(value: anytype) bool {
    return value <= 0x7F;
}

/// Returns `true` if this char has the `White_Space` property.
///
/// `White_Space` is specified in the [Unicode Character Database][ucd] [`PropList.txt`].
///
/// [ucd]: https://www.unicode.org/reports/tr44/
/// [`PropList.txt`]: https://www.unicode.org/Public/UCD/latest/ucd/PropList.txt
pub fn isWhitespace(c: u21) bool {
    return switch (c) {
        ' ', '\x09'...'\x0d' => true,
        else => c > '\x7f' and switch (c >> 8) {
            0 => whitespace_map[c & 0xff] & 1 != 0,
            22 => c == 0x1680,
            32 => whitespace_map[c & 0xff] & 2 != 0,
            48 => c == 0x3000,
            else => false,
        },
    };
}

const whitespace_map = [256]u8{
    2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 2, 2, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
};

/// Converts a `u32` to a char.
///
/// Note that all chars are valid `u32`s, and can be cast to one with
/// `@intCast`.
///
/// However, the reverse is not true: not all valid `u32`s are valid
/// chars. This will return `null` if the input is not a valid value
/// for a char.
pub inline fn charFromU32(i: u32) ?u21 {
    // This is an optimized version of the check
    // i >= 0x110000 || (i >= 0xD800 && i < 0xE000).
    //
    // The XOR with 0xD800 permutes the ranges such that 0xD800..0xE000 is
    // mapped to 0x0000..0x0800, while keeping all the high bits outside 0xFFFF the same.
    // In particular, numbers >= 0x110000 stay in this range.
    //
    // Subtracting 0x800 causes 0x0000..0x0800 to wrap, meaning that a single
    // unsigned comparison against 0x110000 - 0x800 will detect both the wrapped
    // surrogate range as well as the numbers originally larger than 0x110000.
    //
    return if ((i ^ 0xD800) -% 0x800 >= 0x110000 - 0x800)
        null
    else
        @intCast(i);
}

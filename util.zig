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
        else => c > '\x7f' and false, // FIXME
    };
}

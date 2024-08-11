//! Utilities for validating string and char literals and turning them into
//! values they represent.

const std = @import("std");
const mem = std.mem;
const fmt = std.fmt;
const math = std.math;
const assert = std.debug.assert;
const unicode = std.unicode;
const Utf8View = std.unicode.Utf8View;
const Utf8Iterator = std.unicode.Utf8Iterator;
const util = @import("util.zig");

/// Errors and warnings that can occur during string unescaping. They mostly
/// relate to malformed escape sequences, but there are a few that are about
/// other problems.
pub const EscapeError = error{
    /// Expected 1 char, but 0 were found.
    ZeroChars,
    /// Expected 1 char, but more than 1 were found.
    MoreThanOneChar,

    /// Escaped '\' character without continuation.
    LoneSlash,
    /// Invalid escape character (e.g. '\z').
    InvalidEscape,
    /// Raw '\r' encountered.
    BareCarriageReturn,
    /// Raw '\r' encountered in raw string.
    BareCarriageReturnInRawString,
    /// Unescaped character that was expected to be escaped (e.g. raw '\t').
    EscapeOnlyChar,

    /// Numeric character escape is too short (e.g. '\x1').
    TooShortHexEscape,
    /// Invalid character in numeric escape (e.g. '\xz')
    InvalidCharInHexEscape,
    /// Character code in numeric escape is non-ascii (e.g. '\xFF').
    OutOfRangeHexEscape,

    /// '\u' not followed by '{'.
    NoBraceInUnicodeEscape,
    /// Non-hexadecimal value in '\u{..}'.
    InvalidCharInUnicodeEscape,
    /// '\u{}'
    EmptyUnicodeEscape,
    /// No closing brace in '\u{..}', e.g. '\u{12'.
    UnclosedUnicodeEscape,
    /// '\u{_12}'
    LeadingUnderscoreUnicodeEscape,
    /// More than 6 characters in '\u{..}', e.g. '\u{10FFFF_FF}'
    OverlongUnicodeEscape,
    /// Invalid in-bound unicode character code, e.g. '\u{DFFF}'.
    LoneSurrogateUnicodeEscape,
    /// Out of bounds unicode character code, e.g. '\u{FFFFFF}'.
    OutOfRangeUnicodeEscape,

    /// Unicode escape code in byte literal.
    UnicodeEscapeInByte,
    /// Non-ascii character in byte literal, byte string literal, or raw byte string literal.
    NonAsciiCharInByte,

    // `\0` in a C string literal.
    NulInCStr,

    /// After a line ending with '\', the next line contains whitespace
    /// characters that are not skipped.
    UnskippedWhitespaceWarning,

    /// After a line ending with '\', multiple lines are skipped.
    MultipleSkippedLinesWarning,
};

pub fn isFatalError(err: EscapeError) bool {
    return switch (err) {
        error.UnskippedWhitespaceWarning, error.MultipleSkippedLinesWarning => false,
        else => true,
    };
}

/// Takes the contents of a unicode-only (non-mixed-utf8) literal (without
/// quotes) and produces a sequence of escaped characters or errors.
///
/// Values are returned by invoking `callback`. For `char` and `byte` modes,
/// the callback will be called exactly once.
pub fn unescapeUnicode(
    src: Utf8View,
    mode: Mode,
    context: anytype,
    comptime Error: type,
    comptime callback: fn (@TypeOf(context), usize, usize, EscapeError!u21) Error!void,
) Error!void {
    const wrapper = struct {
        fn str(ctx: @TypeOf(context), start: usize, end: usize, result: EscapeError!CharOrByte) Error!void {
            const res = if (result) |res| res.toChar() else |err| err;
            try callback(ctx, start, end, res);
        }
        fn rawCStr(ctx: @TypeOf(context), start: usize, end: usize, result: EscapeError!u21) Error!void {
            const res = if (result) |res| blk: {
                break :blk if (res == 0) error.NulInCStr else res;
            } else |err| err;
            try callback(ctx, start, end, res);
        }
    };
    switch (mode) {
        .char, .byte => {
            var iter = src.iterator();
            const res = unescapeCharOrByte(&iter, mode);
            try callback(context, 0, iter.i, res);
        },
        .str, .byte_str => try unescapeNonRawCommon(src, mode, context, Error, wrapper.str),
        .raw_str, .raw_byte_str => try checkRawCommon(src, mode, context, Error, callback),
        .raw_c_str => try checkRawCommon(src, mode, context, Error, wrapper.rawCStr),
        .c_str => unreachable,
    }
}

/// Used for mixed utf8 string literals, i.e. those that allow both unicode
/// chars and high bytes.
pub const MixedUnit = union(enum) {
    /// Used for ASCII chars (written directly or via `\x00`..`\x7f` escapes)
    /// and Unicode chars (written directly or via `\u` escapes).
    ///
    /// For example, if '¥' appears in a string it is represented here as
    /// `MixedUnit{ .char = '¥' }`, and it will be appended to the relevant
    /// byte string as the two-byte UTF-8 sequence `[0xc2, 0xa5]`
    char: u21,

    /// Used for high bytes (`\x80`..`\xff`).
    ///
    /// For example, if `\xa5` appears in a string it is represented here as
    /// `MixedUnit{ .high_byte = 0xa5 }`, and it will be appended to the
    /// relevant byte string as the single byte `0xa5`.
    high_byte: u8,

    const Self = @This();

    pub fn fromChar(c: u21) Self {
        return .{ .char = c };
    }

    pub fn fromByte(n: u8) Self {
        return if (util.isAscii(n)) .{ .char = n } else .{ .high_byte = n };
    }
};

/// Takes the contents of a mixed-utf8 literal (without quotes) and produces
/// a sequence of escaped characters or errors.
///
/// Values are returned by invoking `callback`.
pub fn unescapeMixed(
    src: Utf8View,
    mode: Mode,
    context: anytype,
    comptime Error: type,
    comptime callback: fn (@TypeOf(context), usize, usize, EscapeError!MixedUnit) Error!void,
) Error!void {
    const wrapper = struct {
        fn func(ctx: @TypeOf(context), start: usize, end: usize, result: EscapeError!CharOrByte) Error!void {
            const res = if (result) |res| switch (res) {
                .char => |c| if (c == 0)
                    error.NulInCStr
                else
                    MixedUnit.fromChar(c),
                .byte => |n| MixedUnit.fromByte(n),
            } else |err| err;
            try callback(ctx, start, end, res);
        }
    };
    switch (mode) {
        .c_str => try unescapeNonRawCommon(src, mode, context, Error, wrapper.func),
        .char, .byte, .str, .raw_str, .byte_str, .raw_byte_str, .raw_c_str => unreachable,
    }
}

/// Takes a contents of a char literal (without quotes), and returns an
/// unescaped char or an error.
pub fn unescapeChar(src: Utf8View) EscapeError!u21 {
    var iter = src.iterator();
    return unescapeCharOrByte(&iter, .char);
}

/// Takes a contents of a byte literal (without quotes), and returns an
/// unescaped byte or an error.
pub fn unescapeByte(src: Utf8View) EscapeError!u8 {
    var iter = src.iterator();
    return byteFromChar(try unescapeCharOrByte(&iter, .byte));
}

/// What kind of literal do we parse.
pub const Mode = enum {
    char,

    byte,

    str,
    raw_str,

    byte_str,
    raw_byte_str,

    c_str,
    raw_c_str,

    const Self = @This();

    pub fn inDoubleQuotes(self: Self) bool {
        return switch (self) {
            .str, .raw_str, .byte_str, .raw_byte_str, .c_str, .raw_c_str => true,
            .char, .byte => false,
        };
    }

    /// Are `\x80`..`\xff` allowed?
    fn allowHighBytes(self: Self) bool {
        return switch (self) {
            .char, .str => false,
            .byte, .byte_str, .c_str => true,
            .raw_str, .raw_byte_str, .raw_c_str => unreachable,
        };
    }

    /// Are unicode (non-ASCII) chars allowed?
    inline fn allowUnicodeChars(self: Self) bool {
        return switch (self) {
            .byte, .byte_str, .raw_byte_str => false,
            .char, .str, .raw_str, .c_str, .raw_c_str => true,
        };
    }

    /// Are unicode escapes (`\u`) allowed?
    fn allowUnicodeEscapes(self: Self) bool {
        return switch (self) {
            .byte, .byte_str => false,
            .char, .str, .c_str => true,
            .raw_str, .raw_byte_str, .raw_c_str => unreachable,
        };
    }

    pub fn prefixNoRaw(self: Self) []const u8 {
        return switch (self) {
            .char, .str, .raw_str => "",
            .byte, .byte_str, .raw_byte_str => "b",
            .c_str, .raw_c_str => "c",
        };
    }
};

const CharOrByte = union(enum) {
    char: u21,
    byte: u8,

    const Self = @This();

    fn toChar(self: Self) u21 {
        return switch (self) {
            .char => |c| c,
            .byte => |b| b,
        };
    }
};

fn scanEscape(iter: *Utf8Iterator, mode: Mode) EscapeError!CharOrByte {
    // Previous character was '\\', unescape what follows.
    const res: u21 = switch (iter.nextCodepoint() orelse return error.LoneSlash) {
        '"' => '"',
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        '\\' => '\\',
        '\'' => '\'',
        '0' => 0,
        'x' => {
            // Parse hexadecimal character code.

            const hi = blk: {
                const s = iter.nextCodepointSlice() orelse
                    return error.TooShortHexEscape;

                break :blk fmt.parseInt(u32, s, 16) catch
                    return error.InvalidCharInHexEscape;
            };

            const lo = blk: {
                const s = iter.nextCodepointSlice() orelse
                    return error.TooShortHexEscape;

                break :blk fmt.parseInt(u32, s, 16) catch
                    return error.InvalidCharInHexEscape;
            };

            const value = @as(u8, @intCast(hi * 16 + lo));

            return if (!mode.allowHighBytes() and !util.isAscii(value))
                error.OutOfRangeHexEscape
            else
                // This may be a high byte, but that will only happen if `T` is
                // `MixedUnit`, because of the `allow_high_bytes` check above.
                .{ .byte = value };
        },
        'u' => try scanUnicode(iter, mode.allowUnicodeEscapes()),
        else => return error.InvalidEscape,
    };
    return .{ .char = res };
}

fn scanUnicode(iter: *Utf8Iterator, allow_unicode_escapes: bool) EscapeError!u21 {
    // We've parsed '\u', now we have to parse '{..}'.

    if (iter.nextCodepoint() != '{')
        return error.NoBraceInUnicodeEscape;

    // First character must be a hexadecimal digit.
    var n_digits: usize = 1;
    var value: u32 = blk: {
        const s = iter.nextCodepointSlice() orelse
            return error.UnclosedUnicodeEscape;

        if (mem.eql(u8, s, "_"))
            return error.LeadingUnderscoreUnicodeEscape;

        if (mem.eql(u8, s, "}"))
            return error.EmptyUnicodeEscape;

        break :blk fmt.parseInt(u32, s, 16) catch
            return error.InvalidCharInUnicodeEscape;
    };

    // First character is valid, now parse the rest of the number
    // and closing brace.
    while (true) {
        const s = iter.nextCodepointSlice() orelse
            return error.UnclosedUnicodeEscape;

        if (mem.eql(u8, s, "_"))
            continue;

        if (mem.eql(u8, s, "}")) {
            if (n_digits > 6)
                return error.OverlongUnicodeEscape;

            // Incorrect syntax has higher priority for error reporting
            // than unallowed value for a literal.
            if (!allow_unicode_escapes)
                return error.UnicodeEscapeInByte;

            return util.charFromU32(value) orelse {
                return if (value > 0x10FFFF)
                    error.OutOfRangeUnicodeEscape
                else
                    error.LoneSurrogateUnicodeEscape;
            };
        }

        const digit: u32 = fmt.parseInt(u32, s, 16) catch
            return error.InvalidCharInUnicodeEscape;

        n_digits += 1;
        if (n_digits > 6) {
            // Stop updating value since we're sure that it's incorrect already.
            continue;
        }
        value = value * 16 + digit;
    }
}

inline fn asciiCheck(c: u21, allow_unicode_chars: bool) EscapeError!u21 {
    return if (allow_unicode_chars or util.isAscii(c)) c else error.NonAsciiCharInByte;
}

fn unescapeCharOrByte(iter: *Utf8Iterator, mode: Mode) EscapeError!u21 {
    const c = iter.nextCodepoint() orelse
        return error.ZeroChars;

    const res = switch (c) {
        '\\' => (try scanEscape(iter, mode)).toChar(),
        '\n', '\t', '\'' => return error.EscapeOnlyChar,
        '\r' => return error.BareCarriageReturn,
        else => try asciiCheck(c, mode.allowUnicodeChars()),
    };

    if (iter.nextCodepoint() != null)
        return error.MoreThanOneChar;

    return res;
}

/// Takes a contents of a string literal (without quotes) and produces a
/// sequence of escaped characters or errors.
fn unescapeNonRawCommon(
    src: Utf8View,
    mode: Mode,
    context: anytype,
    comptime Error: type,
    comptime callback: fn (@TypeOf(context), usize, usize, EscapeError!CharOrByte) Error!void,
) Error!void {
    const wrapper = struct {
        fn func(ctx: @TypeOf(context), start: usize, end: usize, err: EscapeError) Error!void {
            try callback(ctx, start, end, err);
        }
    };

    var iter = src.iterator();
    const allow_unicode_chars = mode.allowUnicodeChars(); // get this outside the loop

    // The `start` and `end` computation here is complicated because
    // `skipAsciiWhitespace` makes us to skip over chars without counting
    // them in the range computation.
    while (iter.nextCodepointSlice()) |s| {
        const start = src.bytes.len - iter.bytes[iter.i..].len - s.len;
        const c = unicode.utf8Decode(s) catch unreachable;
        const res = switch (c) {
            '\\' => blk: {
                var iter_clone = iter;
                if (iter_clone.nextCodepoint() == '\n') {
                    // Rust language specification requires us to skip whitespaces
                    // if unescaped '\' character is followed by '\n'.
                    // For details see [Rust language reference]
                    // (https://doc.rust-lang.org/reference/tokens.html#string-literals).
                    try skipAsciiWhitespace(&iter, start, context, Error, wrapper.func);
                    continue;
                } else {
                    break :blk scanEscape(&iter, mode);
                }
            },
            '"' => error.EscapeOnlyChar,
            '\r' => error.BareCarriageReturn,
            else => if (asciiCheck(c, allow_unicode_chars)) |char|
                CharOrByte{ .char = char }
            else |err|
                err,
        };
        const end = src.bytes.len - iter.bytes[iter.i..].len;
        try callback(context, start, end, res);
    }
}

fn skipAsciiWhitespace(
    iter: *Utf8Iterator,
    start: usize,
    context: anytype,
    comptime Error: type,
    comptime callback: fn (@TypeOf(context), usize, usize, EscapeError) Error!void,
) Error!void {
    const spaces = [_]u8{ ' ', '\t', '\n', '\r' };
    const tail_with_spaces = iter.bytes[iter.i..];
    const first_non_space = mem.indexOfNone(u8, tail_with_spaces, &spaces) orelse
        tail_with_spaces.len;
    if (mem.indexOfScalar(u8, tail_with_spaces[1..first_non_space], '\n') != null) {
        // The +1 accounts for the escaping slash.
        const end = start + first_non_space + 1;
        try callback(context, start, end, error.MultipleSkippedLinesWarning);
    }
    const tail_without_spaces = Utf8View.initUnchecked(tail_with_spaces[first_non_space..]);
    var temp_iter = tail_without_spaces.iterator();
    if (temp_iter.nextCodepointSlice()) |s| {
        const c = unicode.utf8Decode(s) catch unreachable;
        if (util.isWhitespace(c)) {
            // For error reporting, we would like the span to contain the character that was not
            // skipped. The +1 is necessary to account for the leading \ that started the escape.
            const end = start + first_non_space + s.len + 1;
            try callback(context, start, end, error.UnskippedWhitespaceWarning);
        }
    }
    iter.* = tail_without_spaces.iterator();
}

/// Takes a contents of a string literal (without quotes) and produces a
/// sequence of characters or errors.
/// NOTE: Raw strings do not perform any explicit character escaping, here we
/// only produce errors on bare CR.
fn checkRawCommon(
    src: Utf8View,
    mode: Mode,
    context: anytype,
    comptime Error: type,
    comptime callback: fn (@TypeOf(context), usize, usize, EscapeError!u21) Error!void,
) Error!void {
    var iter = src.iterator();
    const allow_unicode_chars = mode.allowUnicodeChars(); // get this outside the loop

    // The `start` and `end` computation here matches the one in
    // `unescapeNonRawCommon` for consistency, even though this function
    // doesn't have to worry about skipping any chars.
    while (iter.nextCodepointSlice()) |s| {
        const c = unicode.utf8Decode(s) catch unreachable;
        const start = src.bytes.len - iter.bytes[iter.i..].len - s.len;
        const res = switch (c) {
            '\r' => error.BareCarriageReturnInRawString,
            else => asciiCheck(c, allow_unicode_chars),
        };
        const end = src.bytes.len - iter.bytes[iter.i..].len;
        try callback(context, start, end, res);
    }
}

pub inline fn byteFromChar(c: u21) u8 {
    assert(c <= math.maxInt(u8)); // guaranteed because of ByteStr
    return @intCast(c);
}

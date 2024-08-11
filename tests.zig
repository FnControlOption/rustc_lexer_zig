const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const testing = std.testing;
const allocator = testing.allocator;
const Utf8View = std.unicode.Utf8View;
const rustc_lexer = @import("rustc_lexer");
const RawStrError = rustc_lexer.RawStrError;
const Token = rustc_lexer.Token;
const tokenize = rustc_lexer.tokenize;
const stripShebang = rustc_lexer.stripShebang;
const unescape = rustc_lexer.unescape;
const unescapeChar = unescape.unescapeChar;
const unescapeByte = unescape.unescapeByte;
const unescapeUnicode = unescape.unescapeUnicode;
const EscapeError = unescape.EscapeError;
const byteFromChar = unescape.byteFromChar;

fn checkRawStr(s: []const u8, expected: RawStrError!u8) !void {
    const raw_str = try std.fmt.allocPrint(allocator, "r{s}", .{s});
    defer allocator.free(raw_str);
    var tokenizer = tokenize(try Utf8View.init(raw_str));
    _ = tokenizer.cursor.bump();
    const res = tokenizer.rawDoubleQuotedString(0);
    try testing.expectEqual(expected, res);
}

test "naked raw str" {
    try checkRawStr(
        \\"abc"
    , 0);
}

test "raw no start" {
    try checkRawStr(
        \\"abc"#
    , 0);
}

test "too many terminators" {
    // this error is handled in the parser later
    try checkRawStr(
        \\#"abc"##
    , 1);
}

test "unterminated" {
    try checkRawStr(
        \\#"abc"
    , error.NoTerminator);

    try checkRawStr(
        \\##"abc"#
    , error.NoTerminator);

    // We're looking for "# not just any #
    try checkRawStr(
        \\##"abc#
    , error.NoTerminator);
}

test "invalid start" {
    try checkRawStr(
        \\#~"abc"#
    , error.InvalidStarter);
}

test "unterminated no pound" {
    // https://github.com/rust-lang/rust/issues/70677
    try checkRawStr(
        \\"
    , error.NoTerminator);
}

test "too many hashes" {
    const max_count = std.math.maxInt(u8);
    const hashes1 = "#" ** max_count;
    const hashes2 = "#" ** (max_count + 1);
    const middle = "\"abc\"";
    const s1 = try std.mem.join(allocator, "", &.{ hashes1, middle, hashes1 });
    defer allocator.free(s1);
    const s2 = try std.mem.join(allocator, "", &.{ hashes2, middle, hashes2 });
    defer allocator.free(s2);

    // Valid number of hashes (255 = 2^8 - 1 = u8::MAX).
    try checkRawStr(s1, 255);

    // One more hash sign (256 = 2^8) becomes too many.
    try checkRawStr(s2, error.TooManyDelimiters);
}

test "valid shebang" {
    // https://github.com/rust-lang/rust/issues/70528
    const input = "#!/usr/bin/rustrun\nlet x = 5;";
    try testing.expectEqual(18, stripShebang(try Utf8View.init(input)));
}

test "invalid shebang valid rust syntax" {
    // https://github.com/rust-lang/rust/issues/70528
    const input = "#!    [bad_attribute]";
    try testing.expectEqual(null, stripShebang(try Utf8View.init(input)));
}

test "shebang second line" {
    // Because shebangs are interpreted by the kernel, they must be on the first line
    const input = "\n#!/bin/bash";
    try testing.expectEqual(null, stripShebang(try Utf8View.init(input)));
}

test "shebang space" {
    const input = "#!    /bin/bash";
    try testing.expectEqual(input.len, stripShebang(try Utf8View.init(input)));
}

test "shebang empty shebang" {
    const input = "#!    \n[attribute(foo)]";
    try testing.expectEqual(null, stripShebang(try Utf8View.init(input)));
}

test "invalid shebang comment" {
    const input = "#!//bin/ami/a/comment\n[";
    try testing.expectEqual(null, stripShebang(try Utf8View.init(input)));
}

test "invalid shebang another comment" {
    const input = "#!/*bin/ami/a/comment*/\n[attribute";
    try testing.expectEqual(null, stripShebang(try Utf8View.init(input)));
}

test "shebang valid rust after" {
    const input = "#!/*bin/ami/a/comment*/\npub fn main() {}";
    try testing.expectEqual(23, stripShebang(try Utf8View.init(input)));
}

test "shebang followed by attrib" {
    const input = "#!/bin/rust-scripts\n#![allow_unused(true)]";
    try testing.expectEqual(19, stripShebang(try Utf8View.init(input)));
}

fn checkLexing(src: []const u8, expected: []const Token) !void {
    var tokenizer = tokenize(try Utf8View.init(src));
    var actual = ArrayList(Token).init(allocator);
    defer actual.deinit();
    while (tokenizer.next()) |token|
        try actual.append(token);
    try testing.expectEqualDeep(expected, actual.items);
}

test "smoke test" {
    try checkLexing(
        "/* my source file */ fn main() { println!(\"zebra\"); }\n",
        &.{
            .{ .kind = .{ .block_comment = .{ .doc_style = null, .terminated = true } }, .len = 20 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .ident, .len = 2 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .ident, .len = 4 },
            .{ .kind = .open_paren, .len = 1 },
            .{ .kind = .close_paren, .len = 1 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .open_brace, .len = 1 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .ident, .len = 7 },
            .{ .kind = .bang, .len = 1 },
            .{ .kind = .open_paren, .len = 1 },
            .{ .kind = .{ .literal = .{ .kind = .{ .str = .{ .terminated = true } }, .suffix_start = 7 } }, .len = 7 },
            .{ .kind = .close_paren, .len = 1 },
            .{ .kind = .semi, .len = 1 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .close_brace, .len = 1 },
            .{ .kind = .whitespace, .len = 1 },
        },
    );
}

test "comment flavors" {
    try checkLexing(
        \\// line
        \\//// line as well
        \\/// outer doc line
        \\//! inner doc line
        \\/* block */
        \\/**/
        \\/*** also block */
        \\/** outer doc block */
        \\/*! inner doc block */
    ,
        &.{
            .{ .kind = .{ .line_comment = .{ .doc_style = null } }, .len = 7 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .{ .line_comment = .{ .doc_style = null } }, .len = 17 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .{ .line_comment = .{ .doc_style = .outer } }, .len = 18 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .{ .line_comment = .{ .doc_style = .inner } }, .len = 18 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .{ .block_comment = .{ .doc_style = null, .terminated = true } }, .len = 11 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .{ .block_comment = .{ .doc_style = null, .terminated = true } }, .len = 4 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .{ .block_comment = .{ .doc_style = null, .terminated = true } }, .len = 18 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .{ .block_comment = .{ .doc_style = .outer, .terminated = true } }, .len = 22 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .{ .block_comment = .{ .doc_style = .inner, .terminated = true } }, .len = 22 },
        },
    );
}

test "nested block comments" {
    try checkLexing(
        "/* /* */ */'a'",
        &.{
            .{ .kind = .{ .block_comment = .{ .doc_style = null, .terminated = true } }, .len = 11 },
            .{ .kind = .{ .literal = .{ .kind = .{ .char = .{ .terminated = true } }, .suffix_start = 3 } }, .len = 3 },
        },
    );
}

test "characters" {
    try checkLexing(
        "'a' ' ' '\\n'",
        &.{
            .{ .kind = .{ .literal = .{ .kind = .{ .char = .{ .terminated = true } }, .suffix_start = 3 } }, .len = 3 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .{ .literal = .{ .kind = .{ .char = .{ .terminated = true } }, .suffix_start = 3 } }, .len = 3 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .{ .literal = .{ .kind = .{ .char = .{ .terminated = true } }, .suffix_start = 4 } }, .len = 4 },
        },
    );
}

test "lifetime" {
    try checkLexing(
        "'abc",
        &.{
            .{ .kind = .{ .lifetime = .{ .starts_with_number = false } }, .len = 4 },
        },
    );
}

test "raw string" {
    try checkLexing(
        "r###\"\"#a\\b\x00c\"\"###",
        &.{
            .{ .kind = .{ .literal = .{ .kind = .{ .raw_str = .{ .n_hashes = 3 } }, .suffix_start = 17 } }, .len = 17 },
        },
    );
}

test "literal suffixes" {
    try checkLexing(
        \\'a'
        \\b'a'
        \\"a"
        \\b"a"
        \\1234
        \\0b101
        \\0xABC
        \\1.0
        \\1.0e10
        \\2us
        \\r###"raw"###suffix
        \\br###"raw"###suffix
    ,
        &.{
            .{ .kind = .{ .literal = .{ .kind = .{ .char = .{ .terminated = true } }, .suffix_start = 3 } }, .len = 3 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .{ .literal = .{ .kind = .{ .byte = .{ .terminated = true } }, .suffix_start = 4 } }, .len = 4 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .{ .literal = .{ .kind = .{ .str = .{ .terminated = true } }, .suffix_start = 3 } }, .len = 3 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .{ .literal = .{ .kind = .{ .byte_str = .{ .terminated = true } }, .suffix_start = 4 } }, .len = 4 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .{ .literal = .{ .kind = .{ .int = .{ .base = .decimal, .empty_int = false } }, .suffix_start = 4 } }, .len = 4 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .{ .literal = .{ .kind = .{ .int = .{ .base = .binary, .empty_int = false } }, .suffix_start = 5 } }, .len = 5 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .{ .literal = .{ .kind = .{ .int = .{ .base = .hexadecimal, .empty_int = false } }, .suffix_start = 5 } }, .len = 5 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .{ .literal = .{ .kind = .{ .float = .{ .base = .decimal, .empty_exponent = false } }, .suffix_start = 3 } }, .len = 3 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .{ .literal = .{ .kind = .{ .float = .{ .base = .decimal, .empty_exponent = false } }, .suffix_start = 6 } }, .len = 6 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .{ .literal = .{ .kind = .{ .int = .{ .base = .decimal, .empty_int = false } }, .suffix_start = 1 } }, .len = 3 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .{ .literal = .{ .kind = .{ .raw_str = .{ .n_hashes = 3 } }, .suffix_start = 12 } }, .len = 18 },
            .{ .kind = .whitespace, .len = 1 },
            .{ .kind = .{ .literal = .{ .kind = .{ .raw_byte_str = .{ .n_hashes = 3 } }, .suffix_start = 13 } }, .len = 19 },
        },
    );
}

fn CheckUnescaped(comptime mode: unescape.Mode) type {
    return struct {
        const Self = @This();
        const Error = Allocator.Error;
        const Entry = struct { usize, usize, EscapeError!u21 };

        unescaped: ArrayList(Entry),

        fn deinit(self: *Self) void {
            self.unescaped.deinit();
            self.* = undefined;
        }

        fn callback(self: *Self, start: usize, end: usize, res: EscapeError!u21) Error!void {
            try self.unescaped.append(.{ start, end, res });
        }

        fn run(literal: []const u8, expected: []const Entry) !void {
            const literal_text = try Utf8View.init(literal);

            var check = Self{ .unescaped = try ArrayList(Entry).initCapacity(allocator, literal.len) };
            defer check.deinit();

            try unescapeUnicode(literal_text, mode, &check, Error, callback);
            try testing.expectEqualSlices(Entry, expected, check.unescaped.items);
        }
    };
}

test "unescape char bad" {
    const Check = struct {
        fn run(literal: []const u8, expected_error: EscapeError) !void {
            const literal_text = try Utf8View.init(literal);
            try testing.expectEqual(expected_error, unescapeChar(literal_text));
        }
    };

    try Check.run("", error.ZeroChars);
    try Check.run("\\", error.LoneSlash);

    try Check.run("\n", error.EscapeOnlyChar);
    try Check.run("\t", error.EscapeOnlyChar);
    try Check.run("'", error.EscapeOnlyChar);
    try Check.run("\r", error.BareCarriageReturn);

    try Check.run("spam", error.MoreThanOneChar);
    try Check.run("\\x0ff", error.MoreThanOneChar);
    try Check.run("\\\"a\"", error.MoreThanOneChar);
    try Check.run("\\na", error.MoreThanOneChar);
    try Check.run("\\ra", error.MoreThanOneChar);
    try Check.run("\\ta", error.MoreThanOneChar);
    try Check.run("\\\\a", error.MoreThanOneChar);
    try Check.run("\\'a", error.MoreThanOneChar);
    try Check.run("\\0a", error.MoreThanOneChar);
    try Check.run("\\u{0}x", error.MoreThanOneChar);
    try Check.run("\\u{1F63b}}", error.MoreThanOneChar);

    try Check.run("\\v", error.InvalidEscape);
    try Check.run("\\💩", error.InvalidEscape);
    try Check.run("\\●", error.InvalidEscape);
    try Check.run("\\\r", error.InvalidEscape);

    try Check.run("\\x", error.TooShortHexEscape);
    try Check.run("\\x0", error.TooShortHexEscape);
    try Check.run("\\xf", error.TooShortHexEscape);
    try Check.run("\\xa", error.TooShortHexEscape);
    try Check.run("\\xx", error.InvalidCharInHexEscape);
    try Check.run("\\xы", error.InvalidCharInHexEscape);
    try Check.run("\\x🦀", error.InvalidCharInHexEscape);
    try Check.run("\\xtt", error.InvalidCharInHexEscape);
    try Check.run("\\xff", error.OutOfRangeHexEscape);
    try Check.run("\\xFF", error.OutOfRangeHexEscape);
    try Check.run("\\x80", error.OutOfRangeHexEscape);

    try Check.run("\\u", error.NoBraceInUnicodeEscape);
    try Check.run("\\u[0123]", error.NoBraceInUnicodeEscape);
    try Check.run("\\u{0x}", error.InvalidCharInUnicodeEscape);
    try Check.run("\\u{", error.UnclosedUnicodeEscape);
    try Check.run("\\u{0000", error.UnclosedUnicodeEscape);
    try Check.run("\\u{}", error.EmptyUnicodeEscape);
    try Check.run("\\u{_0000}", error.LeadingUnderscoreUnicodeEscape);
    try Check.run("\\u{0000000}", error.OverlongUnicodeEscape);
    try Check.run("\\u{FFFFFF}", error.OutOfRangeUnicodeEscape);
    try Check.run("\\u{ffffff}", error.OutOfRangeUnicodeEscape);
    try Check.run("\\u{ffffff}", error.OutOfRangeUnicodeEscape);

    try Check.run("\\u{DC00}", error.LoneSurrogateUnicodeEscape);
    try Check.run("\\u{DDDD}", error.LoneSurrogateUnicodeEscape);
    try Check.run("\\u{DFFF}", error.LoneSurrogateUnicodeEscape);

    try Check.run("\\u{D800}", error.LoneSurrogateUnicodeEscape);
    try Check.run("\\u{DAAA}", error.LoneSurrogateUnicodeEscape);
    try Check.run("\\u{DBFF}", error.LoneSurrogateUnicodeEscape);
}

test "unescape char good" {
    const Check = struct {
        fn run(literal: []const u8, expected_char: u21) !void {
            const literal_text = try Utf8View.init(literal);
            try testing.expectEqual(expected_char, unescapeChar(literal_text));
        }
    };

    try Check.run("a", 'a');
    try Check.run("ы", 'ы');
    try Check.run("🦀", '🦀');

    try Check.run("\\\"", '"');
    try Check.run("\\n", '\n');
    try Check.run("\\r", '\r');
    try Check.run("\\t", '\t');
    try Check.run("\\\\", '\\');
    try Check.run("\\'", '\'');
    try Check.run("\\0", 0);

    try Check.run("\\x00", 0);
    try Check.run("\\x5a", 'Z');
    try Check.run("\\x5A", 'Z');
    try Check.run("\\x7f", 127);

    try Check.run("\\u{0}", 0);
    try Check.run("\\u{000000}", 0);
    try Check.run("\\u{41}", 'A');
    try Check.run("\\u{0041}", 'A');
    try Check.run("\\u{00_41}", 'A');
    try Check.run("\\u{4__1__}", 'A');
    try Check.run("\\u{1F63b}", '😻');
}

test "unescape str warn" {
    const Check = CheckUnescaped(.str);

    // Check we can handle escaped newlines at the end of a file.
    try Check.run("\\\n", &.{});
    try Check.run("\\\n ", &.{});

    try Check.run(
        "\\\n \u{a0} x",
        &.{
            .{ 0, 5, error.UnskippedWhitespaceWarning },
            .{ 3, 5, '\u{a0}' },
            .{ 5, 6, ' ' },
            .{ 6, 7, 'x' },
        },
    );
    try Check.run("\\\n  \n  x", &.{ .{ 0, 7, error.MultipleSkippedLinesWarning }, .{ 7, 8, 'x' } });
}

test "unescape str good" {
    const Check = struct {
        const Self = @This();
        const Error = Allocator.Error || error{ Utf8CannotEncodeSurrogateHalf, CodepointTooLarge };
        const Result = union(enum) {
            ok: ArrayList(u8),
            err: struct { usize, usize, EscapeError },
        };

        buf: Result,

        fn deinit(self: *Self) void {
            switch (self.buf) {
                .ok => |*b| b.deinit(),
                .err => {},
            }
            self.* = undefined;
        }

        fn callback(self: *Self, start: usize, end: usize, res: EscapeError!u21) Error!void {
            switch (self.buf) {
                .ok => |*b| {
                    if (res) |c| {
                        var out: [4]u8 = undefined;
                        const len = try std.unicode.utf8Encode(c, &out);
                        try b.appendSlice(out[0..len]);
                    } else |e| {
                        self.buf = .{ .err = .{ start, end, e } };
                    }
                },
                .err => {},
            }
        }

        fn run(literal: []const u8, expected: []const u8) !void {
            const literal_text = try Utf8View.init(literal);
            _ = try Utf8View.init(expected);

            var check = Self{ .buf = .{ .ok = try ArrayList(u8).initCapacity(allocator, literal.len) } };
            defer check.deinit();

            try unescapeUnicode(literal_text, .str, &check, Error, callback);
            try testing.expect(check.buf == .ok);
            try testing.expectEqualStrings(expected, check.buf.ok.items);
        }
    };

    try Check.run("foo", "foo");
    try Check.run("", "");
    try Check.run(" \t\n", " \t\n");

    try Check.run("hello \\\n     world", "hello world");
    try Check.run("thread's", "thread's");
}

test "unescape byte bad" {
    const Check = struct {
        fn run(literal: []const u8, expected_error: EscapeError) !void {
            const literal_text = try Utf8View.init(literal);
            try testing.expectEqual(expected_error, unescapeByte(literal_text));
        }
    };

    try Check.run("", error.ZeroChars);
    try Check.run("\\", error.LoneSlash);

    try Check.run("\n", error.EscapeOnlyChar);
    try Check.run("\t", error.EscapeOnlyChar);
    try Check.run("'", error.EscapeOnlyChar);
    try Check.run("\r", error.BareCarriageReturn);

    try Check.run("spam", error.MoreThanOneChar);
    try Check.run("\\x0ff", error.MoreThanOneChar);
    try Check.run("\\\"a", error.MoreThanOneChar);
    try Check.run("\\na", error.MoreThanOneChar);
    try Check.run("\\ra", error.MoreThanOneChar);
    try Check.run("\\ta", error.MoreThanOneChar);
    try Check.run("\\\\a", error.MoreThanOneChar);
    try Check.run("\\'a", error.MoreThanOneChar);
    try Check.run("\\0a", error.MoreThanOneChar);

    try Check.run("\\v", error.InvalidEscape);
    try Check.run("\\💩", error.InvalidEscape);
    try Check.run("\\●", error.InvalidEscape);

    try Check.run("\\x", error.TooShortHexEscape);
    try Check.run("\\x0", error.TooShortHexEscape);
    try Check.run("\\xa", error.TooShortHexEscape);
    try Check.run("\\xf", error.TooShortHexEscape);
    try Check.run("\\xx", error.InvalidCharInHexEscape);
    try Check.run("\\xы", error.InvalidCharInHexEscape);
    try Check.run("\\x🦀", error.InvalidCharInHexEscape);
    try Check.run("\\xtt", error.InvalidCharInHexEscape);

    try Check.run("\\u", error.NoBraceInUnicodeEscape);
    try Check.run("\\u[0123]", error.NoBraceInUnicodeEscape);
    try Check.run("\\u{0x}", error.InvalidCharInUnicodeEscape);
    try Check.run("\\u{", error.UnclosedUnicodeEscape);
    try Check.run("\\u{0000", error.UnclosedUnicodeEscape);
    try Check.run("\\u{}", error.EmptyUnicodeEscape);
    try Check.run("\\u{_0000}", error.LeadingUnderscoreUnicodeEscape);
    try Check.run("\\u{0000000}", error.OverlongUnicodeEscape);

    try Check.run("ы", error.NonAsciiCharInByte);
    try Check.run("🦀", error.NonAsciiCharInByte);

    try Check.run("\\u{0}", error.UnicodeEscapeInByte);
    try Check.run("\\u{000000}", error.UnicodeEscapeInByte);
    try Check.run("\\u{41}", error.UnicodeEscapeInByte);
    try Check.run("\\u{0041}", error.UnicodeEscapeInByte);
    try Check.run("\\u{00_41}", error.UnicodeEscapeInByte);
    try Check.run("\\u{4__1__}", error.UnicodeEscapeInByte);
    try Check.run("\\u{1F63b}", error.UnicodeEscapeInByte);
    try Check.run("\\u{0}x", error.UnicodeEscapeInByte);
    try Check.run("\\u{1F63b}}", error.UnicodeEscapeInByte);
    try Check.run("\\u{FFFFFF}", error.UnicodeEscapeInByte);
    try Check.run("\\u{ffffff}", error.UnicodeEscapeInByte);
    try Check.run("\\u{ffffff}", error.UnicodeEscapeInByte);
    try Check.run("\\u{DC00}", error.UnicodeEscapeInByte);
    try Check.run("\\u{DDDD}", error.UnicodeEscapeInByte);
    try Check.run("\\u{DFFF}", error.UnicodeEscapeInByte);
    try Check.run("\\u{D800}", error.UnicodeEscapeInByte);
    try Check.run("\\u{DAAA}", error.UnicodeEscapeInByte);
    try Check.run("\\u{DBFF}", error.UnicodeEscapeInByte);
}

test "unescape byte good" {
    const Check = struct {
        fn run(literal: []const u8, expected_byte: u8) !void {
            const literal_text = try Utf8View.init(literal);
            try testing.expectEqual(expected_byte, unescapeByte(literal_text));
        }
    };

    try Check.run("a", 'a');

    try Check.run("\\\"", '"');
    try Check.run("\\n", '\n');
    try Check.run("\\r", '\r');
    try Check.run("\\t", '\t');
    try Check.run("\\\\", '\\');
    try Check.run("\\'", '\'');
    try Check.run("\\0", 0);

    try Check.run("\\x00", 0);
    try Check.run("\\x5a", 'Z');
    try Check.run("\\x5A", 'Z');
    try Check.run("\\x7f", 127);
    try Check.run("\\x80", 128);
    try Check.run("\\xff", 255);
    try Check.run("\\xFF", 255);
}

test "unescape byte str good" {
    const Check = struct {
        const Self = @This();
        const Error = Allocator.Error;
        const Result = union(enum) {
            ok: ArrayList(u8),
            err: struct { usize, usize, EscapeError },
        };

        buf: Result,

        fn deinit(self: *Self) void {
            switch (self.buf) {
                .ok => |*b| b.deinit(),
                .err => {},
            }
            self.* = undefined;
        }

        fn callback(self: *Self, start: usize, end: usize, res: EscapeError!u21) Error!void {
            switch (self.buf) {
                .ok => |*b| {
                    if (res) |c| {
                        try b.append(byteFromChar(c));
                    } else |e| {
                        self.buf = .{ .err = .{ start, end, e } };
                    }
                },
                .err => {},
            }
        }

        fn run(literal: []const u8, expected: []const u8) !void {
            const literal_text = try Utf8View.init(literal);

            var check = Self{ .buf = .{ .ok = try ArrayList(u8).initCapacity(allocator, literal.len) } };
            defer check.deinit();

            try unescapeUnicode(literal_text, .byte_str, &check, Error, callback);
            try testing.expect(check.buf == .ok);
            try testing.expectEqualStrings(expected, check.buf.ok.items);
        }
    };

    try Check.run("foo", "foo");
    try Check.run("", "");
    try Check.run(" \t\n", " \t\n");

    try Check.run("hello \\\n     world", "hello world");
    try Check.run("thread's", "thread's");
}

test "unescape raw str" {
    const Check = CheckUnescaped(.raw_str);
    try Check.run("\r", &.{.{ 0, 1, error.BareCarriageReturnInRawString }});
    try Check.run("\rx", &.{ .{ 0, 1, error.BareCarriageReturnInRawString }, .{ 1, 2, 'x' } });
}

test "unescape raw byte str" {
    const Check = CheckUnescaped(.raw_byte_str);
    try Check.run("\r", &.{.{ 0, 1, error.BareCarriageReturnInRawString }});
    try Check.run("🦀", &.{.{ 0, 4, error.NonAsciiCharInByte }});
    try Check.run("🦀a", &.{ .{ 0, 4, error.NonAsciiCharInByte }, .{ 4, 5, 'a' } });
}

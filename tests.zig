const std = @import("std");
const testing = std.testing;
const Utf8View = std.unicode.Utf8View;
const rustc_lexer = @import("rustc_lexer");
const RawStrError = rustc_lexer.RawStrError;
const Token = rustc_lexer.Token;
const tokenize = rustc_lexer.tokenize;
const stripShebang = rustc_lexer.stripShebang;

fn checkRawStr(s: []const u8, expected: RawStrError!u8) !void {
    const allocator = testing.allocator;
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
    const allocator = testing.allocator;
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
    const allocator = testing.allocator;
    var tokenizer = tokenize(try Utf8View.init(src));
    var actual = std.ArrayList(Token).init(allocator);
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

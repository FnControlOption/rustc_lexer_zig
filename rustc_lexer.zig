//! Low-level Rust lexer.
//!
//! The purpose of this crate is to convert raw sources into a labeled sequence
//! of well-known token types, so building an actual Rust token stream will
//! be easier.
//!
//! The main entity of this crate is the `TokenKind` tagged union which represents
//! common lexeme types.

const std = @import("std");
const fmt = std.fmt;
const testing = std.testing;
const mem = std.mem;
const math = std.math;
const debug = std.debug;
const assert = debug.assert;
const unicode = std.unicode;
const Utf8View = unicode.Utf8View;
const Utf8Iterator = unicode.Utf8Iterator;

pub const Cursor = @import("cursor.zig").Cursor;
const EOF_CHAR = @import("cursor.zig").EOF_CHAR;

pub const unescape = @import("unescape.zig");
const util = @import("util.zig");

const unicode_xid = @import("unicode_xid");
const unicode_emoji = @import("unicode_properties").emoji;

/// Parsed token.
/// It doesn't contain information about data that has been parsed,
/// only the type of the token and its size.
pub const Token = struct {
    kind: TokenKind,
    len: u32,

    pub fn init(kind: TokenKind, len: u32) Token {
        return .{ .kind = kind, .len = len };
    }
};

/// Tagged union representing common lexeme types.
pub const TokenKind = union(enum) {
    // Multi-char tokens:
    /// "// comment"
    line_comment: struct { doc_style: ?DocStyle },

    /// `/* block comment */`
    ///
    /// Block comments can be recursive, so a sequence like `/* /* */`
    /// will not be considered terminated and will result in a parsing error.
    block_comment: struct { doc_style: ?DocStyle, terminated: bool },

    /// Any whitespace character sequence.
    whitespace,

    /// "ident" or "continue"
    ///
    /// At this step, keywords are also considered identifiers.
    ident,

    /// Like the above, but containing invalid unicode codepoints.
    invalid_ident,

    /// "r#ident"
    raw_ident,

    /// An unknown prefix, like `foo#`, `foo'`, `foo"`.
    ///
    /// Note that only the
    /// prefix (`foo`) is included in the token, not the separator (which is
    /// lexed as its own distinct token). In Rust 2021 and later, reserved
    /// prefixes are reported as errors; in earlier editions, they result in a
    /// (allowed by default) lint, and are treated as regular identifier
    /// tokens.
    unknown_prefix,

    /// Similar to the above, but *always* an error on every edition. This is used
    /// for emoji identifier recovery, as those are not meant to be ever accepted.
    invalid_prefix,

    /// Examples: `12u8`, `1.0e-40`, `b"123"`. Note that `_` is an invalid
    /// suffix, but may be present here on string and float literals. Users of
    /// this type will need to check for and reject that case.
    literal: struct { kind: LiteralKind, suffix_start: u32 },

    /// "'a"
    lifetime: struct { starts_with_number: bool },

    // One-char tokens:
    /// ";"
    semi,
    /// ","
    comma,
    /// "."
    dot,
    /// "("
    open_paren,
    /// ")"
    close_paren,
    /// "{"
    open_brace,
    /// "}"
    close_brace,
    /// "["
    open_bracket,
    /// "]"
    close_bracket,
    /// "@"
    at,
    /// "#"
    pound,
    /// "~"
    tilde,
    /// "?"
    question,
    /// ":"
    colon,
    /// "$"
    dollar,
    /// "="
    eq,
    /// "!"
    bang,
    /// "<"
    lt,
    /// ">"
    gt,
    /// "-"
    minus,
    /// "&"
    @"and",
    /// "|"
    @"or",
    /// "+"
    plus,
    /// "*"
    star,
    /// "/"
    slash,
    /// "^"
    caret,
    /// "%"
    percent,

    /// Unknown token, not expected by the lexer, e.g. "№"
    unknown,

    /// End of input.
    eof,
};

pub const DocStyle = enum {
    outer,
    inner,
};

/// Tagged union representing the literal types supported by the lexer.
///
/// Note that the suffix is *not* considered when deciding the `LiteralKind` in
/// this type. This means that float literals like `1f32` are classified by this
/// type as `Int`.
pub const LiteralKind = union(enum) {
    /// "12_u8", "0o100", "0b120i99", "1f32".
    int: struct { base: Base, empty_int: bool },
    /// "12.34f32", "1e3", but not "1f32".
    float: struct { base: Base, empty_exponent: bool },
    /// "'a'", "'\\'", "'''", "';"
    char: struct { terminated: bool },
    /// "b'a'", "b'\\'", "b'''", "b';"
    byte: struct { terminated: bool },
    /// ""abc"", ""abc"
    str: struct { terminated: bool },
    /// "b"abc"", "b"abc"
    byte_str: struct { terminated: bool },
    /// `c"abc"`, `c"abc`
    c_str: struct { terminated: bool },
    /// "r"abc"", "r#"abc"#", "r####"ab"###"c"####", "r#"a". `None` indicates
    /// an invalid literal.
    raw_str: struct { n_hashes: ?u8 },
    /// "br"abc"", "br#"abc"#", "br####"ab"###"c"####", "br#"a". `None`
    /// indicates an invalid literal.
    raw_byte_str: struct { n_hashes: ?u8 },
    /// `cr"abc"`, "cr#"abc"#", `cr#"a`. `None` indicates an invalid literal.
    raw_c_str: struct { n_hashes: ?u8 },
};

pub const RawStrError = error{
    /// Non `#` characters exist between `r` and `"`, e.g. `r##~"abcde"##`
    InvalidStarter,
    /// The string was not terminated, e.g. `r###"abcde"##`.
    /// `possible_terminator_offset` is the number of characters after `r` or
    /// `br` where they may have intended to terminate it.
    NoTerminator,
    /// More than 255 `#`s exist.
    TooManyDelimiters,
};

/// Base of numeric literal encoding according to its prefix.
pub const Base = enum(u5) {
    /// Literal starts with "0b".
    binary = 2,
    /// Literal starts with "0o".
    octal = 8,
    /// Literal doesn't contain a prefix.
    decimal = 10,
    /// Literal starts with "0x".
    hexadecimal = 16,
};

/// `rustc` allows files to have a shebang, e.g. "#!/usr/bin/rustrun",
/// but shebang isn't a part of rust syntax.
pub fn stripShebang(input: Utf8View) ?usize {
    // Shebang must start with `#!` literally, without any preceding whitespace.
    // For simplicity we consider any line starting with `#!` a shebang,
    // regardless of restrictions put on shebangs by specific platforms.
    var iter = input.iterator();
    if (iter.nextCodepoint() == '#' and iter.nextCodepoint() == '!') {
        // Ok, this is a shebang but if the next non-whitespace token is `[`,
        // then it may be valid Rust code, so consider it Rust code.
        const input_tail = Utf8View.initUnchecked(iter.bytes[iter.i..]);

        var tokenizer = tokenize(input_tail);
        const next_non_whitespace_token = while (tokenizer.next()) |tok| {
            switch (tok.kind) {
                .whitespace => {
                    continue;
                },
                inline .line_comment, .block_comment => |comment| {
                    if (comment.doc_style == null)
                        continue;
                },
                else => {},
            }
            break tok.kind;
        } else return null;

        if (next_non_whitespace_token != .open_bracket) {
            // No other choice than to consider this a shebang.
            var next_line_index = iter.i;
            while (iter.nextCodepoint()) |c| {
                if (c == '\n')
                    break;
                next_line_index = iter.i;
            }
            return next_line_index;
        }
    }

    return null;
}

/// Validates a raw string literal. Used for getting more information about a
/// problem with a `RawStr`/`RawByteStr` with a `None` field.
pub inline fn validateRawStr(input: Utf8View, prefix_len: u32) RawStrError!void {
    assert(input.bytes.len > 0);
    var tokenizer = tokenize(input);
    // Move past the leading `r` or `br`.
    for (0..prefix_len) |_|
        _ = tokenizer.cursor.bump().?;
    _ = try tokenizer.rawDoubleQuotedString(prefix_len);
}

/// Creates an iterator that produces tokens from the input string.
pub fn tokenize(input: Utf8View) Tokenizer {
    return .{ .cursor = Cursor.init(input) };
}

/// True if `c` is considered a whitespace according to Rust language definition.
/// See [Rust language reference](https://doc.rust-lang.org/reference/whitespace.html)
/// for definitions of these classes.
pub fn isWhitespace(c: u21) bool {
    // This is Pattern_White_Space.
    //
    // Note that this set is stable (ie, it doesn't change with different
    // Unicode versions), so it's ok to just hard-code the values.

    return switch (c) {
        // Usual ASCII suspects
        '\u{0009}', // \t
        '\u{000A}', // \n
        '\u{000B}', // vertical tab
        '\u{000C}', // form feed
        '\u{000D}', // \r
        '\u{0020}', // space

        // NEXT LINE from latin1
        '\u{0085}',

        // Bidi markers
        '\u{200E}', // LEFT-TO-RIGHT MARK
        '\u{200F}', // RIGHT-TO-LEFT MARK

        // Dedicated whitespace characters from Unicode
        '\u{2028}', // LINE SEPARATOR
        '\u{2029}', // PARAGRAPH SEPARATOR
        => true,

        else => false,
    };
}

/// True if `c` is valid as a first character of an identifier.
/// See [Rust language reference](https://doc.rust-lang.org/reference/identifiers.html) for
/// a formal definition of valid identifier name.
pub fn isIdStart(c: u21) bool {
    // This is XID_Start OR '_' (which formally is not a XID_Start).
    return c == '_' or unicode_xid.isXidStart(c);
}

/// True if `c` is valid as a non-first character of an identifier.
/// See [Rust language reference](https://doc.rust-lang.org/reference/identifiers.html) for
/// a formal definition of valid identifier name.
pub fn isIdContinue(c: u21) bool {
    return unicode_xid.isXidContinue(c);
}

/// The passed string is lexically an identifier.
pub fn isIdent(view: Utf8View) bool {
    var iter = view.iterator();
    const start = iter.nextCodepoint() orelse
        return false;
    if (!isIdStart(start))
        return false;
    while (iter.nextCodepoint()) |c|
        if (!isIdContinue(c))
            return false;
    return true;
}

fn isFakeIdContinue(c: u21) bool {
    return unicode_xid.isXidContinue(c) or
        (util.isAscii(c) and unicode_emoji.isEmojiChar(c)) or
        c == '\u{200d}';
}

fn charIsNot(comptime c: u21) fn (u21) bool {
    const temp = struct {
        fn func(char: u21) bool {
            return char != c;
        }
    };
    return temp.func;
}

pub const Tokenizer = struct {
    cursor: Cursor,

    const Self = @This();

    pub fn next(self: *Self) ?Token {
        const token = self.advanceToken();
        return if (token.kind != .eof) token else null;
    }

    /// Parses a token from the input string.
    pub fn advanceToken(self: *Self) Token {
        const c = self.cursor.bump() orelse
            return Token.init(.eof, 0);

        const token_kind: TokenKind = switch (c) {
            // Slash, comment or block comment.
            '/' => switch (self.cursor.first()) {
                '/' => self.lineComment(),
                '*' => self.blockComment(),
                else => .slash,
            },

            // Raw identifier, raw string literal or identifier.
            'r' => blk: {
                const c0 = self.cursor.first();
                const c1 = self.cursor.second();

                if (c0 == '#' and isIdStart(c1))
                    break :blk self.rawIdent();

                if (c0 == '#' or c0 == '"') {
                    const n_hashes = self.rawDoubleQuotedString(1) catch null;
                    const suffix_start = self.cursor.posWithinToken();
                    if (n_hashes != null)
                        self.eatLiteralSuffix();
                    const kind = .{ .raw_str = .{ .n_hashes = n_hashes } };
                    break :blk .{ .literal = .{ .kind = kind, .suffix_start = suffix_start } };
                }

                break :blk self.identOrUnknownPrefix();
            },

            // Byte literal, byte string literal, raw byte string literal or identifier.
            'b' => self.cOrByteString("byte_str", "raw_byte_str", "byte"),

            // c-string literal, raw c-string literal or identifier.
            'c' => self.cOrByteString("c_str", "raw_c_str", null),

            // Numeric literal.
            '0'...'9' => blk: {
                const literal_kind = self.number(c);
                const suffix_start = self.cursor.posWithinToken();
                self.eatLiteralSuffix();
                break :blk .{ .literal = .{ .kind = literal_kind, .suffix_start = suffix_start } };
            },

            // One-symbol tokens.
            ';' => .semi,
            ',' => .comma,
            '.' => .dot,
            '(' => .open_paren,
            ')' => .close_paren,
            '{' => .open_brace,
            '}' => .close_brace,
            '[' => .open_bracket,
            ']' => .close_bracket,
            '@' => .at,
            '#' => .pound,
            '~' => .tilde,
            '?' => .question,
            ':' => .colon,
            '$' => .dollar,
            '=' => .eq,
            '!' => .bang,
            '<' => .lt,
            '>' => .gt,
            '-' => .minus,
            '&' => .@"and",
            '|' => .@"or",
            '+' => .plus,
            '*' => .star,
            '^' => .caret,
            '%' => .percent,

            // Lifetime or character literal.
            '\'' => self.lifetimeOrChar(),

            // String literal.
            '"' => blk: {
                const terminated = self.doubleQuotedString();
                const suffix_start = self.cursor.posWithinToken();
                if (terminated)
                    self.eatLiteralSuffix();
                const kind = .{ .str = .{ .terminated = terminated } };
                break :blk .{ .literal = .{ .kind = kind, .suffix_start = suffix_start } };
            },

            else => if (isWhitespace(c))
                // Whitespace sequence.
                self.whitespace()
            else if (isIdStart(c))
                // Identifier (this should be checked after other variant that can
                // start as identifier).
                self.identOrUnknownPrefix()
            else if (!util.isAscii(c) and unicode_emoji.isEmojiChar(c))
                // Identifier starting with an emoji. Only lexed for graceful error recovery.
                self.fakeIdentOrUnknownPrefix()
            else
                .unknown,
        };
        const res = Token.init(token_kind, self.cursor.posWithinToken());
        self.cursor.resetPosWithinToken();
        return res;
    }

    fn lineComment(self: *Self) TokenKind {
        assert(self.cursor.prev() == '/' and self.cursor.first() == '/');
        _ = self.cursor.bump();

        const doc_style = switch (self.cursor.first()) {
            // `//!` is an inner line doc comment.
            '!' => DocStyle.inner,
            // `////` (more than 3 slashes) is not considered a doc comment.
            '/' => switch (self.cursor.second()) {
                '/' => null,
                else => DocStyle.outer,
            },
            else => null,
        };

        self.cursor.eatWhile(charIsNot('\n'));
        return .{ .line_comment = .{ .doc_style = doc_style } };
    }

    fn blockComment(self: *Self) TokenKind {
        assert(self.cursor.prev() == '/' and self.cursor.first() == '*');
        _ = self.cursor.bump();

        const doc_style = switch (self.cursor.first()) {
            // `/*!` is an inner block doc comment.
            '!' => DocStyle.inner,
            // `/***` (more than 2 stars) is not considered a doc comment.
            // `/**/` is not considered a doc comment.
            '*' => switch (self.cursor.second()) {
                '*', '/' => null,
                else => DocStyle.outer,
            },
            else => null,
        };

        var depth: usize = 1;
        while (self.cursor.bump()) |c| {
            if (c == '/' and self.cursor.first() == '*') {
                _ = self.cursor.bump();
                depth += 1;
            } else if (c == '*' and self.cursor.first() == '/') {
                _ = self.cursor.bump();
                depth -= 1;
                if (depth == 0) {
                    // This block comment is closed, so for a construction like "/* */ */"
                    // there will be a successfully parsed block comment "/* */"
                    // and " */" will be processed separately.
                    break;
                }
            }
        }

        return .{ .block_comment = .{ .doc_style = doc_style, .terminated = depth == 0 } };
    }

    fn whitespace(self: *Self) TokenKind {
        assert(isWhitespace(self.cursor.prev()));
        self.cursor.eatWhile(isWhitespace);
        return .whitespace;
    }

    fn rawIdent(self: *Self) TokenKind {
        assert(self.cursor.prev() == 'r' and self.cursor.first() == '#' and isIdStart(self.cursor.second()));
        // Eat "#" symbol.
        _ = self.cursor.bump();
        // Eat the identifier part of RawIdent.
        self.eatIdentifier();
        return .raw_ident;
    }

    fn identOrUnknownPrefix(self: *Self) TokenKind {
        assert(isIdStart(self.cursor.prev()));
        // Start is already eaten, eat the rest of identifier.
        self.cursor.eatWhile(isIdContinue);
        // Known prefixes must have been handled earlier. So if
        // we see a prefix here, it is definitely an unknown prefix.
        return switch (self.cursor.first()) {
            '#', '"', '\'' => .unknown_prefix,
            else => |c| if (!util.isAscii(c) and unicode_emoji.isEmojiChar(c))
                self.fakeIdentOrUnknownPrefix()
            else
                .ident,
        };
    }

    fn fakeIdentOrUnknownPrefix(self: *Self) TokenKind {
        // Start is already eaten, eat the rest of identifier.
        self.cursor.eatWhile(isFakeIdContinue);
        // Known prefixes must have been handled earlier. So if
        // we see a prefix here, it is definitely an unknown prefix.
        return switch (self.cursor.first()) {
            '#', '"', '\'' => .invalid_prefix,
            else => .invalid_ident,
        };
    }

    fn cOrByteString(
        self: *Self,
        comptime kind_name: []const u8,
        comptime raw_kind_name: []const u8,
        comptime single_quoted: ?[]const u8,
    ) TokenKind {
        switch (self.cursor.first()) {
            '\'' => if (single_quoted) |field_name| {
                _ = self.cursor.bump();
                const terminated = self.singleQuotedString();
                const suffix_start = self.cursor.posWithinToken();
                if (terminated)
                    self.eatLiteralSuffix();
                const kind = @unionInit(LiteralKind, field_name, .{ .terminated = terminated });
                return .{ .literal = .{ .kind = kind, .suffix_start = suffix_start } };
            },
            '"' => {
                _ = self.cursor.bump();
                const terminated = self.doubleQuotedString();
                const suffix_start = self.cursor.posWithinToken();
                if (terminated)
                    self.eatLiteralSuffix();
                const kind = @unionInit(LiteralKind, kind_name, .{ .terminated = terminated });
                return .{ .literal = .{ .kind = kind, .suffix_start = suffix_start } };
            },
            'r' => switch (self.cursor.second()) {
                '"', '#' => {
                    _ = self.cursor.bump();
                    const n_hashes = self.rawDoubleQuotedString(2) catch null;
                    const suffix_start = self.cursor.posWithinToken();
                    if (n_hashes != null)
                        self.eatLiteralSuffix();
                    const kind = @unionInit(LiteralKind, raw_kind_name, .{ .n_hashes = n_hashes });
                    return .{ .literal = .{ .kind = kind, .suffix_start = suffix_start } };
                },
                else => {},
            },
            else => {},
        }
        return self.identOrUnknownPrefix();
    }

    fn number(self: *Self, first_digit: u21) LiteralKind {
        assert('0' <= self.cursor.prev() and self.cursor.prev() <= '9');
        var base = Base.decimal;
        if (first_digit == '0') {
            // Attempt to parse encoding base.
            switch (self.cursor.first()) {
                'b' => {
                    base = .binary;
                    _ = self.cursor.bump();
                    if (!self.eatDecimalDigits())
                        return .{ .int = .{ .base = base, .empty_int = true } };
                },
                'o' => {
                    base = .octal;
                    _ = self.cursor.bump();
                    if (!self.eatDecimalDigits())
                        return .{ .int = .{ .base = base, .empty_int = true } };
                },
                'x' => {
                    base = .hexadecimal;
                    _ = self.cursor.bump();
                    if (!self.eatHexadecimalDigits())
                        return .{ .int = .{ .base = base, .empty_int = true } };
                },
                // Not a base prefix; consume additional digits.
                '0'...'9', '_' => _ = self.eatDecimalDigits(),

                // Also not a base prefix; nothing more to do here.
                '.', 'e', 'E' => {},

                // Just a 0.
                else => return .{ .int = .{ .base = base, .empty_int = false } },
            }
        } else {
            // No base prefix, parse number in the usual way.
            _ = self.eatDecimalDigits();
        }

        const c0 = self.cursor.first();
        const c1 = self.cursor.second();

        // Don't be greedy if this is actually an
        // integer literal followed by field/method access or a range pattern
        // (`0..2` and `12.foo()`)
        if (c0 == '.' and c1 != '.' and !isIdStart(c1)) {
            _ = self.cursor.bump();
            var empty_exponent = false;
            if (util.isAsciiDigit(self.cursor.first())) {
                _ = self.eatDecimalDigits();
                switch (self.cursor.first()) {
                    'e', 'E' => {
                        _ = self.cursor.bump();
                        empty_exponent = !self.eatFloatExponent();
                    },
                    else => {},
                }
            }
            return .{ .float = .{ .base = base, .empty_exponent = empty_exponent } };
        }

        if (c0 == 'e' or c0 == 'E') {
            _ = self.cursor.bump();
            const empty_exponent = !self.eatFloatExponent();
            return .{ .float = .{ .base = base, .empty_exponent = empty_exponent } };
        }

        return .{ .int = .{ .base = base, .empty_int = false } };
    }

    fn lifetimeOrChar(self: *Self) TokenKind {
        assert(self.cursor.prev() == '\'');

        const can_be_a_lifetime = if (self.cursor.second() == '\'')
            // It's surely not a lifetime.
            false
        else
            // If the first symbol is valid for identifier, it can be a lifetime.
            // Also check if it's a number for a better error reporting (so '0 will
            // be reported as invalid lifetime and not as unterminated char literal).
            isIdStart(self.cursor.first()) or util.isAsciiDigit(self.cursor.first());

        if (!can_be_a_lifetime) {
            const terminated = self.singleQuotedString();
            const suffix_start = self.cursor.posWithinToken();
            if (terminated)
                self.eatLiteralSuffix();
            const kind = .{ .char = .{ .terminated = terminated } };
            return .{ .literal = .{ .kind = kind, .suffix_start = suffix_start } };
        }

        // Either a lifetime or a character literal with
        // length greater than 1.

        const starts_with_number = util.isAsciiDigit(self.cursor.first());

        // Skip the literal contents.
        // First symbol can be a number (which isn't a valid identifier start),
        // so skip it without any checks.
        _ = self.cursor.bump();
        self.cursor.eatWhile(isIdContinue);

        // Check if after skipping literal contents we've met a closing
        // single quote (which means that user attempted to create a
        // string with single quotes).
        if (self.cursor.first() == '\'') {
            _ = self.cursor.bump();
            const kind = .{ .char = .{ .terminated = true } };
            return .{ .literal = .{ .kind = kind, .suffix_start = self.cursor.posWithinToken() } };
        } else {
            return .{ .lifetime = .{ .starts_with_number = starts_with_number } };
        }
    }

    fn singleQuotedString(self: *Self) bool {
        assert(self.cursor.prev() == '\'');

        // Check if it's a one-symbol literal.
        if (self.cursor.second() == '\'' and self.cursor.first() != '\\') {
            _ = self.cursor.bump();
            _ = self.cursor.bump();
            return true;
        }

        // Literal has more than one symbol.

        // Parse until either quotes are terminated or error is detected.
        while (true) {
            const c = self.cursor.first();
            if (c == '\'') {
                // Quotes are terminated, finish parsing.
                _ = self.cursor.bump();
                return true;
            }

            if (c == '/')
                // Probably beginning of the comment, which we don't want to include
                // to the error report.
                break;

            if (c == '\n' and self.cursor.second() != '\'')
                // Newline without following '\'' means unclosed quote, stop parsing.
                break;

            if (c == EOF_CHAR and self.cursor.isEof())
                // End of file, stop parsing.
                break;

            if (c == '\\') {
                // Escaped slash is considered one character, so bump twice.
                _ = self.cursor.bump();
                _ = self.cursor.bump();
            } else {
                // Skip the character.
                _ = self.cursor.bump();
            }
        }

        // String was not terminated.
        return false;
    }

    /// Eats double-quoted string and returns true
    /// if string is terminated.
    fn doubleQuotedString(self: *Self) bool {
        assert(self.cursor.prev() == '"');
        while (self.cursor.bump()) |c| {
            if (c == '"')
                return true;

            if (c == '\\' and (self.cursor.first() == '\\' or self.cursor.first() == '"'))
                // Bump again to skip escaped character.
                _ = self.cursor.bump();
        }
        // End of file reached.
        return false;
    }

    /// Eats the double-quoted string and returns `n_hashes` and an error if encountered.
    pub fn rawDoubleQuotedString(self: *Self, prefix_len: u32) RawStrError!u8 {
        // Wrap the actual function to handle the error with too many hashes.
        // This way, it eats the whole raw string.
        const n_hashes = try self.rawStringUnvalidated(prefix_len);
        // Only up to 255 `#`s are allowed in raw strings
        return math.cast(u8, n_hashes) orelse error.TooManyDelimiters;
    }

    fn rawStringUnvalidated(self: *Self, prefix_len: u32) RawStrError!u32 {
        assert(self.cursor.prev() == 'r');
        const start_pos = self.cursor.posWithinToken();
        var possible_terminator_offset: ?u32 = null;
        var max_hashes: u32 = 0;

        // Count opening '#' symbols.
        var eaten: u32 = 0;
        while (self.cursor.first() == '#') {
            eaten += 1;
            _ = self.cursor.bump();
        }
        const n_start_hashes = eaten;

        // Check that string is started.
        if (self.cursor.bump() != '"')
            return error.InvalidStarter;

        // Skip the string contents and on each '#' character met, check if this is
        // a raw string termination.
        while (true) {
            self.cursor.eatWhile(charIsNot('"'));

            if (self.cursor.isEof())
                return error.NoTerminator;

            // Eat closing double quote.
            _ = self.cursor.bump();

            // Check that amount of closing '#' symbols
            // is equal to the amount of opening ones.
            // Note that this will not consume extra trailing `#` characters:
            // `r###"abcde"####` is lexed as a `RawStr { n_hashes: 3 }`
            // followed by a `#` token.
            var n_end_hashes: u32 = 0;
            while (self.cursor.first() == '#' and n_end_hashes < n_start_hashes) {
                n_end_hashes += 1;
                _ = self.cursor.bump();
            }

            if (n_end_hashes == n_start_hashes)
                return n_start_hashes;

            if (n_end_hashes > max_hashes) {
                // Keep track of possible terminators to give a hint about
                // where there might be a missing terminator
                possible_terminator_offset = self.cursor.posWithinToken() - start_pos - n_end_hashes + prefix_len;
                max_hashes = n_end_hashes;
            }
        }
    }

    fn eatDecimalDigits(self: *Self) bool {
        var has_digits = false;
        while (true) {
            switch (self.cursor.first()) {
                '_' => {
                    _ = self.cursor.bump();
                },
                '0'...'9' => {
                    has_digits = true;
                    _ = self.cursor.bump();
                },
                else => break,
            }
        }
        return has_digits;
    }

    fn eatHexadecimalDigits(self: *Self) bool {
        var has_digits = false;
        while (true) {
            switch (self.cursor.first()) {
                '_' => {
                    _ = self.cursor.bump();
                },
                '0'...'9', 'a'...'f', 'A'...'F' => {
                    has_digits = true;
                    _ = self.cursor.bump();
                },
                else => break,
            }
        }
        return has_digits;
    }

    /// Eats the float exponent. Returns true if at least one digit was met,
    /// and returns false otherwise.
    fn eatFloatExponent(self: *Self) bool {
        assert(self.cursor.prev() == 'e' or self.cursor.prev() == 'E');
        if (self.cursor.first() == '-' or self.cursor.first() == '+') {
            _ = self.cursor.bump();
        }
        return self.eatDecimalDigits();
    }

    // Eats the suffix of the literal, e.g. "u8".
    fn eatLiteralSuffix(self: *Self) void {
        self.eatIdentifier();
    }

    // Eats the identifier. Note: succeeds on `_`, which isn't a valid
    // identifier.
    fn eatIdentifier(self: *Self) void {
        if (!isIdStart(self.cursor.first()))
            return;

        _ = self.cursor.bump();

        self.cursor.eatWhile(isIdContinue);
    }
};

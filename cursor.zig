const std = @import("std");
const Utf8View = std.unicode.Utf8View;
const Utf8Iterator = std.unicode.Utf8Iterator;
const utf8EncodeComptime = std.unicode.utf8EncodeComptime;
const runtime_safety = std.debug.runtime_safety;

pub const EOF_CHAR: u21 = 0;

/// Peekable iterator over a char sequence.
///
/// Next characters can be peeked via `first` method,
/// and position can be shifted forward via `bump` method.
pub const Cursor = struct {
    len_remaining: usize,
    /// Iterator over chars.
    iter: Utf8Iterator,
    previous: if (runtime_safety) u21 else void,

    const Self = @This();

    pub fn init(input: Utf8View) Self {
        return Self{
            .len_remaining = input.bytes.len,
            .iter = input.iterator(),
            .previous = EOF_CHAR,
        };
    }

    pub fn view(self: Self) Utf8View {
        const slice = self.iter.bytes[self.iter.i..];
        return Utf8View.initUnchecked(slice);
    }

    /// Returns the last eaten symbol (or `'\0'` in release builds).
    /// (For debug assertions only.)
    pub fn prev(self: Self) u21 {
        return if (runtime_safety) self.previous else EOF_CHAR;
    }

    /// Peeks the next symbol from the input stream without consuming it.
    /// If requested position doesn't exist, `EOF_CHAR` is returned.
    /// However, getting `EOF_CHAR` doesn't always mean actual end of file,
    /// it should be checked with `is_eof` method.
    pub fn first(self: Self) u21 {
        var iter = Utf8Iterator{ .bytes = self.iter.bytes, .i = self.iter.i };
        return iter.nextCodepoint() orelse EOF_CHAR;
    }

    /// Peeks the second symbol from the input stream without consuming it.
    pub fn second(self: Self) u21 {
        var iter = Utf8Iterator{ .bytes = self.iter.bytes, .i = self.iter.i };
        _ = iter.nextCodepoint();
        return iter.nextCodepoint() orelse EOF_CHAR;
    }

    /// Peeks the third symbol from the input stream without consuming it.
    pub fn third(self: Self) u21 {
        var iter = Utf8Iterator{ .bytes = self.iter.bytes, .i = self.iter.i };
        _ = iter.nextCodepoint();
        _ = iter.nextCodepoint();
        return iter.nextCodepoint() orelse EOF_CHAR;
    }

    /// Checks if there is nothing more to consume.
    pub fn isEof(self: Self) bool {
        return self.iter.i >= self.iter.bytes.len;
    }

    /// Returns amount of already consumed symbols.
    pub fn posWithinToken(self: Self) u32 {
        const unconsumed = self.iter.bytes.len - self.iter.i;
        return @intCast(self.len_remaining - unconsumed);
    }

    /// Resets the number of bytes consumed to 0.
    pub fn resetPosWithinToken(self: *Self) void {
        self.len_remaining = self.iter.bytes.len - self.iter.i;
    }

    /// Moves to the next character.
    pub fn bump(self: *Self) ?u21 {
        const c = self.iter.nextCodepoint() orelse return null;
        if (runtime_safety) self.previous = c;
        return c;
    }

    /// Eats symbols while predicate returns true or until the end of file is reached.
    pub fn eatWhile(self: *Self, predicate: fn (u21) bool) void {
        while (predicate(self.first()) and !self.isEof()) {
            _ = self.bump();
        }
    }
};

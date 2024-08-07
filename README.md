# rustc_lexer_zig

[Zig](https://ziglang.org) port of [rustc_lexer](https://github.com/rust-lang/rust/tree/master/compiler/rustc_lexer)

## Highlights

### [`@unionInit`](https://ziglang.org/documentation/master/#unionInit)

<table>
<tr>
<th>Rust 🦀</th>
<td>

```rust
match first_char {
    // ...

    'b' => self.c_or_byte_string(
        |terminated| ByteStr { terminated },
        |n_hashes| RawByteStr { n_hashes },
        Some(|terminated| Byte { terminated }),
    ),

    'c' => self.c_or_byte_string(
        |terminated| CStr { terminated },
        |n_hashes| RawCStr { n_hashes },
        None,
    ),

    // ...
}
```

</td>
</tr>
<tr></tr>
<tr>
<th>Zig ⚡</th>
<td>

```zig
switch (c) {
    // ...

    'b' => self.cOrByteString("byte_str", "raw_byte_str", "byte"),

    'c' => self.cOrByteString("c_str", "raw_c_str", null),

    // ...
}
```

</td>
</tr>
</table>

<table>
<tr>
<th>Rust 🦀</th>
<td>

```rust
fn c_or_byte_string(
    &mut self,
    mk_kind: impl FnOnce(bool) -> LiteralKind,
    mk_kind_raw: impl FnOnce(Option<u8>) -> LiteralKind,
    single_quoted: Option<fn(bool) -> LiteralKind>,
) -> TokenKind {
```

</td>
</tr>
<tr></tr>
<tr>
<th>Zig ⚡</th>
<td>

```zig
fn cOrByteString(
    self: *Self,
    comptime kind_name: []const u8,
    comptime raw_kind_name: []const u8,
    comptime single_quoted: ?[]const u8,
) TokenKind {
```

</td>
</tr>
</table>

<table>
<tr>
<th>Rust 🦀</th>
<td>

```rust
let kind = mk_kind(terminated);
```

</td>
</tr>
<tr></tr>
<tr>
<th>Zig ⚡</th>
<td>

```zig
const kind = @unionInit(LiteralKind, kind_name, .{ .terminated = terminated });
```

</td>
</tr>
</table>

### [Multiline String Literals](https://ziglang.org/documentation/master/#Multiline-String-Literals)

<table>
<tr>
<th>Rust 🦀</th>
<td>

```rust
#[test]
fn comment_flavors() {
    check_lexing(
        r"
// line
//// line as well
/// outer doc line
//! inner doc line
/* block */
/**/
/*** also block */
/** outer doc block */
/*! inner doc block */
",
        // ...
    )
}
```

</td>
</tr>
<tr></tr>
<tr>
<th>Zig ⚡</th>
<td>

```zig
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
        // ...
    );
}
```

</td>
</tr>
</table>

### [Anonymous Struct Literals](https://ziglang.org/documentation/master/#Anonymous-Struct-Literals)

<table>
<tr>
<th>Rust 🦀</th>
<td>

```rust
#[test]
fn smoke_test() {
    check_lexing(
        "/* my source file */ fn main() { println!(\"zebra\"); }\n",
        expect![[r#"
            Token { kind: BlockComment { doc_style: None, terminated: true }, len: 20 }
            Token { kind: Whitespace, len: 1 }
            ...
        "#]],
    )
}
```

</td>
</tr>
<tr></tr>
<tr>
<th>Zig ⚡</th>
<td>

```zig
test "smoke test" {
    try checkLexing(
        "/* my source file */ fn main() { println!(\"zebra\"); }\n",
        &.{
            .{ .kind = .{ .block_comment = .{ .doc_style = null, .terminated = true } }, .len = 20 },
            .{ .kind = .whitespace, .len = 1 },
            // ...
        },
    );
}
```

</td>
</tr>
</table>

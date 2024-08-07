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

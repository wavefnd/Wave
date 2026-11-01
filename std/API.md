# Public standard-library API

Generated with `python3 tools/document_std.py`; run the same command after changing
public declarations. `--check` detects a stale inventory. Signatures and public
re-exports below come from source; private imports are omitted. A source path under
`sys/<os>/<arch>` is target-specific even when no additional attribute is shown.
Attributes select declarations; source presence alone does not establish runtime support.
See [system capabilities](SYSTEMS.md) and [compiler targets](../README.md#target-families).

```wave
import("std::fs::file")::{fs_open};
import("std::fs::file" as file);
```

`fs_open` is the portable file API; `std::sys::fs::open` is its raw OS boundary.
The former `std::libc` binding modules have been removed; they are not public std API.

## `std::buffer::alloc`

[Source](buffer/alloc.wave)

[Declaration](buffer/alloc.wave#L29)

```wave
pub import("std::buffer::error")::{
    BUFFER_OK,
    BUFFER_ERR_INVALID,
    BUFFER_ERR_BOUNDS,
    BUFFER_ERR_OVERFLOW,
    BUFFER_ERR_ALLOC,
};
```

[Declaration](buffer/alloc.wave#L36)

```wave
pub import("std::buffer::types")::{Buffer};
```

[Declaration](buffer/alloc.wave#L44)

```wave
pub fun buffer_is_valid(buf: ptr<Buffer>) -> bool
```

[Declaration](buffer/alloc.wave#L67)

```wave
pub fun buffer_init(out_buf: ptr<Buffer>, capacity: i64) -> i64
```

[Declaration](buffer/alloc.wave#L90)

```wave
pub fun buffer_new(capacity: i64) -> Buffer
```

[Declaration](buffer/alloc.wave#L96)

```wave
pub fun buffer_new_default() -> Buffer
```

[Declaration](buffer/alloc.wave#L100)

```wave
pub fun buffer_free(buf: ptr<Buffer>) -> i64
```

[Declaration](buffer/alloc.wave#L121)

```wave
pub fun buffer_clear(buf: ptr<Buffer>) -> i64
```

[Declaration](buffer/alloc.wave#L130)

```wave
pub fun buffer_reserve(buf: ptr<Buffer>, required_cap: i64) -> i64
```

[Declaration](buffer/alloc.wave#L177)

```wave
pub fun buffer_reserve_additional(buf: ptr<Buffer>, additional: i64) -> i64
```

[Declaration](buffer/alloc.wave#L195)

```wave
pub fun buffer_truncate(buf: ptr<Buffer>, new_len: i64) -> i64
```

[Declaration](buffer/alloc.wave#L207)

```wave
pub fun buffer_resize(buf: ptr<Buffer>, new_len: i64, value: u8) -> i64
```

## `std::buffer::error`

[Source](buffer/error.wave)

[Declaration](buffer/error.wave#L7)

```wave
pub const BUFFER_OK: i64 = 0;
```

[Declaration](buffer/error.wave#L8)

```wave
pub const BUFFER_ERR_INVALID: i64 = -4200;
```

[Declaration](buffer/error.wave#L9)

```wave
pub const BUFFER_ERR_BOUNDS: i64 = -4201;
```

[Declaration](buffer/error.wave#L10)

```wave
pub const BUFFER_ERR_OVERFLOW: i64 = -4202;
```

[Declaration](buffer/error.wave#L11)

```wave
pub const BUFFER_ERR_ALLOC: i64 = -4203;
```

## `std::buffer::read`

[Source](buffer/read.wave)

[Declaration](buffer/read.wave#L25)

```wave
pub fun buffer_len(buf: ptr<Buffer>) -> i64
```

[Declaration](buffer/read.wave#L32)

```wave
pub fun buffer_capacity(buf: ptr<Buffer>) -> i64
```

[Declaration](buffer/read.wave#L39)

```wave
pub fun buffer_is_empty(buf: ptr<Buffer>) -> bool
```

[Declaration](buffer/read.wave#L43)

```wave
pub fun buffer_as_ptr(buf: ptr<Buffer>) -> ptr<u8>
```

[Declaration](buffer/read.wave#L51)

```wave
pub fun buffer_get(buf: ptr<Buffer>, index: i64, out_value: ptr<u8>) -> i64
```

[Declaration](buffer/read.wave#L66)

```wave
pub fun buffer_at(buf: Buffer, index: i64) -> u8
```

## `std::buffer::types`

[Source](buffer/types.wave)

[Declaration](buffer/types.wave#L19)

```wave
pub struct Buffer {
    data: ptr<u8>;
    len: i64;
    cap: i64;
}
```

## `std::buffer::write`

[Source](buffer/write.wave)

[Declaration](buffer/write.wave#L34)

```wave
pub fun buffer_push(buf: ptr<Buffer>, value: u8) -> i64
```

[Declaration](buffer/write.wave#L53)

```wave
pub fun buffer_append(buf: ptr<Buffer>, src: ptr<u8>, size: i64) -> i64
```

[Declaration](buffer/write.wave#L112)

```wave
pub fun buffer_append_str(buf: ptr<Buffer>, s: str) -> i64
```

[Declaration](buffer/write.wave#L121)

```wave
pub fun buffer_set(buf: ptr<Buffer>, index: i64, value: u8) -> bool
```

[Declaration](buffer/write.wave#L125)

```wave
pub fun buffer_set_checked(buf: ptr<Buffer>, index: i64, value: u8) -> i64
```

[Declaration](buffer/write.wave#L139)

```wave
pub fun buffer_pop(buf: ptr<Buffer>, out_value: ptr<u8>) -> i64
```

[Declaration](buffer/write.wave#L155)

```wave
pub fun buffer_append_buffer(dst: ptr<Buffer>, src: ptr<Buffer>) -> i64
```

[Declaration](buffer/write.wave#L165)

```wave
pub fun buffer_remove(buf: ptr<Buffer>, offset: i64, count: i64) -> i64
```

## `std::bytes::cursor`

[Source](bytes/cursor.wave)

[Declaration](bytes/cursor.wave#L6)

```wave
pub import("std::bytes::types")::{Bytes, BytesMut, ByteReader, ByteWriter};
```

[Declaration](bytes/cursor.wave#L8)

```wave
pub import("std::bytes::errors")::{
    BYTES_OK,
    BYTES_ERROR_INVALID,
    BYTES_ERROR_EOF,
    BYTES_ERROR_NO_SPACE,
    BYTES_ERROR_OVERFLOW,
};
```

[Declaration](bytes/cursor.wave#L36)

```wave
pub fun bytes_reader(data: ptr<u8>, len: i64) -> ByteReader
```

[Declaration](bytes/cursor.wave#L40)

```wave
pub fun bytes_reader_from(src: Bytes) -> ByteReader
```

[Declaration](bytes/cursor.wave#L44)

```wave
pub fun bytes_writer(data: ptr<u8>, len: i64) -> ByteWriter
```

[Declaration](bytes/cursor.wave#L48)

```wave
pub fun bytes_writer_from(dst: BytesMut) -> ByteWriter
```

[Declaration](bytes/cursor.wave#L68)

```wave
pub fun bytes_reader_position(reader: ByteReader) -> i64
```

[Declaration](bytes/cursor.wave#L69)

```wave
pub fun bytes_writer_position(writer: ByteWriter) -> i64
```

[Declaration](bytes/cursor.wave#L71)

```wave
pub fun bytes_reader_remaining(reader: ByteReader) -> i64
```

[Declaration](bytes/cursor.wave#L81)

```wave
pub fun bytes_writer_remaining(writer: ByteWriter) -> i64
```

[Declaration](bytes/cursor.wave#L91)

```wave
pub fun bytes_reader_seek(reader: ptr<ByteReader>, position: i64) -> i32
```

[Declaration](bytes/cursor.wave#L100)

```wave
pub fun bytes_writer_seek(writer: ptr<ByteWriter>, position: i64) -> i32
```

[Declaration](bytes/cursor.wave#L109)

```wave
pub fun bytes_reader_skip(reader: ptr<ByteReader>, count: i64) -> i32
```

[Declaration](bytes/cursor.wave#L120)

```wave
pub fun bytes_writer_skip(writer: ptr<ByteWriter>, count: i64) -> i32
```

[Declaration](bytes/cursor.wave#L131)

```wave
pub fun bytes_reader_peek_u8(reader: ptr<ByteReader>, out_value: ptr<u8>) -> i32
```

[Declaration](bytes/cursor.wave#L138)

```wave
pub fun bytes_reader_peek_be_u16(reader: ptr<ByteReader>, out_value: ptr<u16>) -> i32
```

[Declaration](bytes/cursor.wave#L146)

```wave
pub fun bytes_reader_peek_le_u16(reader: ptr<ByteReader>, out_value: ptr<u16>) -> i32
```

[Declaration](bytes/cursor.wave#L154)

```wave
pub fun bytes_reader_peek_be_u32(reader: ptr<ByteReader>, out_value: ptr<u32>) -> i32
```

[Declaration](bytes/cursor.wave#L162)

```wave
pub fun bytes_reader_peek_le_u32(reader: ptr<ByteReader>, out_value: ptr<u32>) -> i32
```

[Declaration](bytes/cursor.wave#L170)

```wave
pub fun bytes_reader_peek_be_u64(reader: ptr<ByteReader>, out_value: ptr<u64>) -> i32
```

[Declaration](bytes/cursor.wave#L178)

```wave
pub fun bytes_reader_peek_le_u64(reader: ptr<ByteReader>, out_value: ptr<u64>) -> i32
```

[Declaration](bytes/cursor.wave#L186)

```wave
pub fun bytes_reader_read_u8(reader: ptr<ByteReader>, out_value: ptr<u8>) -> i32
```

[Declaration](bytes/cursor.wave#L195)

```wave
pub fun bytes_reader_read_be_u16(reader: ptr<ByteReader>, out_value: ptr<u16>) -> i32
```

[Declaration](bytes/cursor.wave#L204)

```wave
pub fun bytes_reader_read_le_u16(reader: ptr<ByteReader>, out_value: ptr<u16>) -> i32
```

[Declaration](bytes/cursor.wave#L213)

```wave
pub fun bytes_reader_read_be_u32(reader: ptr<ByteReader>, out_value: ptr<u32>) -> i32
```

[Declaration](bytes/cursor.wave#L222)

```wave
pub fun bytes_reader_read_le_u32(reader: ptr<ByteReader>, out_value: ptr<u32>) -> i32
```

[Declaration](bytes/cursor.wave#L231)

```wave
pub fun bytes_reader_read_be_u64(reader: ptr<ByteReader>, out_value: ptr<u64>) -> i32
```

[Declaration](bytes/cursor.wave#L240)

```wave
pub fun bytes_reader_read_le_u64(reader: ptr<ByteReader>, out_value: ptr<u64>) -> i32
```

[Declaration](bytes/cursor.wave#L249)

```wave
pub fun bytes_reader_read_be_i16(reader: ptr<ByteReader>, out_value: ptr<i16>) -> i32
```

[Declaration](bytes/cursor.wave#L258)

```wave
pub fun bytes_reader_read_le_i16(reader: ptr<ByteReader>, out_value: ptr<i16>) -> i32
```

[Declaration](bytes/cursor.wave#L267)

```wave
pub fun bytes_reader_read_be_i32(reader: ptr<ByteReader>, out_value: ptr<i32>) -> i32
```

[Declaration](bytes/cursor.wave#L276)

```wave
pub fun bytes_reader_read_le_i32(reader: ptr<ByteReader>, out_value: ptr<i32>) -> i32
```

[Declaration](bytes/cursor.wave#L285)

```wave
pub fun bytes_reader_read_be_i64(reader: ptr<ByteReader>, out_value: ptr<i64>) -> i32
```

[Declaration](bytes/cursor.wave#L294)

```wave
pub fun bytes_reader_read_le_i64(reader: ptr<ByteReader>, out_value: ptr<i64>) -> i32
```

[Declaration](bytes/cursor.wave#L303)

```wave
pub fun bytes_reader_read_exact(reader: ptr<ByteReader>, dst: BytesMut) -> i32
```

[Declaration](bytes/cursor.wave#L312)

```wave
pub fun bytes_writer_write_u8(writer: ptr<ByteWriter>, value: u8) -> i32
```

[Declaration](bytes/cursor.wave#L321)

```wave
pub fun bytes_writer_write_be_u16(writer: ptr<ByteWriter>, value: u16) -> i32
```

[Declaration](bytes/cursor.wave#L330)

```wave
pub fun bytes_writer_write_le_u16(writer: ptr<ByteWriter>, value: u16) -> i32
```

[Declaration](bytes/cursor.wave#L339)

```wave
pub fun bytes_writer_write_be_u32(writer: ptr<ByteWriter>, value: u32) -> i32
```

[Declaration](bytes/cursor.wave#L348)

```wave
pub fun bytes_writer_write_le_u32(writer: ptr<ByteWriter>, value: u32) -> i32
```

[Declaration](bytes/cursor.wave#L357)

```wave
pub fun bytes_writer_write_be_u64(writer: ptr<ByteWriter>, value: u64) -> i32
```

[Declaration](bytes/cursor.wave#L366)

```wave
pub fun bytes_writer_write_le_u64(writer: ptr<ByteWriter>, value: u64) -> i32
```

[Declaration](bytes/cursor.wave#L375)

```wave
pub fun bytes_writer_write_be_i16(writer: ptr<ByteWriter>, value: i16) -> i32
```

[Declaration](bytes/cursor.wave#L379)

```wave
pub fun bytes_writer_write_le_i16(writer: ptr<ByteWriter>, value: i16) -> i32
```

[Declaration](bytes/cursor.wave#L383)

```wave
pub fun bytes_writer_write_be_i32(writer: ptr<ByteWriter>, value: i32) -> i32
```

[Declaration](bytes/cursor.wave#L387)

```wave
pub fun bytes_writer_write_le_i32(writer: ptr<ByteWriter>, value: i32) -> i32
```

[Declaration](bytes/cursor.wave#L391)

```wave
pub fun bytes_writer_write_be_i64(writer: ptr<ByteWriter>, value: i64) -> i32
```

[Declaration](bytes/cursor.wave#L395)

```wave
pub fun bytes_writer_write_le_i64(writer: ptr<ByteWriter>, value: i64) -> i32
```

[Declaration](bytes/cursor.wave#L399)

```wave
pub fun bytes_writer_write_exact(writer: ptr<ByteWriter>, src: Bytes) -> i32
```

[Declaration](bytes/cursor.wave#L409)

```wave
pub fun bytes_reader_read_view(reader: ptr<ByteReader>, count: i64, output: ptr<Bytes>) -> i32
```

## `std::bytes::endian`

[Source](bytes/endian.wave)

[Declaration](bytes/endian.wave#L9)

```wave
pub fun bytes_swap_u16(value: u16) -> u16
```

[Declaration](bytes/endian.wave#L14)

```wave
pub fun bytes_swap_u32(value: u32) -> u32
```

[Declaration](bytes/endian.wave#L21)

```wave
pub fun bytes_swap_u64(value: u64) -> u64
```

[Declaration](bytes/endian.wave#L32)

```wave
pub fun bytes_swap_i16(value: i16) -> i16
```

[Declaration](bytes/endian.wave#L36)

```wave
pub fun bytes_swap_i32(value: i32) -> i32
```

[Declaration](bytes/endian.wave#L40)

```wave
pub fun bytes_swap_i64(value: i64) -> i64
```

[Declaration](bytes/endian.wave#L45)

```wave
pub fun bytes_swap16(value: i16) -> i16
```

[Declaration](bytes/endian.wave#L46)

```wave
pub fun bytes_swap32(value: i32) -> i32
```

[Declaration](bytes/endian.wave#L47)

```wave
pub fun bytes_swap64(value: i64) -> i64
```

[Declaration](bytes/endian.wave#L49)

```wave
pub fun bytes_load_be_u16(src: ptr<u8>) -> u16
```

[Declaration](bytes/endian.wave#L53)

```wave
pub fun bytes_load_le_u16(src: ptr<u8>) -> u16
```

[Declaration](bytes/endian.wave#L57)

```wave
pub fun bytes_store_be_u16(dst: ptr<u8>, value: u16)
```

[Declaration](bytes/endian.wave#L62)

```wave
pub fun bytes_store_le_u16(dst: ptr<u8>, value: u16)
```

[Declaration](bytes/endian.wave#L67)

```wave
pub fun bytes_load_be_u32(src: ptr<u8>) -> u32
```

[Declaration](bytes/endian.wave#L74)

```wave
pub fun bytes_load_le_u32(src: ptr<u8>) -> u32
```

[Declaration](bytes/endian.wave#L81)

```wave
pub fun bytes_store_be_u32(dst: ptr<u8>, value: u32)
```

[Declaration](bytes/endian.wave#L88)

```wave
pub fun bytes_store_le_u32(dst: ptr<u8>, value: u32)
```

[Declaration](bytes/endian.wave#L95)

```wave
pub fun bytes_load_be_u64(src: ptr<u8>) -> u64
```

[Declaration](bytes/endian.wave#L106)

```wave
pub fun bytes_load_le_u64(src: ptr<u8>) -> u64
```

[Declaration](bytes/endian.wave#L117)

```wave
pub fun bytes_store_be_u64(dst: ptr<u8>, value: u64)
```

[Declaration](bytes/endian.wave#L128)

```wave
pub fun bytes_store_le_u64(dst: ptr<u8>, value: u64)
```

[Declaration](bytes/endian.wave#L139)

```wave
pub fun bytes_load_be_i16(src: ptr<u8>) -> i16
```

[Declaration](bytes/endian.wave#L140)

```wave
pub fun bytes_load_le_i16(src: ptr<u8>) -> i16
```

[Declaration](bytes/endian.wave#L141)

```wave
pub fun bytes_load_be_i32(src: ptr<u8>) -> i32
```

[Declaration](bytes/endian.wave#L142)

```wave
pub fun bytes_load_le_i32(src: ptr<u8>) -> i32
```

[Declaration](bytes/endian.wave#L143)

```wave
pub fun bytes_load_be_i64(src: ptr<u8>) -> i64
```

[Declaration](bytes/endian.wave#L144)

```wave
pub fun bytes_load_le_i64(src: ptr<u8>) -> i64
```

[Declaration](bytes/endian.wave#L146)

```wave
pub fun bytes_store_be_i16(dst: ptr<u8>, value: i16)
```

[Declaration](bytes/endian.wave#L147)

```wave
pub fun bytes_store_le_i16(dst: ptr<u8>, value: i16)
```

[Declaration](bytes/endian.wave#L148)

```wave
pub fun bytes_store_be_i32(dst: ptr<u8>, value: i32)
```

[Declaration](bytes/endian.wave#L149)

```wave
pub fun bytes_store_le_i32(dst: ptr<u8>, value: i32)
```

[Declaration](bytes/endian.wave#L150)

```wave
pub fun bytes_store_be_i64(dst: ptr<u8>, value: i64)
```

[Declaration](bytes/endian.wave#L151)

```wave
pub fun bytes_store_le_i64(dst: ptr<u8>, value: i64)
```

[Declaration](bytes/endian.wave#L153)

```wave
pub fun bytes_is_little_endian() -> bool
```

[Declaration](bytes/endian.wave#L159)

```wave
pub fun bytes_is_big_endian() -> bool
```

[Declaration](bytes/endian.wave#L161)

```wave
pub fun bytes_to_be_u16(value: u16) -> u16
```

[Declaration](bytes/endian.wave#L166)

```wave
pub fun bytes_from_be_u16(value: u16) -> u16
```

[Declaration](bytes/endian.wave#L168)

```wave
pub fun bytes_to_le_u16(value: u16) -> u16
```

[Declaration](bytes/endian.wave#L173)

```wave
pub fun bytes_from_le_u16(value: u16) -> u16
```

[Declaration](bytes/endian.wave#L175)

```wave
pub fun bytes_to_be_u32(value: u32) -> u32
```

[Declaration](bytes/endian.wave#L180)

```wave
pub fun bytes_from_be_u32(value: u32) -> u32
```

[Declaration](bytes/endian.wave#L182)

```wave
pub fun bytes_to_le_u32(value: u32) -> u32
```

[Declaration](bytes/endian.wave#L187)

```wave
pub fun bytes_from_le_u32(value: u32) -> u32
```

[Declaration](bytes/endian.wave#L189)

```wave
pub fun bytes_to_be_u64(value: u64) -> u64
```

[Declaration](bytes/endian.wave#L194)

```wave
pub fun bytes_from_be_u64(value: u64) -> u64
```

[Declaration](bytes/endian.wave#L196)

```wave
pub fun bytes_to_le_u64(value: u64) -> u64
```

[Declaration](bytes/endian.wave#L201)

```wave
pub fun bytes_from_le_u64(value: u64) -> u64
```

## `std::bytes::errors`

[Source](bytes/errors.wave)

[Declaration](bytes/errors.wave#L9)

```wave
pub const BYTES_OK: i32 = 0;
```

[Declaration](bytes/errors.wave#L10)

```wave
pub const BYTES_ERROR_INVALID: i32 = -1;
```

[Declaration](bytes/errors.wave#L11)

```wave
pub const BYTES_ERROR_EOF: i32 = -2;
```

[Declaration](bytes/errors.wave#L12)

```wave
pub const BYTES_ERROR_NO_SPACE: i32 = -3;
```

[Declaration](bytes/errors.wave#L13)

```wave
pub const BYTES_ERROR_OVERFLOW: i32 = -4;
```

[Declaration](bytes/errors.wave#L15)

```wave
pub fun bytes_error_is_ok(error: i32) -> bool
```

[Declaration](bytes/errors.wave#L19)

```wave
pub fun bytes_error_name(error: i32) -> str
```

## `std::bytes::hex`

[Source](bytes/hex.wave)

[Declaration](bytes/hex.wave#L6)

```wave
pub import("std::bytes::errors")::{
    BYTES_OK, BYTES_ERROR_INVALID, BYTES_ERROR_NO_SPACE, BYTES_ERROR_OVERFLOW,
};
```

[Declaration](bytes/hex.wave#L46)

```wave
pub fun bytes_hex_encode(src: Bytes, dst: BytesMut, out_len: ptr<i64>) -> i32
```

[Declaration](bytes/hex.wave#L73)

```wave
pub fun bytes_hex_decode(src: Bytes, dst: BytesMut, out_len: ptr<i64>) -> i32
```

## `std::bytes::leb128`

[Source](bytes/leb128.wave)

[Declaration](bytes/leb128.wave#L14)

```wave
pub fun bytes_reader_read_uleb128_u64(reader: ptr<ByteReader>, output_value: ptr<u64>) -> i32
```

[Declaration](bytes/leb128.wave#L45)

```wave
pub fun bytes_writer_write_uleb128_u64(writer: ptr<ByteWriter>, value: u64) -> i32
```

[Declaration](bytes/leb128.wave#L79)

```wave
pub fun bytes_reader_read_sleb128_i64(reader: ptr<ByteReader>, output_value: ptr<i64>) -> i32
```

[Declaration](bytes/leb128.wave#L108)

```wave
pub fun bytes_writer_write_sleb128_i64(writer: ptr<ByteWriter>, value: i64) -> i32
```

[Declaration](bytes/leb128.wave#L136)

```wave
pub fun bytes_uleb128_u64_len(value: u64) -> i64
```

[Declaration](bytes/leb128.wave#L143)

```wave
pub fun bytes_sleb128_i64_len(value: i64) -> i64
```

[Declaration](bytes/leb128.wave#L156)

```wave
pub fun bytes_reader_peek_uleb128_u64(reader: ptr<ByteReader>, output_value: ptr<u64>) -> i32
```

[Declaration](bytes/leb128.wave#L165)

```wave
pub fun bytes_reader_peek_sleb128_i64(reader: ptr<ByteReader>, output_value: ptr<i64>) -> i32
```

## `std::bytes::read`

[Source](bytes/read.wave)

[Declaration](bytes/read.wave#L6)

```wave
pub import("std::bytes::types")::{Bytes, BytesMut};
```

[Declaration](bytes/read.wave#L7)

```wave
pub import("std::bytes::errors")::{
    BYTES_OK,
    BYTES_ERROR_INVALID,
    BYTES_ERROR_EOF,
    BYTES_ERROR_NO_SPACE,
    BYTES_ERROR_OVERFLOW,
};
```

[Declaration](bytes/read.wave#L40)

```wave
pub fun bytes_read_u8(src: Bytes, offset: i64, out_value: ptr<u8>) -> i32
```

[Declaration](bytes/read.wave#L48)

```wave
pub fun bytes_read_be_u16(src: Bytes, offset: i64, out_value: ptr<u16>) -> i32
```

[Declaration](bytes/read.wave#L57)

```wave
pub fun bytes_read_le_u16(src: Bytes, offset: i64, out_value: ptr<u16>) -> i32
```

[Declaration](bytes/read.wave#L66)

```wave
pub fun bytes_read_be_u32(src: Bytes, offset: i64, out_value: ptr<u32>) -> i32
```

[Declaration](bytes/read.wave#L75)

```wave
pub fun bytes_read_le_u32(src: Bytes, offset: i64, out_value: ptr<u32>) -> i32
```

[Declaration](bytes/read.wave#L84)

```wave
pub fun bytes_read_be_u64(src: Bytes, offset: i64, out_value: ptr<u64>) -> i32
```

[Declaration](bytes/read.wave#L93)

```wave
pub fun bytes_read_le_u64(src: Bytes, offset: i64, out_value: ptr<u64>) -> i32
```

[Declaration](bytes/read.wave#L102)

```wave
pub fun bytes_read_be_i16(src: Bytes, offset: i64, out_value: ptr<i16>) -> i32
```

[Declaration](bytes/read.wave#L111)

```wave
pub fun bytes_read_le_i16(src: Bytes, offset: i64, out_value: ptr<i16>) -> i32
```

[Declaration](bytes/read.wave#L120)

```wave
pub fun bytes_read_be_i32(src: Bytes, offset: i64, out_value: ptr<i32>) -> i32
```

[Declaration](bytes/read.wave#L129)

```wave
pub fun bytes_read_le_i32(src: Bytes, offset: i64, out_value: ptr<i32>) -> i32
```

[Declaration](bytes/read.wave#L138)

```wave
pub fun bytes_read_be_i64(src: Bytes, offset: i64, out_value: ptr<i64>) -> i32
```

[Declaration](bytes/read.wave#L147)

```wave
pub fun bytes_read_le_i64(src: Bytes, offset: i64, out_value: ptr<i64>) -> i32
```

[Declaration](bytes/read.wave#L157)

```wave
pub fun bytes_read_exact(src: Bytes, offset: i64, dst: BytesMut) -> i32
```

[Declaration](bytes/read.wave#L172)

```wave
pub fun bytes_read_be_f32(src: Bytes, offset: i64, out_value: ptr<f32>) -> i32
```

[Declaration](bytes/read.wave#L186)

```wave
pub fun bytes_read_be_f64(src: Bytes, offset: i64, out_value: ptr<f64>) -> i32
```

[Declaration](bytes/read.wave#L200)

```wave
pub fun bytes_read_le_f32(src: Bytes, offset: i64, out_value: ptr<f32>) -> i32
```

[Declaration](bytes/read.wave#L214)

```wave
pub fun bytes_read_le_f64(src: Bytes, offset: i64, out_value: ptr<f64>) -> i32
```

## `std::bytes::search`

[Source](bytes/search.wave)

[Declaration](bytes/search.wave#L6)

```wave
pub import("std::bytes::errors")::{BYTES_OK, BYTES_ERROR_INVALID};
```

[Declaration](bytes/search.wave#L9)

```wave
pub fun bytes_equal(left: Bytes, right: Bytes, out_equal: ptr<bool>) -> i32
```

[Declaration](bytes/search.wave#L25)

```wave
pub fun bytes_find_byte(src: Bytes, value: u8, out_offset: ptr<i64>) -> i32
```

[Declaration](bytes/search.wave#L43)

```wave
pub fun bytes_find(src: Bytes, needle: Bytes, out_offset: ptr<i64>) -> i32
```

## `std::bytes::types`

[Source](bytes/types.wave)

[Declaration](bytes/types.wave#L8)

```wave
pub struct Bytes {
    data: ptr<u8>;
    len: i64;
}
```

[Declaration](bytes/types.wave#L13)

```wave
pub struct BytesMut {
    data: ptr<u8>;
    len: i64;
}
```

[Declaration](bytes/types.wave#L19)

```wave
pub struct ByteReader {
    data: ptr<u8>;
    len: i64;
    position: i64;
}
```

[Declaration](bytes/types.wave#L26)

```wave
pub struct ByteWriter {
    data: ptr<u8>;
    len: i64;
    position: i64;
}
```

[Declaration](bytes/types.wave#L32)

```wave
pub fun bytes_view(data: ptr<u8>, len: i64) -> Bytes
```

[Declaration](bytes/types.wave#L36)

```wave
pub fun bytes_view_mut(data: ptr<u8>, len: i64) -> BytesMut
```

[Declaration](bytes/types.wave#L40)

```wave
pub fun bytes_view_is_valid(bytes: Bytes) -> bool
```

[Declaration](bytes/types.wave#L46)

```wave
pub fun bytes_view_mut_is_valid(bytes: BytesMut) -> bool
```

[Declaration](bytes/types.wave#L55)

```wave
pub fun bytes_subview(src: Bytes, offset: i64, count: i64, output: ptr<Bytes>) -> i32
```

[Declaration](bytes/types.wave#L64)

```wave
pub fun bytes_subview_mut(src: BytesMut, offset: i64, count: i64, output: ptr<BytesMut>) -> i32
```

## `std::bytes::write`

[Source](bytes/write.wave)

[Declaration](bytes/write.wave#L6)

```wave
pub import("std::bytes::types")::{Bytes, BytesMut};
```

[Declaration](bytes/write.wave#L7)

```wave
pub import("std::bytes::errors")::{
    BYTES_OK,
    BYTES_ERROR_INVALID,
    BYTES_ERROR_EOF,
    BYTES_ERROR_NO_SPACE,
    BYTES_ERROR_OVERFLOW,
};
```

[Declaration](bytes/write.wave#L40)

```wave
pub fun bytes_write_u8(dst: BytesMut, offset: i64, value: u8) -> i32
```

[Declaration](bytes/write.wave#L47)

```wave
pub fun bytes_write_be_u16(dst: BytesMut, offset: i64, value: u16) -> i32
```

[Declaration](bytes/write.wave#L54)

```wave
pub fun bytes_write_le_u16(dst: BytesMut, offset: i64, value: u16) -> i32
```

[Declaration](bytes/write.wave#L61)

```wave
pub fun bytes_write_be_u32(dst: BytesMut, offset: i64, value: u32) -> i32
```

[Declaration](bytes/write.wave#L68)

```wave
pub fun bytes_write_le_u32(dst: BytesMut, offset: i64, value: u32) -> i32
```

[Declaration](bytes/write.wave#L75)

```wave
pub fun bytes_write_be_u64(dst: BytesMut, offset: i64, value: u64) -> i32
```

[Declaration](bytes/write.wave#L82)

```wave
pub fun bytes_write_le_u64(dst: BytesMut, offset: i64, value: u64) -> i32
```

[Declaration](bytes/write.wave#L89)

```wave
pub fun bytes_write_be_i16(dst: BytesMut, offset: i64, value: i16) -> i32
```

[Declaration](bytes/write.wave#L93)

```wave
pub fun bytes_write_le_i16(dst: BytesMut, offset: i64, value: i16) -> i32
```

[Declaration](bytes/write.wave#L97)

```wave
pub fun bytes_write_be_i32(dst: BytesMut, offset: i64, value: i32) -> i32
```

[Declaration](bytes/write.wave#L101)

```wave
pub fun bytes_write_le_i32(dst: BytesMut, offset: i64, value: i32) -> i32
```

[Declaration](bytes/write.wave#L105)

```wave
pub fun bytes_write_be_i64(dst: BytesMut, offset: i64, value: i64) -> i32
```

[Declaration](bytes/write.wave#L109)

```wave
pub fun bytes_write_le_i64(dst: BytesMut, offset: i64, value: i64) -> i32
```

[Declaration](bytes/write.wave#L114)

```wave
pub fun bytes_write_exact(dst: BytesMut, offset: i64, src: Bytes) -> i32
```

[Declaration](bytes/write.wave#L129)

```wave
pub fun bytes_write_be_f32(dst: BytesMut, offset: i64, value: f32) -> i32
```

[Declaration](bytes/write.wave#L134)

```wave
pub fun bytes_write_be_f64(dst: BytesMut, offset: i64, value: f64) -> i32
```

[Declaration](bytes/write.wave#L139)

```wave
pub fun bytes_write_le_f32(dst: BytesMut, offset: i64, value: f32) -> i32
```

[Declaration](bytes/write.wave#L144)

```wave
pub fun bytes_write_le_f64(dst: BytesMut, offset: i64, value: f64) -> i32
```

## `std::debug::core`

[Source](debug/core.wave)

[Declaration](debug/core.wave#L36)

```wave
pub fun debug_log(message: str)
```

[Declaration](debug/core.wave#L42)

```wave
pub fun debug_value<T>(label: str, value: T)
```

[Declaration](debug/core.wave#L47)

```wave
pub fun debug_fail(message: str) -> !
```

[Declaration](debug/core.wave#L54)

```wave
pub fun debug_assert(condition: bool, message: str)
```

## `std::env::consts`

[Source](env/consts.wave)

[Declaration](env/consts.wave#L19)

```wave
pub const ENV_ERR_NOT_FOUND: i64 = -2;
```

[Declaration](env/consts.wave#L20)

```wave
pub const ENV_ERR_NO_SPACE: i64 = -3;
```

[Declaration](env/consts.wave#L21)

```wave
pub const ENV_ERR_INVALID_KEY: i64 = -4;
```

[Declaration](env/consts.wave#L22)

```wave
pub const ENV_ERR_READ: i64 = -5;
```

[Declaration](env/consts.wave#L23)

```wave
pub const ENV_ERR_SOURCE_INCOMPLETE: i64 = -6;
```

[Declaration](env/consts.wave#L24)

```wave
pub const ENV_ERR_NO_MEMORY: i64 = -7;
```

[Declaration](env/consts.wave#L26)

```wave
pub const ENV_O_RDONLY: i32 = 0;
```

[Declaration](env/consts.wave#L27)

```wave
pub const ENV_SCAN_CAP: i64 = 32768;
```

## `std::env::cwd`

[Source](env/cwd.wave)

[Declaration](env/cwd.wave#L58)

```wave
pub fun env_getcwd(dst: ptr<u8>, cap: i64) -> i64
```

[Declaration](env/cwd.wave#L75)

```wave
pub fun env_chdir(path: str) -> i64
```

[Declaration](env/cwd.wave#L79)

```wave
pub fun env_access(path: str, mode: i32) -> i64
```

## `std::env::environ`

[Source](env/environ.wave)

[Declaration](env/environ.wave#L50)

```wave
pub struct EnvResult<T> {
    ok: bool;
    value: T;
}
```

[Declaration](env/environ.wave#L55)

```wave
pub fun env_result_ok<T>(value: T) -> EnvResult<T>
```

[Declaration](env/environ.wave#L62)

```wave
pub fun env_result_err<T>(fallback: T) -> EnvResult<T>
```

[Declaration](env/environ.wave#L69)

```wave
pub fun env_unwrap_or<T>(result: EnvResult<T>, default_value: T) -> T
```

[Declaration](env/environ.wave#L77)

```wave
pub fun env_get(name: str, dst: ptr<u8>, dst_cap: i64) -> i64
```

[Declaration](env/environ.wave#L146)

```wave
pub fun env_exists(name: str) -> bool
```

[Declaration](env/environ.wave#L154)

```wave
pub fun env_get_i64(name: str) -> EnvResult<i64>
```

[Declaration](env/environ.wave#L170)

```wave
pub fun env_get_i32(name: str) -> EnvResult<i32>
```

[Declaration](env/environ.wave#L190)

```wave
pub fun env_get_i32_default(name: str, default_value: i32) -> i32
```

[Declaration](env/environ.wave#L194)

```wave
pub fun env_get_i64_default(name: str, default_value: i64) -> i64
```

## `std::env::parse`

[Source](env/parse.wave)

[Declaration](env/parse.wave#L21)

```wave
pub fun _env_key_len(name: str) -> i64
```

[Declaration](env/parse.wave#L34)

```wave
pub fun _env_match_key(blob: ptr<u8>, start: i64, name: str, key_len: i64) -> bool
```

[Declaration](env/parse.wave#L49)

```wave
pub fun _env_copy_value(
    blob: ptr<u8>,
    begin: i64,
    end: i64,
    dst: ptr<u8>,
    dst_cap: i64
) -> i64
```

[Declaration](env/parse.wave#L74)

```wave
pub fun _env_parse_i64(raw: ptr<u8>, out_value: ptr<i64>) -> bool
```

## `std::fs::consts`

[Source](fs/consts.wave)

[Declaration](fs/consts.wave#L20)

```wave
pub const FS_MODE_USER_RW: i32 = 384;
```

[Declaration](fs/consts.wave#L21)

```wave
pub const FS_MODE_FILE_DEFAULT: i32 = 420;
```

[Declaration](fs/consts.wave#L22)

```wave
pub const FS_MODE_DIR_DEFAULT: i32 = 493;
```

## `std::fs::file`

[Source](fs/file.wave)

[Declaration](fs/file.wave#L99)

```wave
pub struct Metadata {
    size: i64;
    modified_seconds: i64;
    is_file: bool;
    is_directory: bool;
}
```

[Declaration](fs/file.wave#L116)

```wave
pub fun fs_exists(path: str) -> bool
```

[Declaration](fs/file.wave#L125)

```wave
pub fun fs_open(path: str, flags: i32, mode: i32) -> i64
```

[Declaration](fs/file.wave#L130)

```wave
pub fun fs_open_read(path: str) -> i64
```

[Declaration](fs/file.wave#L134)

```wave
pub fun fs_open_write(path: str, mode: i32) -> i64
```

[Declaration](fs/file.wave#L138)

```wave
pub fun fs_open_append(path: str, mode: i32) -> i64
```

[Declaration](fs/file.wave#L142)

```wave
pub fun fs_open_rw(path: str, mode: i32) -> i64
```

[Declaration](fs/file.wave#L146)

```wave
pub fun fs_close(fd: i64) -> i64
```

[Declaration](fs/file.wave#L151)

```wave
pub fun fs_file_size(path: str) -> i64
```

[Declaration](fs/file.wave#L172)

```wave
pub fun fs_file_size_fd(fd: i64) -> i64
```

[Declaration](fs/file.wave#L192)

```wave
pub fun fs_remove(path: str) -> i64
```

[Declaration](fs/file.wave#L196)

```wave
pub fun fs_mkdir(path: str, mode: i32) -> i64
```

[Declaration](fs/file.wave#L200)

```wave
pub fun fs_rmdir(path: str) -> i64
```

[Declaration](fs/file.wave#L204)

```wave
pub fun fs_rename(old_path: str, new_path: str) -> i64
```

[Declaration](fs/file.wave#L211)

```wave
pub fun fs_read_all(path: str, dst: ptr<u8>, dst_cap: i64) -> i64
```

[Declaration](fs/file.wave#L247)

```wave
pub fun fs_write_all(path: str, src: ptr<u8>, len: i64, mode: i32) -> i64
```

[Declaration](fs/file.wave#L272)

```wave
pub fun fs_append_all(path: str, src: ptr<u8>, len: i64, mode: i32) -> i64
```

[Declaration](fs/file.wave#L298)

```wave
pub fun fs_copy(
    src_path: str,
    dst_path: str,
    scratch: ptr<u8>,
    scratch_cap: i64,
    mode: i32
) -> i64
```

[Declaration](fs/file.wave#L364)

```wave
pub fun exists(path: str) -> bool
```

[Declaration](fs/file.wave#L370)

```wave
pub fun open_read(path: str) -> i64
```

[Declaration](fs/file.wave#L376)

```wave
pub fun create(path: str) -> i64
```

[Declaration](fs/file.wave#L381)

```wave
pub fun open_append(path: str) -> i64
```

[Declaration](fs/file.wave#L386)

```wave
pub fun open_read_write(path: str) -> i64
```

[Declaration](fs/file.wave#L392)

```wave
pub fun metadata(path: str, info: ptr<Metadata>) -> i64
```

[Declaration](fs/file.wave#L412)

```wave
pub fun size(path: str) -> i64
```

[Declaration](fs/file.wave#L424)

```wave
pub fun read_into(path: str, dst: ptr<u8>, dst_cap: i64) -> i64
```

[Declaration](fs/file.wave#L431)

```wave
pub fun read_to_end(path: str, dst_buffer: ptr<Buffer>) -> i64
```

[Declaration](fs/file.wave#L526)

```wave
pub fun write(path: str, src: ptr<u8>, len: i64) -> i64
```

[Declaration](fs/file.wave#L531)

```wave
pub fun append(path: str, src: ptr<u8>, len: i64) -> i64
```

[Declaration](fs/file.wave#L536)

```wave
pub fun copy(src_path: str, dst_path: str) -> i64
```

[Declaration](fs/file.wave#L548)

```wave
pub fun rename(old_path: str, new_path: str) -> i64
```

[Declaration](fs/file.wave#L553)

```wave
pub fun remove(path: str) -> i64
```

[Declaration](fs/file.wave#L558)

```wave
pub fun create_dir(path: str) -> i64
```

[Declaration](fs/file.wave#L563)

```wave
pub fun remove_dir(path: str) -> i64
```

## `std::io::consts`

[Source](io/consts.wave)

[Declaration](io/consts.wave#L19)

```wave
pub const IO_STDIN_FD: i64 = 0;
```

[Declaration](io/consts.wave#L20)

```wave
pub const IO_STDOUT_FD: i64 = 1;
```

[Declaration](io/consts.wave#L21)

```wave
pub const IO_STDERR_FD: i64 = 2;
```

[Declaration](io/consts.wave#L25)

```wave
pub const IO_ERR_INVALID: i64 = -4096;
```

[Declaration](io/consts.wave#L26)

```wave
pub const IO_ERR_EOF: i64 = -4097;
```

[Declaration](io/consts.wave#L27)

```wave
pub const IO_ERR_NO_SPACE: i64 = -4098;
```

[Declaration](io/consts.wave#L31)

```wave
pub const IO_ERR_INTR: i64 = SYS_ERR_INTR;
```

## `std::io::fd`

[Source](io/fd.wave)

[Declaration](io/fd.wave#L107)

```wave
pub fun io_close(fd: i64) -> i64
```

[Declaration](io/fd.wave#L114)

```wave
pub fun io_seek(fd: i64, offset: i64, whence: i32) -> i64
```

[Declaration](io/fd.wave#L123)

```wave
pub fun io_read(fd: i64, buf: ptr<u8>, len: i64) -> i64
```

[Declaration](io/fd.wave#L143)

```wave
pub fun io_write(fd: i64, buf: ptr<u8>, len: i64) -> i64
```

[Declaration](io/fd.wave#L165)

```wave
pub fun io_write_all(fd: i64, buf: ptr<u8>, len: i64) -> i64
```

[Declaration](io/fd.wave#L193)

```wave
pub fun io_read_exact(fd: i64, buf: ptr<u8>, len: i64) -> i64
```

[Declaration](io/fd.wave#L220)

```wave
pub fun io_read_at_most(fd: i64, dst: ptr<u8>, dst_cap: i64) -> i64
```

[Declaration](io/fd.wave#L248)

```wave
pub fun io_copy(src_fd: i64, dst_fd: i64, scratch: ptr<u8>, scratch_cap: i64) -> i64
```

[Declaration](io/fd.wave#L275)

```wave
pub fun io_pipe(out_fds: ptr<i32>) -> i64
```

[Declaration](io/fd.wave#L279)

```wave
pub fun io_dup(fd: i64) -> i64
```

[Declaration](io/fd.wave#L286)

```wave
pub fun io_dup2(oldfd: i64, newfd: i64) -> i64
```

## `std::math::bits`

[Source](math/bits.wave)

[Declaration](math/bits.wave#L28)

```wave
pub fun is_power_of_two_u32(x: u32) -> bool
```

[Declaration](math/bits.wave#L32)

```wave
pub fun is_power_of_two_u64(x: u64) -> bool
```

[Declaration](math/bits.wave#L36)

```wave
pub fun lowest_one_bit_u32(x: u32) -> u32
```

[Declaration](math/bits.wave#L40)

```wave
pub fun lowest_one_bit_u64(x: u64) -> u64
```

[Declaration](math/bits.wave#L44)

```wave
pub fun count_ones_u32(value: u32) -> i32
```

[Declaration](math/bits.wave#L54)

```wave
pub fun count_ones_u64(value: u64) -> i32
```

[Declaration](math/bits.wave#L64)

```wave
pub fun count_zeros_u32(value: u32) -> i32
```

[Declaration](math/bits.wave#L68)

```wave
pub fun count_zeros_u64(value: u64) -> i32
```

[Declaration](math/bits.wave#L72)

```wave
pub fun leading_zeros_u32(value: u32) -> i32
```

[Declaration](math/bits.wave#L86)

```wave
pub fun leading_zeros_u64(value: u64) -> i32
```

[Declaration](math/bits.wave#L100)

```wave
pub fun trailing_zeros_u32(value: u32) -> i32
```

[Declaration](math/bits.wave#L107)

```wave
pub fun trailing_zeros_u64(value: u64) -> i32
```

[Declaration](math/bits.wave#L114)

```wave
pub fun bit_length_u32(value: u32) -> i32
```

[Declaration](math/bits.wave#L118)

```wave
pub fun bit_length_u64(value: u64) -> i32
```

[Declaration](math/bits.wave#L122)

```wave
pub fun ilog2_floor_u32(value: u32) -> i32
```

[Declaration](math/bits.wave#L129)

```wave
pub fun ilog2_floor_u64(value: u64) -> i32
```

[Declaration](math/bits.wave#L136)

```wave
pub fun ilog2_ceil_u32(value: u32) -> i32
```

[Declaration](math/bits.wave#L146)

```wave
pub fun ilog2_ceil_u64(value: u64) -> i32
```

[Declaration](math/bits.wave#L156)

```wave
pub fun align_down_u32(value: u32, alignment: u32) -> MathResult<u32>
```

[Declaration](math/bits.wave#L163)

```wave
pub fun align_up_u32(value: u32, alignment: u32) -> MathResult<u32>
```

[Declaration](math/bits.wave#L173)

```wave
pub fun align_down_u64(value: u64, alignment: u64) -> MathResult<u64>
```

[Declaration](math/bits.wave#L180)

```wave
pub fun align_up_u64(value: u64, alignment: u64) -> MathResult<u64>
```

[Declaration](math/bits.wave#L190)

```wave
pub fun next_power_of_two_u32(value: u32) -> MathResult<u32>
```

[Declaration](math/bits.wave#L200)

```wave
pub fun next_power_of_two_u64(value: u64) -> MathResult<u64>
```

[Declaration](math/bits.wave#L210)

```wave
pub fun rotate_left_u32(value: u32, amount: u32) -> u32
```

[Declaration](math/bits.wave#L218)

```wave
pub fun rotate_right_u32(value: u32, amount: u32) -> u32
```

[Declaration](math/bits.wave#L226)

```wave
pub fun rotate_left_u64(value: u64, amount: u64) -> u64
```

[Declaration](math/bits.wave#L234)

```wave
pub fun rotate_right_u64(value: u64, amount: u64) -> u64
```

[Declaration](math/bits.wave#L242)

```wave
pub fun reverse_bits_u32(value: u32) -> u32
```

[Declaration](math/bits.wave#L254)

```wave
pub fun reverse_bits_u64(value: u64) -> u64
```

[Declaration](math/bits.wave#L267)

```wave
pub fun is_pow2(x: i32) -> bool
```

[Declaration](math/bits.wave#L271)

```wave
pub fun align_down(x: i32, align: i32) -> i32
```

[Declaration](math/bits.wave#L279)

```wave
pub fun align_up(x: i32, align: i32) -> i32
```

[Declaration](math/bits.wave#L290)

```wave
pub fun low_bit(x: i32) -> i32
```

[Declaration](math/bits.wave#L294)

```wave
pub fun popcount(x: i32) -> i32
```

[Declaration](math/bits.wave#L298)

```wave
pub fun ctz32(x: i32) -> i32
```

[Declaration](math/bits.wave#L302)

```wave
pub fun bit_length(x: i32) -> i32
```

[Declaration](math/bits.wave#L309)

```wave
pub fun ilog2_floor(x: i32) -> i32
```

[Declaration](math/bits.wave#L316)

```wave
pub fun ilog2_ceil(x: i32) -> i32
```

[Declaration](math/bits.wave#L323)

```wave
pub fun is_pow2_i64(x: i64) -> bool
```

[Declaration](math/bits.wave#L327)

```wave
pub fun align_down_i64(x: i64, align: i64) -> i64
```

[Declaration](math/bits.wave#L335)

```wave
pub fun align_up_i64(x: i64, align: i64) -> i64
```

[Declaration](math/bits.wave#L346)

```wave
pub fun low_bit_i64(x: i64) -> i64
```

[Declaration](math/bits.wave#L350)

```wave
pub fun popcount64(x: i64) -> i32
```

[Declaration](math/bits.wave#L354)

```wave
pub fun ctz64(x: i64) -> i32
```

[Declaration](math/bits.wave#L358)

```wave
pub fun bit_length64(x: i64) -> i32
```

[Declaration](math/bits.wave#L365)

```wave
pub fun ilog2_floor64(x: i64) -> i32
```

[Declaration](math/bits.wave#L372)

```wave
pub fun ilog2_ceil64(x: i64) -> i32
```

[Declaration](math/bits.wave#L380)

```wave
pub fun bswap32(x: i32) -> i32
```

[Declaration](math/bits.wave#L388)

```wave
pub fun bswap64(x: i64) -> i64
```

## `std::math::constants`

[Source](math/constants.wave)

[Declaration](math/constants.wave#L20)

```wave
pub const MATH_E_F32: f32 = 2.718281828459045;
```

[Declaration](math/constants.wave#L21)

```wave
pub const MATH_PI_F32: f32 = 3.141592653589793;
```

[Declaration](math/constants.wave#L22)

```wave
pub const MATH_TWO_PI_F32: f32 = 6.283185307179586;
```

[Declaration](math/constants.wave#L23)

```wave
pub const MATH_HALF_PI_F32: f32 = 1.5707963267948966;
```

[Declaration](math/constants.wave#L24)

```wave
pub const MATH_QUARTER_PI_F32: f32 = 0.7853981633974483;
```

[Declaration](math/constants.wave#L25)

```wave
pub const MATH_LN_2_F32: f32 = 0.6931471805599453;
```

[Declaration](math/constants.wave#L26)

```wave
pub const MATH_LN_10_F32: f32 = 2.302585092994046;
```

[Declaration](math/constants.wave#L27)

```wave
pub const MATH_SQRT_2_F32: f32 = 1.4142135623730951;
```

[Declaration](math/constants.wave#L28)

```wave
pub const MATH_EPSILON_F32: f32 = 0.00000011920928955078125;
```

[Declaration](math/constants.wave#L30)

```wave
pub const MATH_E_F64: f64 = 2.718281828459045;
```

[Declaration](math/constants.wave#L31)

```wave
pub const MATH_PI_F64: f64 = 3.141592653589793;
```

[Declaration](math/constants.wave#L32)

```wave
pub const MATH_TWO_PI_F64: f64 = 6.283185307179586;
```

[Declaration](math/constants.wave#L33)

```wave
pub const MATH_HALF_PI_F64: f64 = 1.5707963267948966;
```

[Declaration](math/constants.wave#L34)

```wave
pub const MATH_QUARTER_PI_F64: f64 = 0.7853981633974483;
```

[Declaration](math/constants.wave#L35)

```wave
pub const MATH_LN_2_F64: f64 = 0.6931471805599453;
```

[Declaration](math/constants.wave#L36)

```wave
pub const MATH_LN_10_F64: f64 = 2.302585092994046;
```

[Declaration](math/constants.wave#L37)

```wave
pub const MATH_SQRT_2_F64: f64 = 1.4142135623730951;
```

[Declaration](math/constants.wave#L38)

```wave
pub const MATH_EPSILON_F64: f64 = 0.0000000000000002220446049250313;
```

[Declaration](math/constants.wave#L40)

```wave
pub const MATH_I32_MIN: i32 = -2147483648;
```

[Declaration](math/constants.wave#L41)

```wave
pub const MATH_I32_MAX: i32 = 2147483647;
```

[Declaration](math/constants.wave#L42)

```wave
pub const MATH_I64_MIN: i64 = -9223372036854775808;
```

[Declaration](math/constants.wave#L43)

```wave
pub const MATH_I64_MAX: i64 = 9223372036854775807;
```

[Declaration](math/constants.wave#L44)

```wave
pub const MATH_U32_MAX: u32 = 4294967295;
```

[Declaration](math/constants.wave#L45)

```wave
pub const MATH_U64_MAX: u64 = 18446744073709551615;
```

## `std::math::float`

[Source](math/float.wave)

[Declaration](math/float.wave#L27)

```wave
pub fun float_to_bits_f32(value: f32) -> u32
```

[Declaration](math/float.wave#L33)

```wave
pub fun float_from_bits_f32(value: u32) -> f32
```

[Declaration](math/float.wave#L39)

```wave
pub fun float_to_bits_f64(value: f64) -> u64
```

[Declaration](math/float.wave#L45)

```wave
pub fun float_from_bits_f64(value: u64) -> f64
```

[Declaration](math/float.wave#L51)

```wave
pub fun nan_f32() -> f32
```

[Declaration](math/float.wave#L55)

```wave
pub fun nan_f64() -> f64
```

[Declaration](math/float.wave#L59)

```wave
pub fun infinity_f32() -> f32
```

[Declaration](math/float.wave#L63)

```wave
pub fun infinity_f64() -> f64
```

[Declaration](math/float.wave#L67)

```wave
pub fun is_nan_f32(value: f32) -> bool
```

[Declaration](math/float.wave#L73)

```wave
pub fun is_nan_f64(value: f64) -> bool
```

[Declaration](math/float.wave#L79)

```wave
pub fun is_infinite_f32(value: f32) -> bool
```

[Declaration](math/float.wave#L83)

```wave
pub fun is_infinite_f64(value: f64) -> bool
```

[Declaration](math/float.wave#L88)

```wave
pub fun is_finite_f32(value: f32) -> bool
```

[Declaration](math/float.wave#L92)

```wave
pub fun is_finite_f64(value: f64) -> bool
```

[Declaration](math/float.wave#L97)

```wave
pub fun is_normal_f32(value: f32) -> bool
```

[Declaration](math/float.wave#L102)

```wave
pub fun is_normal_f64(value: f64) -> bool
```

[Declaration](math/float.wave#L107)

```wave
pub fun is_subnormal_f32(value: f32) -> bool
```

[Declaration](math/float.wave#L112)

```wave
pub fun is_subnormal_f64(value: f64) -> bool
```

[Declaration](math/float.wave#L118)

```wave
pub fun sign_bit_f32(value: f32) -> bool
```

[Declaration](math/float.wave#L122)

```wave
pub fun sign_bit_f64(value: f64) -> bool
```

[Declaration](math/float.wave#L126)

```wave
pub fun abs_f32(value: f32) -> f32
```

[Declaration](math/float.wave#L130)

```wave
pub fun abs_f64(value: f64) -> f64
```

[Declaration](math/float.wave#L134)

```wave
pub fun copy_sign_f32(magnitude: f32, sign: f32) -> f32
```

[Declaration](math/float.wave#L140)

```wave
pub fun copy_sign_f64(magnitude: f64, sign: f64) -> f64
```

[Declaration](math/float.wave#L146)

```wave
pub fun min_f32(a: f32, b: f32) -> f32
```

[Declaration](math/float.wave#L157)

```wave
pub fun min_f64(a: f64, b: f64) -> f64
```

[Declaration](math/float.wave#L168)

```wave
pub fun max_f32(a: f32, b: f32) -> f32
```

[Declaration](math/float.wave#L179)

```wave
pub fun max_f64(a: f64, b: f64) -> f64
```

[Declaration](math/float.wave#L190)

```wave
pub fun clamp_f32(value: f32, low: f32, high: f32) -> f32
```

[Declaration](math/float.wave#L195)

```wave
pub fun clamp_f64(value: f64, low: f64, high: f64) -> f64
```

[Declaration](math/float.wave#L200)

```wave
pub fun clamp_f32_checked(value: f32, low: f32, high: f32) -> MathResult<f32>
```

[Declaration](math/float.wave#L207)

```wave
pub fun clamp_f64_checked(value: f64, low: f64, high: f64) -> MathResult<f64>
```

[Declaration](math/float.wave#L214)

```wave
pub fun trunc_f32(value: f32) -> f32
```

[Declaration](math/float.wave#L227)

```wave
pub fun trunc_f64(value: f64) -> f64
```

[Declaration](math/float.wave#L240)

```wave
pub fun floor_f32(value: f32) -> f32
```

[Declaration](math/float.wave#L246)

```wave
pub fun floor_f64(value: f64) -> f64
```

[Declaration](math/float.wave#L252)

```wave
pub fun ceil_f32(value: f32) -> f32
```

[Declaration](math/float.wave#L258)

```wave
pub fun ceil_f64(value: f64) -> f64
```

[Declaration](math/float.wave#L264)

```wave
pub fun round_f32(value: f32) -> f32
```

[Declaration](math/float.wave#L274)

```wave
pub fun round_f64(value: f64) -> f64
```

[Declaration](math/float.wave#L284)

```wave
pub fun fract_f32(value: f32) -> f32
```

[Declaration](math/float.wave#L288)

```wave
pub fun fract_f64(value: f64) -> f64
```

[Declaration](math/float.wave#L292)

```wave
pub fun approx_eq_f32(a: f32, b: f32, absolute_tolerance: f32, relative_tolerance: f32) -> bool
```

[Declaration](math/float.wave#L316)

```wave
pub fun approx_eq_f64(a: f64, b: f64, absolute_tolerance: f64, relative_tolerance: f64) -> bool
```

[Declaration](math/float.wave#L340)

```wave
pub fun sqrt_f32(value: f32) -> f32
```

[Declaration](math/float.wave#L358)

```wave
pub fun sqrt_f64(value: f64) -> f64
```

[Declaration](math/float.wave#L376)

```wave
pub fun hypot_f32(a: f32, b: f32) -> f32
```

[Declaration](math/float.wave#L386)

```wave
pub fun hypot_f64(a: f64, b: f64) -> f64
```

[Declaration](math/float.wave#L396)

```wave
pub fun lerp_f32(a: f32, b: f32, amount: f32) -> f32
```

[Declaration](math/float.wave#L420)

```wave
pub fun lerp_f64(a: f64, b: f64, amount: f64) -> f64
```

## `std::math::int`

[Source](math/int.wave)

[Declaration](math/int.wave#L36)

```wave
pub fun num_abs<T>(x: T, zero: T) -> T
```

[Declaration](math/int.wave#L44)

```wave
pub fun num_min<T>(a: T, b: T) -> T
```

[Declaration](math/int.wave#L52)

```wave
pub fun num_max<T>(a: T, b: T) -> T
```

[Declaration](math/int.wave#L60)

```wave
pub fun num_clamp<T>(x: T, lo: T, hi: T) -> T
```

[Declaration](math/int.wave#L73)

```wave
pub fun ptr_swap<T>(a: ptr<T>, b: ptr<T>) -> bool
```

[Declaration](math/int.wave#L84)

```wave
pub fun abs_i32(x: i32) -> i32
```

[Declaration](math/int.wave#L88)

```wave
pub fun abs_i64(x: i64) -> i64
```

[Declaration](math/int.wave#L92)

```wave
pub fun abs_i32_checked(x: i32) -> MathResult<i32>
```

[Declaration](math/int.wave#L100)

```wave
pub fun abs_i64_checked(x: i64) -> MathResult<i64>
```

[Declaration](math/int.wave#L108)

```wave
pub fun abs_i32_saturating(x: i32) -> i32
```

[Declaration](math/int.wave#L116)

```wave
pub fun abs_i64_saturating(x: i64) -> i64
```

[Declaration](math/int.wave#L124)

```wave
pub fun min_i32(a: i32, b: i32) -> i32
```

[Declaration](math/int.wave#L128)

```wave
pub fun min_i64(a: i64, b: i64) -> i64
```

[Declaration](math/int.wave#L132)

```wave
pub fun max_i32(a: i32, b: i32) -> i32
```

[Declaration](math/int.wave#L136)

```wave
pub fun max_i64(a: i64, b: i64) -> i64
```

[Declaration](math/int.wave#L140)

```wave
pub fun clamp_i32(x: i32, lo: i32, hi: i32) -> i32
```

[Declaration](math/int.wave#L144)

```wave
pub fun clamp_i64(x: i64, lo: i64, hi: i64) -> i64
```

[Declaration](math/int.wave#L148)

```wave
pub fun clamp_i32_checked(x: i32, lo: i32, hi: i32) -> MathResult<i32>
```

[Declaration](math/int.wave#L156)

```wave
pub fun clamp_i64_checked(x: i64, lo: i64, hi: i64) -> MathResult<i64>
```

[Declaration](math/int.wave#L164)

```wave
pub fun sign_i32(x: i32) -> i32
```

[Declaration](math/int.wave#L176)

```wave
pub fun sign_i64(x: i64) -> i32
```

[Declaration](math/int.wave#L188)

```wave
pub fun is_even_i32(x: i32) -> bool
```

[Declaration](math/int.wave#L192)

```wave
pub fun is_odd_i32(x: i32) -> bool
```

[Declaration](math/int.wave#L196)

```wave
pub fun is_even_i64(x: i64) -> bool
```

[Declaration](math/int.wave#L200)

```wave
pub fun is_odd_i64(x: i64) -> bool
```

[Declaration](math/int.wave#L204)

```wave
pub fun div_floor_i32_checked(a: i32, b: i32) -> MathResult<i32>
```

[Declaration](math/int.wave#L222)

```wave
pub fun div_ceil_i32_checked(a: i32, b: i32) -> MathResult<i32>
```

[Declaration](math/int.wave#L240)

```wave
pub fun div_floor_i64_checked(a: i64, b: i64) -> MathResult<i64>
```

[Declaration](math/int.wave#L258)

```wave
pub fun div_ceil_i64_checked(a: i64, b: i64) -> MathResult<i64>
```

[Declaration](math/int.wave#L277)

```wave
pub fun abs(x: i32) -> i32
```

[Declaration](math/int.wave#L281)

```wave
pub fun min(a: i32, b: i32) -> i32
```

[Declaration](math/int.wave#L285)

```wave
pub fun max(a: i32, b: i32) -> i32
```

[Declaration](math/int.wave#L289)

```wave
pub fun clamp(x: i32, lo: i32, hi: i32) -> i32
```

[Declaration](math/int.wave#L293)

```wave
pub fun sign(x: i32) -> i32
```

[Declaration](math/int.wave#L297)

```wave
pub fun is_even(x: i32) -> bool
```

[Declaration](math/int.wave#L301)

```wave
pub fun is_odd(x: i32) -> bool
```

[Declaration](math/int.wave#L305)

```wave
pub fun div_ceil_pos(a: i32, b: i32) -> i32
```

[Declaration](math/int.wave#L321)

```wave
pub fun div_floor_pos(a: i32, b: i32) -> i32
```

[Declaration](math/int.wave#L333)

```wave
pub fun swap_i32(a: ptr<i32>, b: ptr<i32>)
```

## `std::math::num`

[Source](math/num.wave)

[Declaration](math/num.wave#L46)

```wave
pub fun gcd_u32(left: u32, right: u32) -> u32
```

[Declaration](math/num.wave#L57)

```wave
pub fun gcd_u64(left: u64, right: u64) -> u64
```

[Declaration](math/num.wave#L68)

```wave
pub fun gcd_i32_checked(left: i32, right: i32) -> MathResult<i32>
```

[Declaration](math/num.wave#L76)

```wave
pub fun gcd_i64_checked(left: i64, right: i64) -> MathResult<i64>
```

[Declaration](math/num.wave#L84)

```wave
pub fun lcm_u32_checked(left: u32, right: u32) -> MathResult<u32>
```

[Declaration](math/num.wave#L97)

```wave
pub fun lcm_u64_checked(left: u64, right: u64) -> MathResult<u64>
```

[Declaration](math/num.wave#L110)

```wave
pub fun lcm_i32_checked(left: i32, right: i32) -> MathResult<i32>
```

[Declaration](math/num.wave#L118)

```wave
pub fun lcm_i64_checked(left: i64, right: i64) -> MathResult<i64>
```

[Declaration](math/num.wave#L126)

```wave
pub fun pow_i32_wrapping(base_value: i32, exponent_value: u32) -> i32
```

[Declaration](math/num.wave#L142)

```wave
pub fun pow_i64_wrapping(base_value: i64, exponent_value: u64) -> i64
```

[Declaration](math/num.wave#L158)

```wave
pub fun pow_i32_checked(base_value: i32, exponent_value: u32) -> MathResult<i32>
```

[Declaration](math/num.wave#L180)

```wave
pub fun pow_f32_i32(base_value: f32, exponent_value: i32) -> f32
```

[Declaration](math/num.wave#L203)

```wave
pub fun pow_f64_i32(base_value: f64, exponent_value: i32) -> f64
```

[Declaration](math/num.wave#L227)

```wave
pub fun gcd(a: i32, b: i32) -> i32
```

[Declaration](math/num.wave#L232)

```wave
pub fun lcm(a: i32, b: i32) -> i32
```

[Declaration](math/num.wave#L237)

```wave
pub fun pow_i32(base: i32, exponent: i32) -> i32
```

## `std::math::result`

[Source](math/result.wave)

[Declaration](math/result.wave#L19)

```wave
pub const MATH_ERROR_NONE: i32 = 0;
```

[Declaration](math/result.wave#L20)

```wave
pub const MATH_ERROR_DOMAIN: i32 = -1;
```

[Declaration](math/result.wave#L21)

```wave
pub const MATH_ERROR_DIVIDE_BY_ZERO: i32 = -2;
```

[Declaration](math/result.wave#L22)

```wave
pub const MATH_ERROR_OVERFLOW: i32 = -3;
```

[Declaration](math/result.wave#L23)

```wave
pub const MATH_ERROR_INVALID_ARGUMENT: i32 = -4;
```

[Declaration](math/result.wave#L26)

```wave
pub struct MathResult<T> {
    value: T;
    error: i32;
}
```

[Declaration](math/result.wave#L31)

```wave
pub fun math_result_ok<T>(value: T) -> MathResult<T>
```

[Declaration](math/result.wave#L35)

```wave
pub fun math_result_error<T>(value: T, error: i32) -> MathResult<T>
```

[Declaration](math/result.wave#L39)

```wave
pub fun math_result_is_ok<T>(result: MathResult<T>) -> bool
```

## `std::math::trig`

[Source](math/trig.wave)

[Declaration](math/trig.wave#L19)

```wave
pub import("std::math::constants")::{
    MATH_PI_F32,
    MATH_TWO_PI_F32,
    MATH_HALF_PI_F32,
    MATH_QUARTER_PI_F32,
    MATH_PI_F64,
    MATH_TWO_PI_F64,
    MATH_HALF_PI_F64,
    MATH_QUARTER_PI_F64,
};
```

[Declaration](math/trig.wave#L29)

```wave
pub import("std::math::float")::{
    abs_f32,
    abs_f64,
    sqrt_f32,
    sqrt_f64,
    is_finite_f64,
    nan_f64,
};
```

[Declaration](math/trig.wave#L40)

```wave
pub struct SinCosF32 {
    sin: f32;
    cos: f32;
}
```

[Declaration](math/trig.wave#L45)

```wave
pub struct SinCosF64 {
    sin: f64;
    cos: f64;
}
```

[Declaration](math/trig.wave#L51)

```wave
pub fun wrap_angle_pi_f64(value: f64) -> f64
```

[Declaration](math/trig.wave#L65)

```wave
pub fun wrap_angle_pi_f32(value: f32) -> f32
```

[Declaration](math/trig.wave#L104)

```wave
pub fun sin_f64(value: f64) -> f64
```

[Declaration](math/trig.wave#L108)

```wave
pub fun cos_f64(value: f64) -> f64
```

[Declaration](math/trig.wave#L112)

```wave
pub fun tan_f64(value: f64) -> f64
```

[Declaration](math/trig.wave#L117)

```wave
pub fun sin_f32(value: f32) -> f32
```

[Declaration](math/trig.wave#L121)

```wave
pub fun cos_f32(value: f32) -> f32
```

[Declaration](math/trig.wave#L125)

```wave
pub fun tan_f32(value: f32) -> f32
```

[Declaration](math/trig.wave#L129)

```wave
pub fun sin_cos_f64(value: f64) -> SinCosF64
```

[Declaration](math/trig.wave#L146)

```wave
pub fun sin_cos_f32(value: f32) -> SinCosF32
```

[Declaration](math/trig.wave#L151)

```wave
pub fun radians_f64(degrees: f64) -> f64
```

[Declaration](math/trig.wave#L155)

```wave
pub fun degrees_f64(radians: f64) -> f64
```

[Declaration](math/trig.wave#L159)

```wave
pub fun radians_f32(degrees: f32) -> f32
```

[Declaration](math/trig.wave#L163)

```wave
pub fun degrees_f32(radians: f32) -> f32
```

## `std::math::trig_reduction`

[Source](math/trig_reduction.wave)

[Declaration](math/trig_reduction.wave#L27)

```wave
pub struct ReducedAngle { quadrant: i32; remainder: f64; }
```

[Declaration](math/trig_reduction.wave#L30)

```wave
pub fun reduce_angle_f64(value: f64) -> ReducedAngle
```

## `std::mem::alloc`

[Source](mem/alloc.wave)

[Declaration](mem/alloc.wave#L38)

```wave
pub fun mem_alloc(size: i64) -> ptr<u8>
```

[Declaration](mem/alloc.wave#L42)

```wave
pub fun mem_alloc_zeroed(size: i64) -> ptr<u8>
```

[Declaration](mem/alloc.wave#L52)

```wave
pub fun mem_free(p: ptr<u8>, size: i64) -> i64
```

[Declaration](mem/alloc.wave#L64)

```wave
pub fun mem_realloc(old_ptr: ptr<u8>, old_size: i64, new_size: i64) -> ptr<u8>
```

[Declaration](mem/alloc.wave#L106)

```wave
pub fun mem_alloc_items<T>(count: i64, elem_size: i64) -> ptr<T>
```

[Declaration](mem/alloc.wave#L119)

```wave
pub fun mem_alloc_items_zeroed<T>(count: i64, elem_size: i64) -> ptr<T>
```

[Declaration](mem/alloc.wave#L132)

```wave
pub fun mem_realloc_items<T>(
    old_ptr: ptr<T>,
    old_count: i64,
    new_count: i64,
    elem_size: i64
) -> ptr<T>
```

[Declaration](mem/alloc.wave#L159)

```wave
pub fun mem_free_items<T>(p: ptr<T>, count: i64, elem_size: i64) -> i64
```

[Declaration](mem/alloc.wave#L177)

```wave
pub fun mem_page_size() -> i64
```

[Declaration](mem/alloc.wave#L183)

```wave
pub fun mem_pages_for_size(size: i64) -> i64
```

[Declaration](mem/alloc.wave#L201)

```wave
pub fun mem_size_align_page(size: i64) -> i64
```

[Declaration](mem/alloc.wave#L215)

```wave
pub fun mem_alloc_pages(page_count: i64) -> ptr<u8>
```

[Declaration](mem/alloc.wave#L228)

```wave
pub fun mem_free_pages(p: ptr<u8>, page_count: i64) -> i64
```

[Declaration](mem/alloc.wave#L246)

```wave
pub fun mem_is_aligned(p: ptr<u8>, align: i64) -> bool
```

[Declaration](mem/alloc.wave#L262)

```wave
pub fun mem_alloc_aligned(size: i64, align: i64) -> ptr<u8>
```

[Declaration](mem/alloc.wave#L309)

```wave
pub fun mem_free_aligned(p: ptr<u8>) -> i64
```

## `std::mem::consts`

[Source](mem/consts.wave)

[Declaration](mem/consts.wave#L19)

```wave
pub const MEM_ERR_INVALID: i64 = -4096;
```

[Declaration](mem/consts.wave#L20)

```wave
pub const MEM_ERR_NO_SPACE: i64 = -4097;
```

[Declaration](mem/consts.wave#L21)

```wave
pub const MEM_ERR_OVERFLOW: i64 = -4098;
```

[Declaration](mem/consts.wave#L22)

```wave
pub const MEM_ERR_ALLOC: i64 = -4099;
```

[Declaration](mem/consts.wave#L26)

```wave
pub const MEM_MAX_SIZE: i64 = 9223372036854775807;
```

## `std::mem::cstr`

[Source](mem/cstr.wave)

[Declaration](mem/cstr.wave#L22)

```wave
pub fun mem_len_cstr(s: str) -> i64
```

[Declaration](mem/cstr.wave#L33)

```wave
pub fun mem_len_cstr_n(s: str, max_len: i64) -> i64
```

[Declaration](mem/cstr.wave#L51)

```wave
pub fun mem_copy_cstr(dst: ptr<u8>, s: str) -> i64
```

[Declaration](mem/cstr.wave#L67)

```wave
pub fun mem_copy_cstr_n(dst: ptr<u8>, dst_cap: i64, s: str) -> i64
```

[Declaration](mem/cstr.wave#L90)

```wave
pub fun mem_eq_cstr(a: str, b: str) -> bool
```

[Declaration](mem/cstr.wave#L105)

```wave
pub fun mem_starts_with_cstr(s: str, prefix: str) -> bool
```

[Declaration](mem/cstr.wave#L116)

```wave
pub fun mem_find_cstr_char(s: str, c: u8) -> i64
```

## `std::mem::layout`

[Source](mem/layout.wave)

[Declaration](mem/layout.wave#L8)

```wave
pub fun size_of<T>() -> u64
```

[Declaration](mem/layout.wave#L13)

```wave
pub fun align_of<T>() -> u64
```

## `std::mem::ops`

[Source](mem/ops.wave)

[Declaration](mem/ops.wave#L30)

```wave
pub fun mem_size_add_checked(a: i64, b: i64, out_value: ptr<i64>) -> i64
```

[Declaration](mem/ops.wave#L44)

```wave
pub fun mem_size_mul_checked(a: i64, b: i64, out_value: ptr<i64>) -> i64
```

[Declaration](mem/ops.wave#L59)

```wave
pub fun mem_set(dst: ptr<u8>, value: u8, size: i64)
```

[Declaration](mem/ops.wave#L71)

```wave
pub fun mem_zero(dst: ptr<u8>, size: i64)
```

[Declaration](mem/ops.wave#L75)

```wave
pub fun mem_copy(dst: ptr<u8>, src: ptr<u8>, size: i64)
```

[Declaration](mem/ops.wave#L87)

```wave
pub fun mem_move(dst: ptr<u8>, src: ptr<u8>, size: i64)
```

[Declaration](mem/ops.wave#L114)

```wave
pub fun mem_cmp(a: ptr<u8>, b: ptr<u8>, size: i64) -> i32
```

[Declaration](mem/ops.wave#L147)

```wave
pub fun mem_eq(a: ptr<u8>, b: ptr<u8>, size: i64) -> bool
```

[Declaration](mem/ops.wave#L155)

```wave
pub fun mem_find_byte(src: ptr<u8>, size: i64, value: u8) -> i64
```

[Declaration](mem/ops.wave#L170)

```wave
pub fun mem_swap<T>(a: ptr<T>, b: ptr<T>)
```

[Declaration](mem/ops.wave#L180)

```wave
pub fun mem_copy_items<T>(dst: ptr<T>, src: ptr<T>, count: i64, elem_size: i64)
```

[Declaration](mem/ops.wave#L193)

```wave
pub fun mem_set_items<T>(dst: ptr<T>, value: T, count: i64, elem_size: i64)
```

[Declaration](mem/ops.wave#L210)

```wave
pub fun mem_move_items<T>(dst: ptr<T>, src: ptr<T>, count: i64, elem_size: i64)
```

[Declaration](mem/ops.wave#L223)

```wave
pub fun mem_zero_items<T>(dst: ptr<T>, count: i64, elem_size: i64)
```

[Declaration](mem/ops.wave#L236)

```wave
pub fun mem_copy_checked(dst: ptr<u8>, dst_cap: i64, src: ptr<u8>, src_len: i64) -> i64
```

[Declaration](mem/ops.wave#L257)

```wave
pub fun mem_move_checked(dst: ptr<u8>, dst_cap: i64, src: ptr<u8>, src_len: i64) -> i64
```

[Declaration](mem/ops.wave#L278)

```wave
pub fun mem_set_checked(dst: ptr<u8>, dst_cap: i64, value: u8, len: i64) -> i64
```

[Declaration](mem/ops.wave#L299)

```wave
pub fun mem_zero_checked(dst: ptr<u8>, dst_cap: i64, len: i64) -> i64
```

[Declaration](mem/ops.wave#L304)

```wave
pub fun mem_cmp_checked(
    a: ptr<u8>,
    a_len: i64,
    b: ptr<u8>,
    b_len: i64,
    out_order: ptr<i32>
) -> i64
```

[Declaration](mem/ops.wave#L354)

```wave
pub fun mem_copy_items_checked<T>(dst: ptr<T>, src: ptr<T>, count: i64) -> i64
```

[Declaration](mem/ops.wave#L360)

```wave
pub fun mem_move_items_checked<T>(dst: ptr<T>, src: ptr<T>, count: i64) -> i64
```

[Declaration](mem/ops.wave#L366)

```wave
pub fun mem_zero_items_checked<T>(dst: ptr<T>, count: i64) -> i64
```

## `std::net::address`

[Source](net/address.wave)

[Declaration](net/address.wave#L9)

```wave
pub const NET_ADDRESS_IPV4: i32 = 4;
```

[Declaration](net/address.wave#L10)

```wave
pub const NET_ADDRESS_IPV6: i32 = 6;
```

[Declaration](net/address.wave#L11)

```wave
pub const NET_ADDRESS_TEXT_INVALID: i64 = -22;
```

[Declaration](net/address.wave#L12)

```wave
pub const NET_ADDRESS_TEXT_NO_SPACE: i64 = -28;
```

[Declaration](net/address.wave#L14)

```wave
pub struct Ipv4Addr {
    value: u32;
}
```

[Declaration](net/address.wave#L18)

```wave
pub struct Ipv6Addr {
    octets: array<u8, 16>;
}
```

[Declaration](net/address.wave#L22)

```wave
pub struct SocketAddrV4 {
    ip: Ipv4Addr;
    port: u16;
}
```

[Declaration](net/address.wave#L27)

```wave
pub struct SocketAddrV6 {
    ip: Ipv6Addr;
    port: u16;
    flowinfo: u32;
    scope_id: u32;
}
```

[Declaration](net/address.wave#L36)

```wave
pub struct IpAddr {
    family: i32;
    octets: array<u8, 16>;
}
```

[Declaration](net/address.wave#L41)

```wave
pub struct SocketAddr {
    ip: IpAddr;
    port: u16;
    flowinfo: u32;
    scope_id: u32;
}
```

[Declaration](net/address.wave#L48)

```wave
pub struct Ipv4ParseResult {
    ok: bool;
    value: Ipv4Addr;
}
```

[Declaration](net/address.wave#L53)

```wave
pub struct Ipv6ParseResult {
    ok: bool;
    value: Ipv6Addr;
}
```

[Declaration](net/address.wave#L58)

```wave
pub struct IpAddrV4Result {
    ok: bool;
    value: Ipv4Addr;
}
```

[Declaration](net/address.wave#L63)

```wave
pub struct IpAddrV6Result {
    ok: bool;
    value: Ipv6Addr;
}
```

[Declaration](net/address.wave#L68)

```wave
pub struct SocketAddrV4Result {
    ok: bool;
    value: SocketAddrV4;
}
```

[Declaration](net/address.wave#L73)

```wave
pub struct SocketAddrV6Result {
    ok: bool;
    value: SocketAddrV6;
}
```

[Declaration](net/address.wave#L78)

```wave
pub fun net_hton_u16(value: u16) -> u16
```

[Declaration](net/address.wave#L82)

```wave
pub fun net_ntoh_u16(value: u16) -> u16
```

[Declaration](net/address.wave#L86)

```wave
pub fun net_hton_u32(value: u32) -> u32
```

[Declaration](net/address.wave#L93)

```wave
pub fun net_ntoh_u32(value: u32) -> u32
```

[Declaration](net/address.wave#L99)

```wave
pub fun net_htons(value: i16) -> i16
```

[Declaration](net/address.wave#L103)

```wave
pub fun net_ntohs(value: i16) -> i16
```

[Declaration](net/address.wave#L107)

```wave
pub fun net_htonl(value: i32) -> i32
```

[Declaration](net/address.wave#L111)

```wave
pub fun net_ntohl(value: i32) -> i32
```

[Declaration](net/address.wave#L115)

```wave
pub fun ipv4_from_u32(value: u32) -> Ipv4Addr
```

[Declaration](net/address.wave#L119)

```wave
pub fun ipv4_to_u32(value: Ipv4Addr) -> u32
```

[Declaration](net/address.wave#L123)

```wave
pub fun ipv4_addr(a: u8, b: u8, c: u8, d: u8) -> Ipv4Addr
```

[Declaration](net/address.wave#L131)

```wave
pub fun ipv4_unspecified() -> Ipv4Addr
```

[Declaration](net/address.wave#L135)

```wave
pub fun ipv4_loopback() -> Ipv4Addr
```

[Declaration](net/address.wave#L139)

```wave
pub fun ipv4_broadcast() -> Ipv4Addr
```

[Declaration](net/address.wave#L143)

```wave
pub fun ipv4_octet(value: Ipv4Addr, index: i32) -> u8
```

[Declaration](net/address.wave#L151)

```wave
pub fun socket_addr_v4(ip: Ipv4Addr, port: u16) -> SocketAddrV4
```

[Declaration](net/address.wave#L155)

```wave
pub fun socket_addr_v4_any(port: u16) -> SocketAddrV4
```

[Declaration](net/address.wave#L159)

```wave
pub fun socket_addr_v4_loopback(port: u16) -> SocketAddrV4
```

[Declaration](net/address.wave#L163)

```wave
pub fun ipv6_unspecified() -> Ipv6Addr
```

[Declaration](net/address.wave#L173)

```wave
pub fun ipv6_loopback() -> Ipv6Addr
```

[Declaration](net/address.wave#L179)

```wave
pub fun ipv6_from_octets(octets: ptr<u8>) -> Ipv6Addr
```

[Declaration](net/address.wave#L196)

```wave
pub fun ipv6_parse(text: str) -> Ipv6ParseResult
```

[Declaration](net/address.wave#L312)

```wave
pub fun socket_addr_v6(
    ip: Ipv6Addr,
    port: u16,
    flowinfo: u32,
    scope_id: u32
) -> SocketAddrV6
```

[Declaration](net/address.wave#L326)

```wave
pub fun socket_addr_v6_any(port: u16) -> SocketAddrV6
```

[Declaration](net/address.wave#L330)

```wave
pub fun socket_addr_v6_loopback(port: u16) -> SocketAddrV6
```

[Declaration](net/address.wave#L334)

```wave
pub fun ip_addr_from_v4(ip: Ipv4Addr) -> IpAddr
```

[Declaration](net/address.wave#L349)

```wave
pub fun ip_addr_from_v6(ip: Ipv6Addr) -> IpAddr
```

[Declaration](net/address.wave#L360)

```wave
pub fun socket_addr_from_v4(addr: SocketAddrV4) -> SocketAddr
```

[Declaration](net/address.wave#L369)

```wave
pub fun socket_addr_from_v6(addr: SocketAddrV6) -> SocketAddr
```

[Declaration](net/address.wave#L378)

```wave
pub fun ip_addr_as_v4(ip: IpAddr) -> IpAddrV4Result
```

[Declaration](net/address.wave#L388)

```wave
pub fun ip_addr_as_v6(ip: IpAddr) -> IpAddrV6Result
```

[Declaration](net/address.wave#L401)

```wave
pub fun socket_addr_as_v4(addr: SocketAddr) -> SocketAddrV4Result
```

[Declaration](net/address.wave#L415)

```wave
pub fun socket_addr_as_v6(addr: SocketAddr) -> SocketAddrV6Result
```

[Declaration](net/address.wave#L429)

```wave
pub fun ipv4_parse(text: str) -> Ipv4ParseResult
```

[Declaration](net/address.wave#L524)

```wave
pub fun ipv4_format(ip: Ipv4Addr, dst: ptr<u8>, cap: i64) -> i64
```

[Declaration](net/address.wave#L550)

```wave
pub fun socket_addr_v4_format(addr: SocketAddrV4, dst: ptr<u8>, cap: i64) -> i64
```

[Declaration](net/address.wave#L568)

```wave
pub fun ipv6_format(ip: Ipv6Addr, dst: ptr<u8>, cap: i64) -> i64
```

[Declaration](net/address.wave#L617)

```wave
pub fun socket_addr_v6_format(addr: SocketAddrV6, dst: ptr<u8>, cap: i64) -> i64
```

[Declaration](net/address.wave#L646)

```wave
pub struct NetAddrV4 {
    ip: u32;
    port: u16;
}
```

[Declaration](net/address.wave#L651)

```wave
pub fun net_addr_v4(ip_host_order: u32, port_host_order: u16) -> NetAddrV4
```

[Declaration](net/address.wave#L658)

```wave
pub fun net_addr_any_v4(port_host_order: u16) -> NetAddrV4
```

[Declaration](net/address.wave#L662)

```wave
pub fun net_addr_loopback_v4(port_host_order: u16) -> NetAddrV4
```

[Declaration](net/address.wave#L666)

```wave
pub fun net_addr_to_socket_v4(addr: NetAddrV4) -> SocketAddrV4
```

[Declaration](net/address.wave#L673)

```wave
pub fun socket_addr_to_net_v4(addr: SocketAddrV4) -> NetAddrV4
```

## `std::net::error`

[Source](net/error.wave)

[Declaration](net/error.wave#L9)

```wave
pub const NET_ERROR_NONE: i32 = 0;
```

[Declaration](net/error.wave#L10)

```wave
pub const NET_ERROR_INVALID: i32 = 1;
```

[Declaration](net/error.wave#L11)

```wave
pub const NET_ERROR_INTERRUPTED: i32 = 2;
```

[Declaration](net/error.wave#L12)

```wave
pub const NET_ERROR_WOULD_BLOCK: i32 = 3;
```

[Declaration](net/error.wave#L13)

```wave
pub const NET_ERROR_TIMED_OUT: i32 = 4;
```

[Declaration](net/error.wave#L14)

```wave
pub const NET_ERROR_REFUSED: i32 = 5;
```

[Declaration](net/error.wave#L15)

```wave
pub const NET_ERROR_RESET: i32 = 6;
```

[Declaration](net/error.wave#L16)

```wave
pub const NET_ERROR_ADDRESS_IN_USE: i32 = 7;
```

[Declaration](net/error.wave#L17)

```wave
pub const NET_ERROR_NOT_CONNECTED: i32 = 8;
```

[Declaration](net/error.wave#L18)

```wave
pub const NET_ERROR_UNSUPPORTED: i32 = 9;
```

[Declaration](net/error.wave#L19)

```wave
pub const NET_ERROR_EOF: i32 = 10;
```

[Declaration](net/error.wave#L20)

```wave
pub const NET_ERROR_RESOLUTION: i32 = 11;
```

[Declaration](net/error.wave#L21)

```wave
pub const NET_ERROR_OTHER: i32 = 12;
```

[Declaration](net/error.wave#L23)

```wave
pub struct NetError {
    kind: i32;
    native_code: i64;
}
```

[Declaration](net/error.wave#L28)

```wave
pub struct NetResult<T> {
    ok: bool;
    value: T;
    error: NetError;
}
```

[Declaration](net/error.wave#L34)

```wave
pub struct NetIoResult {
    count: i64;
    eof: bool;
    error: NetError;
}
```

[Declaration](net/error.wave#L40)

```wave
pub fun net_error_none() -> NetError
```

[Declaration](net/error.wave#L44)

```wave
pub fun net_error(kind: i32, native_code: i64) -> NetError
```

[Declaration](net/error.wave#L48)

```wave
pub fun net_error_from_native(native_code: i64) -> NetError
```

[Declaration](net/error.wave#L150)

```wave
pub fun net_result_ok<T>(value: T) -> NetResult<T>
```

[Declaration](net/error.wave#L158)

```wave
pub fun net_result_err<T>(fallback: T, error: NetError) -> NetResult<T>
```

[Declaration](net/error.wave#L166)

```wave
pub fun net_io_from_native(value: i64) -> NetIoResult
```

[Declaration](net/error.wave#L190)

```wave
pub fun net_io_ok(count: i64) -> NetIoResult
```

[Declaration](net/error.wave#L194)

```wave
pub fun net_io_eof(count: i64) -> NetIoResult
```

[Declaration](net/error.wave#L202)

```wave
pub fun net_io_error(count: i64, native_code: i64) -> NetIoResult
```

[Declaration](net/error.wave#L211)

```wave
#[target(os="linux")]
pub const NET_NATIVE_TIMEOUT: i64 = -110;
```

[Declaration](net/error.wave#L213)

```wave
#[target(os="linux")]
pub fun net_connect_pending(code: i64) -> bool
```

[Declaration](net/error.wave#L217)

```wave
#[target(os="macos")]
pub const NET_NATIVE_TIMEOUT: i64 = -60;
```

[Declaration](net/error.wave#L219)

```wave
#[target(os="macos")]
pub fun net_connect_pending(code: i64) -> bool
```

[Declaration](net/error.wave#L223)

```wave
#[target(os="freebsd")]
pub const NET_NATIVE_TIMEOUT: i64 = -60;
```

[Declaration](net/error.wave#L225)

```wave
#[target(os="freebsd")]
pub fun net_connect_pending(code: i64) -> bool
```

[Declaration](net/error.wave#L229)

```wave
#[target(os="windows")]
pub const NET_NATIVE_TIMEOUT: i64 = -10060;
```

[Declaration](net/error.wave#L231)

```wave
#[target(os="windows")]
pub fun net_connect_pending(code: i64) -> bool
```

[Declaration](net/error.wave#L235)

```wave
#[target(os="none")]
pub const NET_NATIVE_TIMEOUT: i64 = -110;
```

[Declaration](net/error.wave#L237)

```wave
#[target(os="none")]
pub fun net_connect_pending(code: i64) -> bool
```

[Declaration](net/error.wave#L241)

```wave
#[target(os="unknown")]
pub const NET_NATIVE_TIMEOUT: i64 = -110;
```

[Declaration](net/error.wave#L243)

```wave
#[target(os="unknown")]
pub fun net_connect_pending(code: i64) -> bool
```

[Declaration](net/error.wave#L247)

```wave
#[target(os="wasi")]
pub const NET_NATIVE_TIMEOUT: i64 = -73;
```

[Declaration](net/error.wave#L249)

```wave
#[target(os="wasi")]
pub fun net_connect_pending(code: i64) -> bool
```

## `std::net::event`

[Source](net/event.wave)

[Declaration](net/event.wave#L14)

```wave
pub const NET_EVENT_BACKEND_EPOLL: i32 = 1;
```

[Declaration](net/event.wave#L15)

```wave
pub const NET_EVENT_BACKEND_KQUEUE: i32 = 2;
```

[Declaration](net/event.wave#L16)

```wave
pub const NET_EVENT_BACKEND_POLL: i32 = 3;
```

[Declaration](net/event.wave#L17)

```wave
pub const NET_EVENT_BACKEND_IOCP: i32 = 4;
```

[Declaration](net/event.wave#L18)

```wave
pub const NET_EVENT_READABLE: i32 = EVENT_READABLE;
```

[Declaration](net/event.wave#L19)

```wave
pub const NET_EVENT_WRITABLE: i32 = EVENT_WRITABLE;
```

[Declaration](net/event.wave#L21)

```wave
pub struct NetEventLoop { handle: i64; backend: i32; }
```

[Declaration](net/event.wave#L22)

```wave
pub struct NetEvent {
    fd: i64;
    token: u64;
    readable: bool;
    writable: bool;
    error: bool;
    hangup: bool;
}
```

[Declaration](net/event.wave#L30)

```wave
pub struct NetEventWaitResult { count: i32; error: NetError; }
```

[Declaration](net/event.wave#L36)

```wave
pub fun net_event_loop_create(capacity: i32) -> NetResult<NetEventLoop>
```

[Declaration](net/event.wave#L48)

```wave
pub fun net_event_add(
    loop: NetEventLoop, fd: i64, token: u64, flags: i32
) -> NetError
```

[Declaration](net/event.wave#L53)

```wave
pub fun net_event_modify(
    loop: NetEventLoop, fd: i64, token: u64, flags: i32
) -> NetError
```

[Declaration](net/event.wave#L58)

```wave
pub fun net_event_remove(loop: NetEventLoop, fd: i64) -> NetError
```

[Declaration](net/event.wave#L61)

```wave
pub fun net_event_wait(
    loop: NetEventLoop, output: ptr<NetEvent>, capacity: i32, timeout_ms: i32
) -> NetEventWaitResult
```

[Declaration](net/event.wave#L87)

```wave
pub fun net_event_loop_close(loop: NetEventLoop) -> NetError
```

## `std::net::interface`

[Source](net/interface.wave)

[Declaration](net/interface.wave#L12)

```wave
pub const NET_INTERFACE_NAME_CAPACITY: i32 = 64;
```

[Declaration](net/interface.wave#L14)

```wave
pub struct NetworkInterface {
    index: u32;
    flags: u32;
    name: array<u8, 64>;
    address: SocketAddr;
}
```

[Declaration](net/interface.wave#L21)

```wave
pub struct NetworkInterfacesResult {
    ok: bool;
    count: i64;
    written: i64;
    truncated: bool;
    error: NetError;
}
```

[Declaration](net/interface.wave#L40)

```wave
pub fun net_interfaces(
    output: ptr<NetworkInterface>, capacity: i64
) -> NetworkInterfacesResult
```

## `std::net::poll`

[Source](net/poll.wave)

[Declaration](net/poll.wave#L12)

```wave
pub import("std::sys::socket")::{PollFd};
```

[Declaration](net/poll.wave#L14)

```wave
pub const NET_POLLIN: i16 = POLLIN;
```

[Declaration](net/poll.wave#L15)

```wave
pub const NET_POLLOUT: i16 = POLLOUT;
```

[Declaration](net/poll.wave#L16)

```wave
pub const NET_POLLERR: i16 = POLLERR;
```

[Declaration](net/poll.wave#L17)

```wave
pub const NET_POLLHUP: i16 = POLLHUP;
```

[Declaration](net/poll.wave#L18)

```wave
pub const NET_POLLNVAL: i16 = POLLNVAL;
```

[Declaration](net/poll.wave#L20)

```wave
pub fun net_poll(fds: ptr<PollFd>, count: i64, timeout_ms: i32) -> i64
```

[Declaration](net/poll.wave#L53)

```wave
pub fun net_wait_readable(fd: i64, timeout_ms: i32) -> i64
```

[Declaration](net/poll.wave#L70)

```wave
pub fun net_wait_writable(fd: i64, timeout_ms: i32) -> i64
```

[Declaration](net/poll.wave#L86)

```wave
pub fun net_socket_error(fd: i64) -> i64
```

[Declaration](net/poll.wave#L95)

```wave
pub fun net_wait_connected(fd: i64, timeout_ms: i32) -> i64
```

## `std::net::posix_timeout`

[Source](net/posix_timeout.wave)

[Declaration](net/posix_timeout.wave#L23)

```wave
#[target(os="linux")]
pub fun net_io_timeout(fd: i64, buffer: ptr<u8>, size: i64, timeout_ms: i32, writing: bool) -> NetIoResult
```

[Declaration](net/posix_timeout.wave#L27)

```wave
#[target(os="macos")]
pub fun net_io_timeout(fd: i64, buffer: ptr<u8>, size: i64, timeout_ms: i32, writing: bool) -> NetIoResult
```

[Declaration](net/posix_timeout.wave#L31)

```wave
#[target(os="freebsd")]
pub fun net_io_timeout(fd: i64, buffer: ptr<u8>, size: i64, timeout_ms: i32, writing: bool) -> NetIoResult
```

## `std::net::resolve`

[Source](net/resolve.wave)

[Declaration](net/resolve.wave#L18)

```wave
pub const NET_RESOLVE_ERROR_NONE: i32 = 0;
```

[Declaration](net/resolve.wave#L19)

```wave
pub const NET_RESOLVE_ERROR_INVALID: i32 = 1;
```

[Declaration](net/resolve.wave#L20)

```wave
pub const NET_RESOLVE_ERROR_NOT_FOUND: i32 = 2;
```

[Declaration](net/resolve.wave#L21)

```wave
pub const NET_RESOLVE_ERROR_TEMPORARY: i32 = 3;
```

[Declaration](net/resolve.wave#L22)

```wave
pub const NET_RESOLVE_ERROR_UNSUPPORTED: i32 = 4;
```

[Declaration](net/resolve.wave#L23)

```wave
pub const NET_RESOLVE_ERROR_NO_MEMORY: i32 = 5;
```

[Declaration](net/resolve.wave#L24)

```wave
pub const NET_RESOLVE_ERROR_SYSTEM: i32 = 6;
```

[Declaration](net/resolve.wave#L25)

```wave
pub const NET_RESOLVE_ERROR_OTHER: i32 = 7;
```

[Declaration](net/resolve.wave#L27)

```wave
pub struct NetResolveError {
    kind: i32;
    native_code: i64;
}
```

[Declaration](net/resolve.wave#L32)

```wave
pub struct NetResolveResult {
    ok: bool;
    count: i64;
    written: i64;
    truncated: bool;
    error: NetResolveError;
}
```

[Declaration](net/resolve.wave#L199)

```wave
pub fun net_resolve_host(
    host: str, service: str, output: ptr<SocketAddr>, capacity: i64
) -> NetResolveResult
```

[Declaration](net/resolve.wave#L205)

```wave
pub fun net_resolve_tcp(
    host: str, service: str, output: ptr<SocketAddr>, capacity: i64
) -> NetResolveResult
```

[Declaration](net/resolve.wave#L211)

```wave
pub fun net_resolve_udp(
    host: str, service: str, output: ptr<SocketAddr>, capacity: i64
) -> NetResolveResult
```

## `std::net::resolve_table`

[Source](net/resolve_table.wave)

[Declaration](net/resolve_table.wave#L12)

```wave
pub const RESOLVE_ANY_PROTOCOL: i32 = 0;
```

[Declaration](net/resolve_table.wave#L13)

```wave
pub const RESOLVE_TCP: i32 = 6;
```

[Declaration](net/resolve_table.wave#L14)

```wave
pub const RESOLVE_UDP: i32 = 17;
```

[Declaration](net/resolve_table.wave#L16)

```wave
pub struct NetResolveEntry {
    host: str;
    service: str;
    protocol: i32;
    address: SocketAddr;
}
```

[Declaration](net/resolve_table.wave#L55)

```wave
pub fun net_resolve_from_table(
    host: str, service: str, protocol: i32,
    entries: ptr<NetResolveEntry>, entry_count: i64,
    output: ptr<SocketAddr>, capacity: i64
) -> NetResolveResult
```

## `std::net::socket_base`

[Source](net/socket_base.wave)

[Declaration](net/socket_base.wave#L34)

```wave
#[target(os="windows")]
pub fun net_fd_valid(fd: i64) -> bool
```

[Declaration](net/socket_base.wave#L36)

```wave
#[target(os="linux")]
pub fun net_fd_valid(fd: i64) -> bool
```

[Declaration](net/socket_base.wave#L40)

```wave
#[target(os="macos")]
pub fun net_fd_valid(fd: i64) -> bool
```

[Declaration](net/socket_base.wave#L44)

```wave
#[target(os="freebsd")]
pub fun net_fd_valid(fd: i64) -> bool
```

[Declaration](net/socket_base.wave#L51)

```wave
pub fun net_set_reuseaddr(fd: i64) -> i64
```

[Declaration](net/socket_base.wave#L83)

```wave
pub fun net_socket_tcp_v4() -> i64
```

[Declaration](net/socket_base.wave#L84)

```wave
pub fun net_socket_tcp_v6() -> i64
```

[Declaration](net/socket_base.wave#L85)

```wave
pub fun net_socket_udp_v4() -> i64
```

[Declaration](net/socket_base.wave#L86)

```wave
pub fun net_socket_udp_v6() -> i64
```

[Declaration](net/socket_base.wave#L123)

```wave
pub fun net_decode_native_v4(address: ptr<i8>) -> SocketAddrV4
```

[Declaration](net/socket_base.wave#L128)

```wave
pub fun net_bind_v4(fd: i64, addr: NetAddrV4) -> i64
```

[Declaration](net/socket_base.wave#L136)

```wave
pub fun net_bind_socket_v4(fd: i64, addr: SocketAddrV4) -> i64
```

[Declaration](net/socket_base.wave#L143)

```wave
pub fun net_connect_v4(fd: i64, addr: NetAddrV4) -> i64
```

[Declaration](net/socket_base.wave#L151)

```wave
pub fun net_connect_socket_v4(fd: i64, addr: SocketAddrV4) -> i64
```

[Declaration](net/socket_base.wave#L158)

```wave
pub fun net_accept_v4(fd: i64, out_addr: ptr<NetAddrV4>) -> i64
```

[Declaration](net/socket_base.wave#L174)

```wave
pub fun net_accept_socket_v4(fd: i64, out_addr: ptr<SocketAddrV4>) -> i64
```

[Declaration](net/socket_base.wave#L205)

```wave
pub fun net_decode_native_v6(address: ptr<i8>) -> SocketAddrV6
```

[Declaration](net/socket_base.wave#L210)

```wave
pub fun net_bind_v6(fd: i64, addr: SocketAddrV6) -> i64
```

[Declaration](net/socket_base.wave#L218)

```wave
pub fun net_connect_v6(fd: i64, addr: SocketAddrV6) -> i64
```

[Declaration](net/socket_base.wave#L226)

```wave
pub fun net_accept_v6(fd: i64, out_addr: ptr<SocketAddrV6>) -> i64
```

[Declaration](net/socket_base.wave#L242)

```wave
pub fun net_local_addr_v4(fd: i64, out_addr: ptr<SocketAddrV4>) -> i64
```

[Declaration](net/socket_base.wave#L259)

```wave
pub fun net_peer_addr_v4(fd: i64, out_addr: ptr<SocketAddrV4>) -> i64
```

[Declaration](net/socket_base.wave#L276)

```wave
pub fun net_local_addr_v6(fd: i64, out_addr: ptr<SocketAddrV6>) -> i64
```

[Declaration](net/socket_base.wave#L293)

```wave
pub fun net_peer_addr_v6(fd: i64, out_addr: ptr<SocketAddrV6>) -> i64
```

[Declaration](net/socket_base.wave#L319)

```wave
pub fun net_send_all(fd: i64, buf: ptr<u8>, len: i64, flags: i32) -> i64
```

[Declaration](net/socket_base.wave#L341)

```wave
pub fun net_send_all_io(fd: i64, buf: ptr<u8>, len: i64, flags: i32) -> NetIoResult
```

[Declaration](net/socket_base.wave#L365)

```wave
pub fun net_send_once(fd: i64, buf: ptr<u8>, len: i64, flags: i32) -> i64
```

[Declaration](net/socket_base.wave#L376)

```wave
pub fun net_recv_once(fd: i64, buf: ptr<u8>, len: i64, flags: i32) -> i64
```

[Declaration](net/socket_base.wave#L387)

```wave
pub fun net_recv_exact(fd: i64, buf: ptr<u8>, len: i64, flags: i32) -> i64
```

[Declaration](net/socket_base.wave#L412)

```wave
pub fun net_recv_exact_io(fd: i64, buf: ptr<u8>, len: i64, flags: i32) -> NetIoResult
```

[Declaration](net/socket_base.wave#L433)

```wave
pub fun net_sendto_v4(
    fd: i64, addr: NetAddrV4, buf: ptr<u8>, len: i64, flags: i32
) -> i64
```

[Declaration](net/socket_base.wave#L450)

```wave
pub fun net_recvfrom_v4(
    fd: i64, buf: ptr<u8>, len: i64, flags: i32, out_addr: ptr<NetAddrV4>
) -> i64
```

[Declaration](net/socket_base.wave#L472)

```wave
pub fun net_sendto_v6(
    fd: i64, addr: SocketAddrV6, buf: ptr<u8>, len: i64, flags: i32
) -> i64
```

[Declaration](net/socket_base.wave#L489)

```wave
pub fun net_recvfrom_v6(
    fd: i64, buf: ptr<u8>, len: i64, flags: i32, out_addr: ptr<SocketAddrV6>
) -> i64
```

[Declaration](net/socket_base.wave#L511)

```wave
pub fun net_close(fd: i64) -> i64
```

[Declaration](net/socket_base.wave#L518)

```wave
pub fun net_shutdown(fd: i64, how: i32) -> i64
```

[Declaration](net/socket_base.wave#L526)

```wave
pub fun net_shutdown_close(fd: i64)
```

## `std::net::socketopt`

[Source](net/socketopt.wave)

[Declaration](net/socketopt.wave#L33)

```wave
#[target(os="linux")]
pub fun net_set_nonblock(fd: i64, enabled: i32) -> i64
```

[Declaration](net/socketopt.wave#L40)

```wave
#[target(os="macos")]
pub fun net_set_nonblock(fd: i64, enabled: i32) -> i64
```

[Declaration](net/socketopt.wave#L47)

```wave
#[target(os="freebsd")]
pub fun net_set_nonblock(fd: i64, enabled: i32) -> i64
```

[Declaration](net/socketopt.wave#L54)

```wave
#[target(os="windows")]
pub fun net_set_nonblock(fd: i64, enabled: i32) -> i64
```

[Declaration](net/socketopt.wave#L90)

```wave
#[target(os="linux")]
pub fun net_get_nonblock(fd: i64) -> i64
```

[Declaration](net/socketopt.wave#L97)

```wave
#[target(os="macos")]
pub fun net_get_nonblock(fd: i64) -> i64
```

[Declaration](net/socketopt.wave#L104)

```wave
#[target(os="freebsd")]
pub fun net_get_nonblock(fd: i64) -> i64
```

[Declaration](net/socketopt.wave#L111)

```wave
#[target(os="windows")]
pub fun net_get_nonblock(fd: i64) -> i64
```

[Declaration](net/socketopt.wave#L140)

```wave
pub fun net_set_reuseaddr_flag(fd: i64, enabled: i32) -> i64
```

[Declaration](net/socketopt.wave#L149)

```wave
pub fun net_get_reuseaddr_flag(fd: i64) -> i64
```

[Declaration](net/socketopt.wave#L174)

```wave
pub fun net_set_tcp_nodelay(fd: i64, enabled: bool) -> i64
```

[Declaration](net/socketopt.wave#L181)

```wave
pub fun net_get_tcp_nodelay(fd: i64) -> i64
```

[Declaration](net/socketopt.wave#L188)

```wave
pub fun net_set_keepalive(fd: i64, enabled: bool) -> i64
```

[Declaration](net/socketopt.wave#L195)

```wave
pub fun net_get_keepalive(fd: i64) -> i64
```

[Declaration](net/socketopt.wave#L202)

```wave
pub fun net_set_recv_buffer(fd: i64, size: i32) -> i64
```

[Declaration](net/socketopt.wave#L210)

```wave
pub fun net_get_recv_buffer(fd: i64) -> i64
```

[Declaration](net/socketopt.wave#L217)

```wave
pub fun net_set_send_buffer(fd: i64, size: i32) -> i64
```

[Declaration](net/socketopt.wave#L225)

```wave
pub fun net_get_send_buffer(fd: i64) -> i64
```

[Declaration](net/socketopt.wave#L232)

```wave
pub fun net_set_broadcast(fd: i64, enabled: bool) -> i64
```

[Declaration](net/socketopt.wave#L239)

```wave
pub fun net_get_broadcast(fd: i64) -> i64
```

[Declaration](net/socketopt.wave#L246)

```wave
pub fun net_set_ipv4_ttl(fd: i64, ttl: i32) -> i64
```

[Declaration](net/socketopt.wave#L254)

```wave
pub fun net_get_ipv4_ttl(fd: i64) -> i64
```

[Declaration](net/socketopt.wave#L261)

```wave
pub struct NetIpv4Membership {
    group: u32;
    interface: u32;
}
```

[Declaration](net/socketopt.wave#L266)

```wave
pub struct NetIpv6Membership {
    group: array<u8, 16>;
    interface_index: u32;
}
```

[Declaration](net/socketopt.wave#L291)

```wave
pub fun net_join_multicast_v4(fd: i64, group: Ipv4Addr, interface: Ipv4Addr) -> i64
```

[Declaration](net/socketopt.wave#L305)

```wave
pub fun net_leave_multicast_v4(fd: i64, group: Ipv4Addr, interface: Ipv4Addr) -> i64
```

[Declaration](net/socketopt.wave#L319)

```wave
pub fun net_join_multicast_v6(fd: i64, group: Ipv6Addr, interface_index: u32) -> i64
```

[Declaration](net/socketopt.wave#L328)

```wave
pub fun net_leave_multicast_v6(fd: i64, group: Ipv6Addr, interface_index: u32) -> i64
```

[Declaration](net/socketopt.wave#L381)

```wave
pub fun net_set_multicast_interface_v4(fd: i64, interface: Ipv4Addr) -> i64
```

[Declaration](net/socketopt.wave#L389)

```wave
pub fun net_get_multicast_interface_v4(fd: i64) -> i64
```

[Declaration](net/socketopt.wave#L400)

```wave
pub fun net_set_multicast_ttl_v4(fd: i64, ttl: i32) -> i64
```

[Declaration](net/socketopt.wave#L408)

```wave
pub fun net_get_multicast_ttl_v4(fd: i64) -> i64
```

[Declaration](net/socketopt.wave#L415)

```wave
pub fun net_set_multicast_loop_v4(fd: i64, enabled: bool) -> i64
```

[Declaration](net/socketopt.wave#L422)

```wave
pub fun net_get_multicast_loop_v4(fd: i64) -> i64
```

[Declaration](net/socketopt.wave#L429)

```wave
pub fun net_set_multicast_interface_v6(fd: i64, interface_index: u32) -> i64
```

[Declaration](net/socketopt.wave#L436)

```wave
pub fun net_get_multicast_interface_v6(fd: i64) -> i64
```

[Declaration](net/socketopt.wave#L443)

```wave
pub fun net_set_multicast_hops_v6(fd: i64, hops: i32) -> i64
```

[Declaration](net/socketopt.wave#L451)

```wave
pub fun net_get_multicast_hops_v6(fd: i64) -> i64
```

[Declaration](net/socketopt.wave#L458)

```wave
pub fun net_set_multicast_loop_v6(fd: i64, enabled: bool) -> i64
```

[Declaration](net/socketopt.wave#L465)

```wave
pub fun net_get_multicast_loop_v6(fd: i64) -> i64
```

[Declaration](net/socketopt.wave#L473)

```wave
#[target(os="linux")]
pub fun net_set_reuseport_flag(fd: i64, enabled: i32) -> i64
```

[Declaration](net/socketopt.wave#L480)

```wave
#[target(os="macos")]
pub fun net_set_reuseport_flag(fd: i64, enabled: i32) -> i64
```

[Declaration](net/socketopt.wave#L487)

```wave
#[target(os="freebsd")]
pub fun net_set_reuseport_flag(fd: i64, enabled: i32) -> i64
```

[Declaration](net/socketopt.wave#L494)

```wave
#[target(os="windows")]
pub fun net_set_reuseport_flag(fd: i64, enabled: i32) -> i64
```

## `std::net::tcp`

[Source](net/tcp.wave)

[Declaration](net/tcp.wave#L54)

```wave
pub struct TcpListener {
    fd: i64;
}
```

[Declaration](net/tcp.wave#L58)

```wave
pub struct TcpStream {
    fd: i64;
}
```

[Declaration](net/tcp.wave#L87)

```wave
pub fun tcp_listen_v4(addr: SocketAddrV4, backlog: i32) -> NetResult<TcpListener>
```

[Declaration](net/tcp.wave#L104)

```wave
pub fun tcp_listen_v6(addr: SocketAddrV6, backlog: i32) -> NetResult<TcpListener>
```

[Declaration](net/tcp.wave#L121)

```wave
pub fun tcp_bind(port: u16) -> NetResult<TcpListener>
```

[Declaration](net/tcp.wave#L125)

```wave
pub fun tcp_bind_loopback(port: u16) -> NetResult<TcpListener>
```

[Declaration](net/tcp.wave#L129)

```wave
pub fun tcp_bind_v6(port: u16) -> NetResult<TcpListener>
```

[Declaration](net/tcp.wave#L133)

```wave
pub fun tcp_bind_v6_loopback(port: u16) -> NetResult<TcpListener>
```

[Declaration](net/tcp.wave#L137)

```wave
pub fun tcp_accept(listener: TcpListener) -> NetResult<TcpStream>
```

[Declaration](net/tcp.wave#L143)

```wave
pub fun tcp_accept_from_v4(
    listener: TcpListener, peer: ptr<SocketAddrV4>
) -> NetResult<TcpStream>
```

[Declaration](net/tcp.wave#L151)

```wave
pub fun tcp_accept_from_v6(
    listener: TcpListener, peer: ptr<SocketAddrV6>
) -> NetResult<TcpStream>
```

[Declaration](net/tcp.wave#L159)

```wave
pub fun tcp_connect(addr: SocketAddrV4) -> NetResult<TcpStream>
```

[Declaration](net/tcp.wave#L170)

```wave
pub fun tcp_connect_v6(addr: SocketAddrV6) -> NetResult<TcpStream>
```

[Declaration](net/tcp.wave#L181)

```wave
pub fun tcp_connect_addr(addr: SocketAddr) -> NetResult<TcpStream>
```

[Declaration](net/tcp.wave#L227)

```wave
pub fun tcp_connect_timeout(
    addr: SocketAddrV4, timeout_ms: i32
) -> NetResult<TcpStream>
```

[Declaration](net/tcp.wave#L241)

```wave
pub fun tcp_connect_v6_timeout(
    addr: SocketAddrV6, timeout_ms: i32
) -> NetResult<TcpStream>
```

[Declaration](net/tcp.wave#L255)

```wave
pub fun tcp_connect_addr_timeout(
    addr: SocketAddr, timeout_ms: i32
) -> NetResult<TcpStream>
```

[Declaration](net/tcp.wave#L271)

```wave
pub fun tcp_listener_local_addr_v4(listener: TcpListener) -> NetResult<SocketAddrV4>
```

[Declaration](net/tcp.wave#L280)

```wave
pub fun tcp_stream_local_addr_v4(stream: TcpStream) -> NetResult<SocketAddrV4>
```

[Declaration](net/tcp.wave#L289)

```wave
pub fun tcp_stream_peer_addr_v4(stream: TcpStream) -> NetResult<SocketAddrV4>
```

[Declaration](net/tcp.wave#L298)

```wave
pub fun tcp_listener_local_addr_v6(listener: TcpListener) -> NetResult<SocketAddrV6>
```

[Declaration](net/tcp.wave#L307)

```wave
pub fun tcp_stream_local_addr_v6(stream: TcpStream) -> NetResult<SocketAddrV6>
```

[Declaration](net/tcp.wave#L316)

```wave
pub fun tcp_stream_peer_addr_v6(stream: TcpStream) -> NetResult<SocketAddrV6>
```

[Declaration](net/tcp.wave#L325)

```wave
pub fun tcp_read(stream: TcpStream, buf: ptr<u8>, size: i64) -> i64
```

[Declaration](net/tcp.wave#L329)

```wave
pub fun tcp_write(stream: TcpStream, buf: ptr<u8>, size: i64) -> i64
```

[Declaration](net/tcp.wave#L333)

```wave
pub fun tcp_write_all(stream: TcpStream, buf: ptr<u8>, size: i64) -> i64
```

[Declaration](net/tcp.wave#L337)

```wave
pub fun tcp_read_exact(stream: TcpStream, buf: ptr<u8>, size: i64) -> i64
```

[Declaration](net/tcp.wave#L341)

```wave
pub fun tcp_read_result(stream: TcpStream, buf: ptr<u8>, size: i64) -> NetIoResult
```

[Declaration](net/tcp.wave#L349)

```wave
pub fun tcp_wait_read(stream: TcpStream, timeout_ms: i32) -> NetError
```

[Declaration](net/tcp.wave#L357)

```wave
pub fun tcp_wait_write(stream: TcpStream, timeout_ms: i32) -> NetError
```

[Declaration](net/tcp.wave#L370)

```wave
pub fun tcp_read_timeout(
    stream: TcpStream, buf: ptr<u8>, size: i64, timeout_ms: i32
) -> NetIoResult
```

[Declaration](net/tcp.wave#L380)

```wave
pub fun tcp_write_timeout(
    stream: TcpStream, buf: ptr<u8>, size: i64, timeout_ms: i32
) -> NetIoResult
```

[Declaration](net/tcp.wave#L390)

```wave
pub fun tcp_write_result(stream: TcpStream, buf: ptr<u8>, size: i64) -> NetIoResult
```

[Declaration](net/tcp.wave#L396)

```wave
pub fun tcp_write_all_result(stream: TcpStream, buf: ptr<u8>, size: i64) -> NetIoResult
```

[Declaration](net/tcp.wave#L400)

```wave
pub fun tcp_read_exact_result(stream: TcpStream, buf: ptr<u8>, size: i64) -> NetIoResult
```

[Declaration](net/tcp.wave#L404)

```wave
pub fun tcp_write_vectored(
    stream: TcpStream, slices: ptr<NetIoSlice>, count: i32
) -> NetIoResult
```

[Declaration](net/tcp.wave#L410)

```wave
pub fun tcp_read_vectored(
    stream: TcpStream, slices: ptr<NetIoSlice>, count: i32
) -> NetIoResult
```

[Declaration](net/tcp.wave#L416)

```wave
pub fun tcp_write_str(stream: TcpStream, text: str) -> NetIoResult
```

[Declaration](net/tcp.wave#L421)

```wave
pub fun tcp_shutdown_read(stream: TcpStream) -> NetError
```

[Declaration](net/tcp.wave#L427)

```wave
pub fun tcp_shutdown_write(stream: TcpStream) -> NetError
```

[Declaration](net/tcp.wave#L433)

```wave
pub fun tcp_shutdown_both(stream: TcpStream) -> NetError
```

[Declaration](net/tcp.wave#L439)

```wave
pub fun tcp_stream_set_nonblock(stream: TcpStream, enabled: bool) -> NetError
```

[Declaration](net/tcp.wave#L445)

```wave
pub fun tcp_listener_set_nonblock(listener: TcpListener, enabled: bool) -> NetError
```

[Declaration](net/tcp.wave#L451)

```wave
pub fun tcp_stream_is_nonblock(stream: TcpStream) -> NetResult<bool>
```

[Declaration](net/tcp.wave#L459)

```wave
pub fun tcp_listener_is_nonblock(listener: TcpListener) -> NetResult<bool>
```

[Declaration](net/tcp.wave#L486)

```wave
pub fun tcp_set_nodelay(stream: TcpStream, enabled: bool) -> NetError
```

[Declaration](net/tcp.wave#L490)

```wave
pub fun tcp_nodelay(stream: TcpStream) -> NetResult<bool>
```

[Declaration](net/tcp.wave#L494)

```wave
pub fun tcp_set_keepalive(stream: TcpStream, enabled: bool) -> NetError
```

[Declaration](net/tcp.wave#L498)

```wave
pub fun tcp_keepalive(stream: TcpStream) -> NetResult<bool>
```

[Declaration](net/tcp.wave#L502)

```wave
pub fun tcp_set_recv_buffer(stream: TcpStream, size: i32) -> NetError
```

[Declaration](net/tcp.wave#L506)

```wave
pub fun tcp_recv_buffer(stream: TcpStream) -> NetResult<i64>
```

[Declaration](net/tcp.wave#L510)

```wave
pub fun tcp_set_send_buffer(stream: TcpStream, size: i32) -> NetError
```

[Declaration](net/tcp.wave#L514)

```wave
pub fun tcp_send_buffer(stream: TcpStream) -> NetResult<i64>
```

[Declaration](net/tcp.wave#L519)

```wave
pub fun tcp_from_fd(fd: i64) -> NetResult<TcpStream>
```

[Declaration](net/tcp.wave#L524)

```wave
pub fun tcp_listener_from_fd(fd: i64) -> NetResult<TcpListener>
```

[Declaration](net/tcp.wave#L529)

```wave
pub fun tcp_stream_valid(stream: TcpStream) -> bool
```

[Declaration](net/tcp.wave#L530)

```wave
pub fun tcp_listener_valid(listener: TcpListener) -> bool
```

[Declaration](net/tcp.wave#L532)

```wave
pub fun tcp_close(stream: TcpStream) -> NetError
```

[Declaration](net/tcp.wave#L538)

```wave
pub fun tcp_close_listener(listener: TcpListener) -> NetError
```

## `std::net::udp`

[Source](net/udp.wave)

[Declaration](net/udp.wave#L53)

```wave
pub struct UdpSocket {
    fd: i64;
    family: i32;
}
```

[Declaration](net/udp.wave#L64)

```wave
pub fun udp_bind_v4(addr: SocketAddrV4) -> NetResult<UdpSocket>
```

[Declaration](net/udp.wave#L81)

```wave
pub fun udp_bind_v6(addr: SocketAddrV6) -> NetResult<UdpSocket>
```

[Declaration](net/udp.wave#L98)

```wave
pub fun udp_bind(port: u16) -> NetResult<UdpSocket>
```

[Declaration](net/udp.wave#L102)

```wave
pub fun udp_bind_loopback(port: u16) -> NetResult<UdpSocket>
```

[Declaration](net/udp.wave#L106)

```wave
pub fun udp_bind_ipv6(port: u16) -> NetResult<UdpSocket>
```

[Declaration](net/udp.wave#L110)

```wave
pub fun udp_bind_ipv6_loopback(port: u16) -> NetResult<UdpSocket>
```

[Declaration](net/udp.wave#L114)

```wave
pub fun udp_bind_addr(addr: SocketAddr) -> NetResult<UdpSocket>
```

[Declaration](net/udp.wave#L128)

```wave
pub fun udp_connect_v4(addr: SocketAddrV4) -> NetResult<UdpSocket>
```

[Declaration](net/udp.wave#L139)

```wave
pub fun udp_connect_v6(addr: SocketAddrV6) -> NetResult<UdpSocket>
```

[Declaration](net/udp.wave#L150)

```wave
pub fun udp_connect_addr(addr: SocketAddr) -> NetResult<UdpSocket>
```

[Declaration](net/udp.wave#L164)

```wave
pub fun udp_connect_existing_v4(sock: UdpSocket, addr: SocketAddrV4) -> NetError
```

[Declaration](net/udp.wave#L169)

```wave
pub fun udp_connect_existing_v6(sock: UdpSocket, addr: SocketAddrV6) -> NetError
```

[Declaration](net/udp.wave#L174)

```wave
pub fun udp_send_to(
    sock: UdpSocket, addr: SocketAddrV4, buf: ptr<u8>, size: i64
) -> NetIoResult
```

[Declaration](net/udp.wave#L187)

```wave
pub fun udp_send_to_v6(
    sock: UdpSocket, addr: SocketAddrV6, buf: ptr<u8>, size: i64
) -> NetIoResult
```

[Declaration](net/udp.wave#L196)

```wave
pub fun udp_send_str_to(sock: UdpSocket, addr: SocketAddrV4, text: str) -> NetIoResult
```

[Declaration](net/udp.wave#L200)

```wave
pub fun udp_send_to_addr(
    sock: UdpSocket, addr: SocketAddr, buf: ptr<u8>, size: i64
) -> NetIoResult
```

[Declaration](net/udp.wave#L216)

```wave
pub fun udp_recv(sock: UdpSocket, buf: ptr<u8>, size: i64) -> NetIoResult
```

[Declaration](net/udp.wave#L222)

```wave
pub fun udp_send(sock: UdpSocket, buf: ptr<u8>, size: i64) -> NetIoResult
```

[Declaration](net/udp.wave#L228)

```wave
pub fun udp_send_vectored(
    sock: UdpSocket, slices: ptr<NetIoSlice>, count: i32
) -> NetIoResult
```

[Declaration](net/udp.wave#L234)

```wave
pub fun udp_recv_vectored(
    sock: UdpSocket, slices: ptr<NetIoSlice>, count: i32
) -> NetIoResult
```

[Declaration](net/udp.wave#L240)

```wave
pub fun udp_recv_from(
    sock: UdpSocket, buf: ptr<u8>, size: i64, peer: ptr<SocketAddrV4>
) -> NetIoResult
```

[Declaration](net/udp.wave#L253)

```wave
pub fun udp_recv_from_v6(
    sock: UdpSocket, buf: ptr<u8>, size: i64, peer: ptr<SocketAddrV6>
) -> NetIoResult
```

[Declaration](net/udp.wave#L262)

```wave
pub fun udp_local_addr_v4(sock: UdpSocket) -> NetResult<SocketAddrV4>
```

[Declaration](net/udp.wave#L271)

```wave
pub fun udp_peer_addr_v4(sock: UdpSocket) -> NetResult<SocketAddrV4>
```

[Declaration](net/udp.wave#L280)

```wave
pub fun udp_local_addr_v6(sock: UdpSocket) -> NetResult<SocketAddrV6>
```

[Declaration](net/udp.wave#L289)

```wave
pub fun udp_peer_addr_v6(sock: UdpSocket) -> NetResult<SocketAddrV6>
```

[Declaration](net/udp.wave#L298)

```wave
pub fun udp_set_nonblock(sock: UdpSocket, enabled: bool) -> NetError
```

[Declaration](net/udp.wave#L304)

```wave
pub fun udp_is_nonblock(sock: UdpSocket) -> NetResult<bool>
```

[Declaration](net/udp.wave#L331)

```wave
pub fun udp_set_broadcast(sock: UdpSocket, enabled: bool) -> NetError
```

[Declaration](net/udp.wave#L335)

```wave
pub fun udp_broadcast(sock: UdpSocket) -> NetResult<bool>
```

[Declaration](net/udp.wave#L339)

```wave
pub fun udp_set_ipv4_ttl(sock: UdpSocket, ttl: i32) -> NetError
```

[Declaration](net/udp.wave#L343)

```wave
pub fun udp_ipv4_ttl(sock: UdpSocket) -> NetResult<i64>
```

[Declaration](net/udp.wave#L347)

```wave
pub fun udp_set_recv_buffer(sock: UdpSocket, size: i32) -> NetError
```

[Declaration](net/udp.wave#L351)

```wave
pub fun udp_recv_buffer(sock: UdpSocket) -> NetResult<i64>
```

[Declaration](net/udp.wave#L355)

```wave
pub fun udp_set_send_buffer(sock: UdpSocket, size: i32) -> NetError
```

[Declaration](net/udp.wave#L359)

```wave
pub fun udp_send_buffer(sock: UdpSocket) -> NetResult<i64>
```

[Declaration](net/udp.wave#L363)

```wave
pub fun udp_join_multicast_v4(
    sock: UdpSocket, group: Ipv4Addr, interface: Ipv4Addr
) -> NetError
```

[Declaration](net/udp.wave#L370)

```wave
pub fun udp_leave_multicast_v4(
    sock: UdpSocket, group: Ipv4Addr, interface: Ipv4Addr
) -> NetError
```

[Declaration](net/udp.wave#L377)

```wave
pub fun udp_join_multicast_v6(
    sock: UdpSocket, group: Ipv6Addr, interface_index: u32
) -> NetError
```

[Declaration](net/udp.wave#L384)

```wave
pub fun udp_leave_multicast_v6(
    sock: UdpSocket, group: Ipv6Addr, interface_index: u32
) -> NetError
```

[Declaration](net/udp.wave#L391)

```wave
pub fun udp_set_multicast_interface_v4(sock: UdpSocket, interface: Ipv4Addr) -> NetError
```

[Declaration](net/udp.wave#L396)

```wave
pub fun udp_multicast_interface_v4(sock: UdpSocket) -> NetResult<Ipv4Addr>
```

[Declaration](net/udp.wave#L407)

```wave
pub fun udp_set_multicast_ttl_v4(sock: UdpSocket, ttl: i32) -> NetError
```

[Declaration](net/udp.wave#L412)

```wave
pub fun udp_multicast_ttl_v4(sock: UdpSocket) -> NetResult<i64>
```

[Declaration](net/udp.wave#L419)

```wave
pub fun udp_set_multicast_loop_v4(sock: UdpSocket, enabled: bool) -> NetError
```

[Declaration](net/udp.wave#L424)

```wave
pub fun udp_multicast_loop_v4(sock: UdpSocket) -> NetResult<bool>
```

[Declaration](net/udp.wave#L431)

```wave
pub fun udp_set_multicast_interface_v6(sock: UdpSocket, interface_index: u32) -> NetError
```

[Declaration](net/udp.wave#L436)

```wave
pub fun udp_multicast_interface_v6(sock: UdpSocket) -> NetResult<i64>
```

[Declaration](net/udp.wave#L443)

```wave
pub fun udp_set_multicast_hops_v6(sock: UdpSocket, hops: i32) -> NetError
```

[Declaration](net/udp.wave#L448)

```wave
pub fun udp_multicast_hops_v6(sock: UdpSocket) -> NetResult<i64>
```

[Declaration](net/udp.wave#L455)

```wave
pub fun udp_set_multicast_loop_v6(sock: UdpSocket, enabled: bool) -> NetError
```

[Declaration](net/udp.wave#L460)

```wave
pub fun udp_multicast_loop_v6(sock: UdpSocket) -> NetResult<bool>
```

[Declaration](net/udp.wave#L468)

```wave
pub fun udp_from_fd(fd: i64, family: i32) -> NetResult<UdpSocket>
```

[Declaration](net/udp.wave#L473)

```wave
pub fun udp_socket_valid(sock: UdpSocket) -> bool
```

[Declaration](net/udp.wave#L475)

```wave
pub fun udp_close(sock: UdpSocket) -> NetError
```

## `std::net::unix`

[Source](net/unix.wave)

[Declaration](net/unix.wave#L32)

```wave
pub struct UnixListener { fd: i64; }
```

[Declaration](net/unix.wave#L33)

```wave
pub struct UnixStream { fd: i64; }
```

[Declaration](net/unix.wave#L65)

```wave
pub fun unix_listen(path: str, backlog: i32) -> NetResult<UnixListener>
```

[Declaration](net/unix.wave#L85)

```wave
pub fun unix_connect(path: str) -> NetResult<UnixStream>
```

[Declaration](net/unix.wave#L100)

```wave
#[target(os="linux")]
pub fun unix_listen_abstract(name: str, backlog: i32) -> NetResult<UnixListener>
```

[Declaration](net/unix.wave#L114)

```wave
#[target(os="macos")]
pub fun unix_listen_abstract(name: str, backlog: i32) -> NetResult<UnixListener>
```

[Declaration](net/unix.wave#L118)

```wave
#[target(os="freebsd")]
pub fun unix_listen_abstract(name: str, backlog: i32) -> NetResult<UnixListener>
```

[Declaration](net/unix.wave#L122)

```wave
#[target(os="windows")]
pub fun unix_listen_abstract(name: str, backlog: i32) -> NetResult<UnixListener>
```

[Declaration](net/unix.wave#L126)

```wave
pub fun unix_accept(listener: UnixListener) -> NetResult<UnixStream>
```

[Declaration](net/unix.wave#L132)

```wave
pub fun unix_read(stream: UnixStream, buf: ptr<u8>, size: i64) -> NetIoResult
```

[Declaration](net/unix.wave#L145)

```wave
pub fun unix_write(stream: UnixStream, buf: ptr<u8>, size: i64) -> NetIoResult
```

[Declaration](net/unix.wave#L151)

```wave
pub fun unix_write_all(stream: UnixStream, buf: ptr<u8>, size: i64) -> NetIoResult
```

[Declaration](net/unix.wave#L155)

```wave
pub fun unix_write_vectored(
    stream: UnixStream, slices: ptr<NetIoSlice>, count: i32
) -> NetIoResult
```

[Declaration](net/unix.wave#L161)

```wave
pub fun unix_read_vectored(
    stream: UnixStream, slices: ptr<NetIoSlice>, count: i32
) -> NetIoResult
```

[Declaration](net/unix.wave#L167)

```wave
pub fun unix_shutdown_read(stream: UnixStream) -> NetError
```

[Declaration](net/unix.wave#L172)

```wave
pub fun unix_shutdown_write(stream: UnixStream) -> NetError
```

[Declaration](net/unix.wave#L177)

```wave
pub fun unix_shutdown_both(stream: UnixStream) -> NetError
```

[Declaration](net/unix.wave#L183)

```wave
pub fun unix_close(stream: UnixStream) -> NetError
```

[Declaration](net/unix.wave#L188)

```wave
pub fun unix_close_listener(listener: UnixListener) -> NetError
```

[Declaration](net/unix.wave#L195)

```wave
#[target(os="linux")]
pub fun unix_remove(path: str) -> NetError
```

[Declaration](net/unix.wave#L201)

```wave
#[target(os="macos")]
pub fun unix_remove(path: str) -> NetError
```

[Declaration](net/unix.wave#L207)

```wave
#[target(os="freebsd")]
pub fun unix_remove(path: str) -> NetError
```

[Declaration](net/unix.wave#L213)

```wave
#[target(os="windows")]
pub fun unix_remove(path: str) -> NetError
```

[Declaration](net/unix.wave#L221)

```wave
pub fun unix_stream_valid(stream: UnixStream) -> bool
```

[Declaration](net/unix.wave#L222)

```wave
pub fun unix_listener_valid(listener: UnixListener) -> bool
```

## `std::net::vectored`

[Source](net/vectored.wave)

[Declaration](net/vectored.wave#L8)

```wave
pub const NET_IOV_MAX: i32 = 32;
```

[Declaration](net/vectored.wave#L10)

```wave
pub struct NetIoSlice {
    data: ptr<u8>;
    length: i64;
}
```

[Declaration](net/vectored.wave#L37)

```wave
pub fun net_send_vectored(
    fd: i64, slices: ptr<NetIoSlice>, count: i32, flags: i32
) -> NetIoResult
```

[Declaration](net/vectored.wave#L52)

```wave
pub fun net_recv_vectored(
    fd: i64, slices: ptr<NetIoSlice>, count: i32, flags: i32, eof_on_zero: bool
) -> NetIoResult
```

## `std::net::windows_timeout`

[Source](net/windows_timeout.wave)

[Declaration](net/windows_timeout.wave#L5)

```wave
pub fun net_io_timeout(fd: i64, buffer: ptr<u8>, size: i64, timeout_ms: i32, writing: bool) -> NetIoResult
```

## `std::path::analyze`

[Source](path/analyze.wave)

[Declaration](path/analyze.wave#L62)

```wave
pub fun path_basename_start(path: str) -> i32
```

[Declaration](path/analyze.wave#L71)

```wave
pub fun path_basename_len(path: str) -> i32
```

[Declaration](path/analyze.wave#L78)

```wave
pub fun path_dirname_len(path: str) -> i32
```

[Declaration](path/analyze.wave#L93)

```wave
pub fun path_ext_start(path: str) -> i32
```

[Declaration](path/analyze.wave#L115)

```wave
pub fun path_has_ext(path: str) -> bool
```

## `std::path::consts`

[Source](path/consts.wave)

[Declaration](path/consts.wave#L19)

```wave
pub const PATH_ERR_NO_SPACE: i32 = -1;
```

## `std::path::copy`

[Source](path/copy.wave)

[Declaration](path/copy.wave#L32)

```wave
pub fun path_join2(dst: ptr<u8>, dst_cap: i32, left: str, right: str) -> i32
```

[Declaration](path/copy.wave#L77)

```wave
pub fun path_basename_copy(dst: ptr<u8>, dst_cap: i32, path: str) -> i32
```

[Declaration](path/copy.wave#L98)

```wave
pub fun path_dirname_copy(dst: ptr<u8>, dst_cap: i32, path: str) -> i32
```

## `std::path::core`

[Source](path/core.wave)

[Declaration](path/core.wave#L19)

```wave
pub fun path_is_sep(c: u8) -> bool
```

[Declaration](path/core.wave#L31)

```wave
pub fun path_len(path: str) -> i32
```

[Declaration](path/core.wave#L59)

```wave
pub fun path_is_abs(path: str) -> bool
```

## `std::process::consts`

[Source](process/consts.wave)

[Declaration](process/consts.wave#L19)

```wave
pub const PROC_ERR_INTR: i64 = -4;
```

[Declaration](process/consts.wave#L21)

```wave
pub const PROC_WAIT_NOHANG: i32 = 1;
```

[Declaration](process/consts.wave#L23)

```wave
pub const PROC_EXIT_EXEC_FAIL: i32 = 127;
```

[Declaration](process/consts.wave#L24)

```wave
pub const PROC_EXIT_DUP_FAIL: i32 = 126;
```

## `std::process::core`

[Source](process/core.wave)

[Declaration](process/core.wave#L64)

```wave
pub fun proc_exit(code: i32) -> !
```

[Declaration](process/core.wave#L68)

```wave
pub fun proc_getpid() -> i64
```

[Declaration](process/core.wave#L72)

```wave
pub fun proc_getppid() -> i64
```

[Declaration](process/core.wave#L76)

```wave
pub fun proc_fork() -> i64
```

[Declaration](process/core.wave#L80)

```wave
pub fun proc_execve(path: str, argv: ptr<ptr<i8>>, envp: ptr<ptr<i8>>) -> i64
```

[Declaration](process/core.wave#L84)

```wave
pub fun proc_waitpid_raw(pid: i64, status: ptr<i32>, options: i32) -> i64
```

[Declaration](process/core.wave#L99)

```wave
pub fun proc_kill(pid: i64, sig: i32) -> i64
```

## `std::process::spawn`

[Source](process/spawn.wave)

[Declaration](process/spawn.wave#L57)

```wave
pub struct ProcPipeResult {
    status: i64;
    read_fd: i64;
    write_fd: i64;
}
```

[Declaration](process/spawn.wave#L63)

```wave
pub struct ProcSpawnStdoutResult {
    status: i64;
    pid: i64;
    read_fd: i64;
}
```

[Declaration](process/spawn.wave#L88)

```wave
pub fun _proc_remap_child_fds(stdin_fd: i64, stdout_fd: i64, stderr_fd: i64) -> i64
```

[Declaration](process/spawn.wave#L154)

```wave
pub fun proc_spawn_exec_raw(
    path: str,
    argv: ptr<ptr<i8>>,
    envp: ptr<ptr<i8>>,
    stdin_fd: i64,
    stdout_fd: i64,
    stderr_fd: i64
) -> i64
```

[Declaration](process/spawn.wave#L201)

```wave
pub fun proc_spawn(path: str, argv: ptr<ptr<i8>>, envp: ptr<ptr<i8>>) -> i64
```

[Declaration](process/spawn.wave#L205)

```wave
pub fun proc_make_pipe() -> ProcPipeResult
```

[Declaration](process/spawn.wave#L223)

```wave
pub fun proc_spawn_capture_stdout(
    path: str,
    argv: ptr<ptr<i8>>,
    envp: ptr<ptr<i8>>
) -> ProcSpawnStdoutResult
```

## `std::process::wait`

[Source](process/wait.wave)

[Declaration](process/wait.wave#L35)

```wave
pub fun proc_wait(pid: i64, out_status: ptr<i32>) -> i64
```

[Declaration](process/wait.wave#L39)

```wave
pub fun proc_wait_nohang(pid: i64, out_status: ptr<i32>) -> i64
```

[Declaration](process/wait.wave#L43)

```wave
pub fun proc_status_exited(status: i32) -> bool
```

[Declaration](process/wait.wave#L51)

```wave
pub fun proc_status_exit_code(status: i32) -> i32
```

[Declaration](process/wait.wave#L55)

```wave
pub fun proc_status_signaled(status: i32) -> bool
```

[Declaration](process/wait.wave#L64)

```wave
pub fun proc_status_term_signal(status: i32) -> i32
```

[Declaration](process/wait.wave#L68)

```wave
pub fun proc_status_stopped(status: i32) -> bool
```

[Declaration](process/wait.wave#L76)

```wave
pub fun proc_status_stop_signal(status: i32) -> i32
```

[Declaration](process/wait.wave#L82)

```wave
#[target(os="linux")]
pub fun proc_status_continued(status: i32) -> bool
```

[Declaration](process/wait.wave#L87)

```wave
#[target(os="windows")]
pub fun proc_status_continued(status: i32) -> bool
```

[Declaration](process/wait.wave#L92)

```wave
#[target(os="freebsd")]
pub fun proc_status_continued(status: i32) -> bool
```

[Declaration](process/wait.wave#L97)

```wave
#[target(os="macos")]
pub fun proc_status_continued(status: i32) -> bool
```

[Declaration](process/wait.wave#L102)

```wave
#[target(os="wasi")]
pub fun proc_status_continued(status: i32) -> bool
```

## `std::random::fill`

[Source](random/fill.wave)

[Declaration](random/fill.wave#L8)

```wave
pub struct RandomFillResult {
    ok: bool;
    written: i64;
    error: i64;
}
```

[Declaration](random/fill.wave#L15)

```wave
pub fun random_available() -> bool
```

[Declaration](random/fill.wave#L22)

```wave
pub fun random_fill(buffer: ptr<u8>, size: i64) -> RandomFillResult
```

## `std::string::ascii`

[Source](string/ascii.wave)

[Declaration](string/ascii.wave#L19)

```wave
pub fun is_digit(c: u8) -> bool
```

[Declaration](string/ascii.wave#L27)

```wave
pub fun is_lower(c: u8) -> bool
```

[Declaration](string/ascii.wave#L35)

```wave
pub fun is_upper(c: u8) -> bool
```

[Declaration](string/ascii.wave#L43)

```wave
pub fun is_alpha(c: u8) -> bool
```

[Declaration](string/ascii.wave#L55)

```wave
pub fun is_alnum(c: u8) -> bool
```

[Declaration](string/ascii.wave#L67)

```wave
pub fun is_space(c: u8) -> bool
```

[Declaration](string/ascii.wave#L91)

```wave
pub fun to_lower(c: u8) -> u8
```

[Declaration](string/ascii.wave#L99)

```wave
pub fun to_upper(c: u8) -> u8
```

[Declaration](string/ascii.wave#L108)

```wave
pub fun is_ascii(c: u8) -> bool
```

[Declaration](string/ascii.wave#L112)

```wave
pub fun is_hex_digit(c: u8) -> bool
```

[Declaration](string/ascii.wave#L116)

```wave
pub fun is_control(c: u8) -> bool
```

[Declaration](string/ascii.wave#L120)

```wave
pub fun is_printable(c: u8) -> bool
```

[Declaration](string/ascii.wave#L124)

```wave
pub fun is_graphic(c: u8) -> bool
```

[Declaration](string/ascii.wave#L128)

```wave
pub fun is_punct(c: u8) -> bool
```

## `std::string::cmp`

[Source](string/cmp.wave)

[Declaration](string/cmp.wave#L21)

```wave
pub fun eq(a: str, b: str) -> bool
```

[Declaration](string/cmp.wave#L44)

```wave
pub fun eq_ignore_ascii_case(a: str, b: str) -> bool
```

[Declaration](string/cmp.wave#L65)

```wave
pub fun cmp(a: str, b: str) -> i32
```

[Declaration](string/cmp.wave#L90)

```wave
pub fun starts_with(s: str, prefix: str) -> bool
```

[Declaration](string/cmp.wave#L104)

```wave
pub fun ends_with(s: str, suffix: str) -> bool
```

## `std::string::find`

[Source](string/find.wave)

[Declaration](string/find.wave#L21)

```wave
pub fun find_char(s: str, c: u8) -> i32
```

[Declaration](string/find.wave#L35)

```wave
pub fun rfind_char(s: str, c: u8) -> i32
```

[Declaration](string/find.wave#L50)

```wave
pub fun contains_char(s: str, c: u8) -> bool
```

[Declaration](string/find.wave#L58)

```wave
pub fun count_char(s: str, c: u8) -> i32
```

[Declaration](string/find.wave#L73)

```wave
pub fun find(s: str, needle: str) -> i32
```

[Declaration](string/find.wave#L119)

```wave
pub fun rfind(s: str, needle: str) -> i32
```

[Declaration](string/find.wave#L160)

```wave
pub fun contains(s: str, needle: str) -> bool
```

[Declaration](string/find.wave#L168)

```wave
pub fun count(s: str, needle: str) -> i32
```

## `std::string::hash`

[Source](string/hash.wave)

[Declaration](string/hash.wave#L19)

```wave
pub fun djb2_32(s: str) -> i32
```

[Declaration](string/hash.wave#L34)

```wave
pub fun fnv1a_64(s: str) -> i64
```

## `std::string::len`

[Source](string/len.wave)

[Declaration](string/len.wave#L19)

```wave
pub fun len(s: str) -> i32
```

[Declaration](string/len.wave#L27)

```wave
pub fun is_empty(s: str) -> bool
```

## `std::string::trim`

[Source](string/trim.wave)

[Declaration](string/trim.wave#L21)

```wave
pub fun trim_left_index(s: str) -> i32
```

[Declaration](string/trim.wave#L37)

```wave
pub fun trim_right_index(s: str) -> i32
```

[Declaration](string/trim.wave#L63)

```wave
pub fun trim_range(s: str, out_start: ptr<i32>, out_end: ptr<i32>)
```

## `std::sys::env`

[Source](sys/env.wave)

[Declaration](sys/env.wave#L23)

```wave
#[target(os="linux")]
pub import("std::sys::linux::env")::{
    env_read,
};
```

[Declaration](sys/env.wave#L28)

```wave
#[target(os="macos")]
pub import("std::sys::macos::env")::{
    ERR_NOT_IMPLEMENTED,
    env_read,
};
```

[Declaration](sys/env.wave#L34)

```wave
#[target(os="windows")]
pub import("std::sys::windows::env")::{env_read};
```

[Declaration](sys/env.wave#L37)

```wave
#[target(os="freebsd")]
pub import("std::sys::freebsd::env")::{env_read};
```

[Declaration](sys/env.wave#L40)

```wave
#[target(os="wasi")]
pub import("std::sys::wasm::env")::{env_read, ENV_READ_NEEDS_SPACE};
```

## `std::sys::errors`

[Source](sys/errors.wave)

[Declaration](sys/errors.wave#L8)

```wave
#[target(os="linux")]
pub const SYS_ERR_INTR: i64 = -4;
```

[Declaration](sys/errors.wave#L10)

```wave
#[target(os="linux")]
pub const SYS_ERR_INVAL: i64 = -22;
```

[Declaration](sys/errors.wave#L12)

```wave
#[target(os="linux")]
pub const SYS_ERR_OVERFLOW: i64 = -75;
```

[Declaration](sys/errors.wave#L14)

```wave
#[target(os="macos")]
pub const SYS_ERR_INTR: i64 = -4;
```

[Declaration](sys/errors.wave#L16)

```wave
#[target(os="macos")]
pub const SYS_ERR_INVAL: i64 = -22;
```

[Declaration](sys/errors.wave#L18)

```wave
#[target(os="macos")]
pub const SYS_ERR_OVERFLOW: i64 = -84;
```

[Declaration](sys/errors.wave#L20)

```wave
#[target(os="windows")]
pub const SYS_ERR_INTR: i64 = -4;
```

[Declaration](sys/errors.wave#L22)

```wave
#[target(os="windows")]
pub const SYS_ERR_INVAL: i64 = -22;
```

[Declaration](sys/errors.wave#L24)

```wave
#[target(os="windows")]
pub const SYS_ERR_OVERFLOW: i64 = -75;
```

[Declaration](sys/errors.wave#L26)

```wave
#[target(os="freebsd")]
pub const SYS_ERR_INTR: i64 = -4;
```

[Declaration](sys/errors.wave#L28)

```wave
#[target(os="freebsd")]
pub const SYS_ERR_INVAL: i64 = -22;
```

[Declaration](sys/errors.wave#L30)

```wave
#[target(os="freebsd")]
pub const SYS_ERR_OVERFLOW: i64 = -84;
```

[Declaration](sys/errors.wave#L32)

```wave
#[target(os="unknown")]
pub const SYS_ERR_INTR: i64 = -4;
```

[Declaration](sys/errors.wave#L34)

```wave
#[target(os="unknown")]
pub const SYS_ERR_INVAL: i64 = -22;
```

[Declaration](sys/errors.wave#L36)

```wave
#[target(os="unknown")]
pub const SYS_ERR_OVERFLOW: i64 = -75;
```

[Declaration](sys/errors.wave#L38)

```wave
#[target(os="none")]
pub const SYS_ERR_INTR: i64 = -4;
```

[Declaration](sys/errors.wave#L40)

```wave
#[target(os="none")]
pub const SYS_ERR_INVAL: i64 = -22;
```

[Declaration](sys/errors.wave#L42)

```wave
#[target(os="none")]
pub const SYS_ERR_OVERFLOW: i64 = -75;
```

[Declaration](sys/errors.wave#L44)

```wave
#[target(os="wasi")]
pub const SYS_ERR_INTR: i64 = -27;
```

[Declaration](sys/errors.wave#L46)

```wave
#[target(os="wasi")]
pub const SYS_ERR_INVAL: i64 = -28;
```

[Declaration](sys/errors.wave#L48)

```wave
#[target(os="wasi")]
pub const SYS_ERR_OVERFLOW: i64 = -61;
```

[Declaration](sys/errors.wave#L50)

```wave
pub fun sys_is_interrupted(result: i64) -> bool
```

## `std::sys::event`

[Source](sys/event.wave)

[Declaration](sys/event.wave#L4)

```wave
#[target(os="linux")]
pub import("std::sys::linux::event")::{
    EVENT_BACKEND_KIND, EVENT_READABLE, EVENT_WRITABLE, EVENT_ERROR, EVENT_HANGUP,
    NativeEvent, event_create, event_add, event_modify, event_remove, event_wait, event_close,
};
```

[Declaration](sys/event.wave#L9)

```wave
#[target(os="macos")]
pub import("std::sys::macos::event")::{
    EVENT_BACKEND_KIND, EVENT_READABLE, EVENT_WRITABLE, EVENT_ERROR, EVENT_HANGUP,
    NativeEvent, event_create, event_add, event_modify, event_remove, event_wait, event_close,
};
```

[Declaration](sys/event.wave#L14)

```wave
#[target(os="freebsd")]
pub import("std::sys::freebsd::event")::{
    EVENT_BACKEND_KIND, EVENT_READABLE, EVENT_WRITABLE, EVENT_ERROR, EVENT_HANGUP,
    NativeEvent, event_create, event_add, event_modify, event_remove, event_wait, event_close,
};
```

[Declaration](sys/event.wave#L19)

```wave
#[target(os="windows")]
pub import("std::sys::windows::event")::{
    EVENT_BACKEND_KIND, EVENT_READABLE, EVENT_WRITABLE, EVENT_ERROR, EVENT_HANGUP,
    NativeEvent, event_create, event_add, event_modify, event_remove, event_wait, event_close,
};
```

## `std::sys::freebsd::amd64::env`

[Source](sys/freebsd/amd64/env.wave)

[Declaration](sys/freebsd/amd64/env.wave#L6)

```wave
pub import("std::sys::freebsd::common::env")::{
    ERR_NOT_IMPLEMENTED, env_read
};
```

## `std::sys::freebsd::amd64::fs`

[Source](sys/freebsd/amd64/fs.wave)

[Declaration](sys/freebsd/amd64/fs.wave#L6)

```wave
pub import("std::sys::freebsd::common::fs")::{
    FS_O_RDONLY, FS_O_WRONLY, FS_O_RDWR, FS_O_NONBLOCK, FS_O_APPEND, FS_O_CREAT, FS_O_TRUNC,
    FS_O_EXCL, FS_F_OK, FS_X_OK, FS_W_OK, FS_R_OK, FS_SEEK_SET, FS_SEEK_CUR, FS_SEEK_END,
    FS_F_GETFL, FS_F_SETFL, Stat, open, close, dup, pipe, dup2, fsync, fcntl, read, write,
    getcwd, chdir, access, lseek, unlink, mkdir, rmdir, rename_path, stat, fstat, ftruncate, same_file
};
```

## `std::sys::freebsd::amd64::memory`

[Source](sys/freebsd/amd64/memory.wave)

[Declaration](sys/freebsd/amd64/memory.wave#L6)

```wave
pub import("std::sys::freebsd::common::memory")::{
    PROT_READ, PROT_WRITE, MAP_PRIVATE, MAP_ANON, MAP_ANONYMOUS, mmap, munmap, brk, sys_alloc,
    sys_free, sys_page_size
};
```

## `std::sys::freebsd::amd64::process`

[Source](sys/freebsd/amd64/process.wave)

[Declaration](sys/freebsd/amd64/process.wave#L6)

```wave
pub import("std::sys::freebsd::common::process")::{
    exit, getpid, getppid, fork, execve, waitpid, kill
};
```

## `std::sys::freebsd::amd64::random`

[Source](sys/freebsd/amd64/random.wave)

[Declaration](sys/freebsd/amd64/random.wave#L6)

```wave
pub fun sys_random_available() -> bool
```

[Declaration](sys/freebsd/amd64/random.wave#L7)

```wave
pub fun sys_random_read(buffer: ptr<u8>, size: i64) -> i64
```

## `std::sys::freebsd::amd64::socket`

[Source](sys/freebsd/amd64/socket.wave)

[Declaration](sys/freebsd/amd64/socket.wave#L6)

```wave
pub import("std::sys::freebsd::common::socket")::{
    AF_INET, AF_INET6, SOCK_STREAM, SOCK_DGRAM, IPPROTO_IP, IPPROTO_IPV6, IPPROTO_TCP,
    IPPROTO_UDP, SHUT_RD, SHUT_WR, SHUT_RDWR, SOL_SOCKET, SO_REUSEADDR, SO_REUSEPORT, SO_ERROR,
    SO_RCVBUF, SO_SNDBUF, SO_BROADCAST, SO_KEEPALIVE, TCP_NODELAY, IP_TTL, IP_MULTICAST_IF,
    IP_MULTICAST_TTL, IP_MULTICAST_LOOP, IP_ADD_MEMBERSHIP, IP_DROP_MEMBERSHIP,
    IPV6_MULTICAST_IF, IPV6_MULTICAST_HOPS, IPV6_MULTICAST_LOOP, IPV6_JOIN_GROUP,
    IPV6_LEAVE_GROUP, MSG_NOSIGNAL, MSG_DONTWAIT, POLLIN, POLLOUT, POLLERR, POLLHUP, POLLNVAL, PollFd, socket,
    bind, listen, accept, connect, getsockname, getpeername, shutdown, close_socket, setsockopt,
    getsockopt, poll, send, recv, sendto, recvfrom
};
```

## `std::sys::freebsd::amd64::syscall`

[Source](sys/freebsd/amd64/syscall.wave)

[Declaration](sys/freebsd/amd64/syscall.wave#L9)

```wave
pub fun syscall0(id: i64) -> i64
```

[Declaration](sys/freebsd/amd64/syscall.wave#L15)

```wave
pub fun syscall1(id: i64, a1: i64) -> i64
```

[Declaration](sys/freebsd/amd64/syscall.wave#L21)

```wave
pub fun syscall2(id: i64, a1: i64, a2: i64) -> i64
```

[Declaration](sys/freebsd/amd64/syscall.wave#L27)

```wave
pub fun syscall3(id: i64, a1: i64, a2: i64, a3: i64) -> i64
```

[Declaration](sys/freebsd/amd64/syscall.wave#L33)

```wave
pub fun syscall4(id: i64, a1: i64, a2: i64, a3: i64, a4: i64) -> i64
```

[Declaration](sys/freebsd/amd64/syscall.wave#L39)

```wave
pub fun syscall5(id: i64, a1: i64, a2: i64, a3: i64, a4: i64, a5: i64) -> i64
```

[Declaration](sys/freebsd/amd64/syscall.wave#L45)

```wave
pub fun syscall6(id: i64, a1: i64, a2: i64, a3: i64, a4: i64, a5: i64, a6: i64) -> i64
```

## `std::sys::freebsd::amd64::time`

[Source](sys/freebsd/amd64/time.wave)

[Declaration](sys/freebsd/amd64/time.wave#L6)

```wave
pub import("std::sys::freebsd::common::time")::{
    TimeSpec, nanosleep, clock_gettime
};
```

## `std::sys::freebsd::amd64::tty`

[Source](sys/freebsd/amd64/tty.wave)

[Declaration](sys/freebsd/amd64/tty.wave#L6)

```wave
pub import("std::sys::freebsd::common::tty")::{
    TTY_SYS_IOCTL, TTY_SYS_FCNTL, TTY_TCGETS, TTY_TCSETS, TTY_TCSETSW, TTY_TCSETSF, TTY_TCSANOW,
    TTY_TCSADRAIN, TTY_TCSAFLUSH, TTY_F_GETFL, TTY_F_SETFL, TTY_O_NONBLOCK, TTY_ICANON,
    TTY_ECHO, TTY_VMIN_IDX, TTY_VTIME_IDX, Termios, TtyRawState, tty_getattr, tty_setattr,
    tty_getfl, tty_setfl, tty_enable_raw_nonblock, tty_restore
};
```

## `std::sys::freebsd::arm64::env`

[Source](sys/freebsd/arm64/env.wave)

[Declaration](sys/freebsd/arm64/env.wave#L6)

```wave
pub import("std::sys::freebsd::common::env")::{
    ERR_NOT_IMPLEMENTED, env_read
};
```

## `std::sys::freebsd::arm64::fs`

[Source](sys/freebsd/arm64/fs.wave)

[Declaration](sys/freebsd/arm64/fs.wave#L6)

```wave
pub import("std::sys::freebsd::common::fs")::{
    FS_O_RDONLY, FS_O_WRONLY, FS_O_RDWR, FS_O_NONBLOCK, FS_O_APPEND, FS_O_CREAT, FS_O_TRUNC,
    FS_O_EXCL, FS_F_OK, FS_X_OK, FS_W_OK, FS_R_OK, FS_SEEK_SET, FS_SEEK_CUR, FS_SEEK_END,
    FS_F_GETFL, FS_F_SETFL, Stat, open, close, dup, pipe, dup2, fsync, fcntl, read, write,
    getcwd, chdir, access, lseek, unlink, mkdir, rmdir, rename_path, stat, fstat, ftruncate, same_file
};
```

## `std::sys::freebsd::arm64::memory`

[Source](sys/freebsd/arm64/memory.wave)

[Declaration](sys/freebsd/arm64/memory.wave#L6)

```wave
pub import("std::sys::freebsd::common::memory")::{
    PROT_READ, PROT_WRITE, MAP_PRIVATE, MAP_ANON, MAP_ANONYMOUS, mmap, munmap, brk, sys_alloc,
    sys_free, sys_page_size
};
```

## `std::sys::freebsd::arm64::process`

[Source](sys/freebsd/arm64/process.wave)

[Declaration](sys/freebsd/arm64/process.wave#L6)

```wave
pub import("std::sys::freebsd::common::process")::{
    exit, getpid, getppid, fork, execve, waitpid, kill
};
```

## `std::sys::freebsd::arm64::socket`

[Source](sys/freebsd/arm64/socket.wave)

[Declaration](sys/freebsd/arm64/socket.wave#L6)

```wave
pub import("std::sys::freebsd::common::socket")::{
    AF_INET, AF_INET6, SOCK_STREAM, SOCK_DGRAM, IPPROTO_IP, IPPROTO_IPV6, IPPROTO_TCP,
    IPPROTO_UDP, SHUT_RD, SHUT_WR, SHUT_RDWR, SOL_SOCKET, SO_REUSEADDR, SO_REUSEPORT, SO_ERROR,
    SO_RCVBUF, SO_SNDBUF, SO_BROADCAST, SO_KEEPALIVE, TCP_NODELAY, IP_TTL, IP_MULTICAST_IF,
    IP_MULTICAST_TTL, IP_MULTICAST_LOOP, IP_ADD_MEMBERSHIP, IP_DROP_MEMBERSHIP,
    IPV6_MULTICAST_IF, IPV6_MULTICAST_HOPS, IPV6_MULTICAST_LOOP, IPV6_JOIN_GROUP,
    IPV6_LEAVE_GROUP, MSG_NOSIGNAL, MSG_DONTWAIT, POLLIN, POLLOUT, POLLERR, POLLHUP, POLLNVAL, PollFd, socket,
    bind, listen, accept, connect, getsockname, getpeername, shutdown, close_socket, setsockopt,
    getsockopt, poll, send, recv, sendto, recvfrom
};
```

## `std::sys::freebsd::arm64::syscall`

[Source](sys/freebsd/arm64/syscall.wave)

[Declaration](sys/freebsd/arm64/syscall.wave#L8)

```wave
pub fun syscall0(id: i64) -> i64
```

[Declaration](sys/freebsd/arm64/syscall.wave#L24)

```wave
pub fun syscall1(id: i64, a1: i64) -> i64
```

[Declaration](sys/freebsd/arm64/syscall.wave#L40)

```wave
pub fun syscall2(id: i64, a1: i64, a2: i64) -> i64
```

[Declaration](sys/freebsd/arm64/syscall.wave#L57)

```wave
pub fun syscall3(id: i64, a1: i64, a2: i64, a3: i64) -> i64
```

[Declaration](sys/freebsd/arm64/syscall.wave#L75)

```wave
pub fun syscall4(id: i64, a1: i64, a2: i64, a3: i64, a4: i64) -> i64
```

[Declaration](sys/freebsd/arm64/syscall.wave#L94)

```wave
pub fun syscall5(id: i64, a1: i64, a2: i64, a3: i64, a4: i64, a5: i64) -> i64
```

[Declaration](sys/freebsd/arm64/syscall.wave#L114)

```wave
pub fun syscall6(id: i64, a1: i64, a2: i64, a3: i64, a4: i64, a5: i64, a6: i64) -> i64
```

## `std::sys::freebsd::arm64::time`

[Source](sys/freebsd/arm64/time.wave)

[Declaration](sys/freebsd/arm64/time.wave#L6)

```wave
pub import("std::sys::freebsd::common::time")::{
    TimeSpec, nanosleep, clock_gettime
};
```

## `std::sys::freebsd::arm64::tty`

[Source](sys/freebsd/arm64/tty.wave)

[Declaration](sys/freebsd/arm64/tty.wave#L6)

```wave
pub import("std::sys::freebsd::common::tty")::{
    TTY_SYS_IOCTL, TTY_SYS_FCNTL, TTY_TCGETS, TTY_TCSETS, TTY_TCSETSW, TTY_TCSETSF, TTY_TCSANOW,
    TTY_TCSADRAIN, TTY_TCSAFLUSH, TTY_F_GETFL, TTY_F_SETFL, TTY_O_NONBLOCK, TTY_ICANON,
    TTY_ECHO, TTY_VMIN_IDX, TTY_VTIME_IDX, Termios, TtyRawState, tty_getattr, tty_setattr,
    tty_getfl, tty_setfl, tty_enable_raw_nonblock, tty_restore
};
```

## `std::sys::freebsd::common::env`

[Source](sys/freebsd/common/env.wave)

[Declaration](sys/freebsd/common/env.wave#L22)

```wave
pub const ERR_NOT_IMPLEMENTED: i64 = -78;
```

[Declaration](sys/freebsd/common/env.wave#L24)

```wave
pub fun env_read(buf: ptr<u8>, cap: i64) -> i64
```

## `std::sys::freebsd::common::fs`

[Source](sys/freebsd/common/fs.wave)

[Declaration](sys/freebsd/common/fs.wave#L34)

```wave
pub const FS_O_RDONLY: i32 = 0;
```

[Declaration](sys/freebsd/common/fs.wave#L35)

```wave
pub const FS_O_WRONLY: i32 = 1;
```

[Declaration](sys/freebsd/common/fs.wave#L36)

```wave
pub const FS_O_RDWR: i32 = 2;
```

[Declaration](sys/freebsd/common/fs.wave#L37)

```wave
pub const FS_O_NONBLOCK: i32 = 4;
```

[Declaration](sys/freebsd/common/fs.wave#L38)

```wave
pub const FS_O_APPEND: i32 = 8;
```

[Declaration](sys/freebsd/common/fs.wave#L39)

```wave
pub const FS_O_CREAT: i32 = 512;
```

[Declaration](sys/freebsd/common/fs.wave#L40)

```wave
pub const FS_O_TRUNC: i32 = 1024;
```

[Declaration](sys/freebsd/common/fs.wave#L41)

```wave
pub const FS_O_EXCL: i32 = 2048;
```

[Declaration](sys/freebsd/common/fs.wave#L44)

```wave
pub const FS_F_OK: i32 = 0;
```

[Declaration](sys/freebsd/common/fs.wave#L45)

```wave
pub const FS_X_OK: i32 = 1;
```

[Declaration](sys/freebsd/common/fs.wave#L46)

```wave
pub const FS_W_OK: i32 = 2;
```

[Declaration](sys/freebsd/common/fs.wave#L47)

```wave
pub const FS_R_OK: i32 = 4;
```

[Declaration](sys/freebsd/common/fs.wave#L50)

```wave
pub const FS_SEEK_SET: i32 = 0;
```

[Declaration](sys/freebsd/common/fs.wave#L51)

```wave
pub const FS_SEEK_CUR: i32 = 1;
```

[Declaration](sys/freebsd/common/fs.wave#L52)

```wave
pub const FS_SEEK_END: i32 = 2;
```

[Declaration](sys/freebsd/common/fs.wave#L55)

```wave
pub const FS_F_GETFL: i32 = 3;
```

[Declaration](sys/freebsd/common/fs.wave#L56)

```wave
pub const FS_F_SETFL: i32 = 4;
```

[Declaration](sys/freebsd/common/fs.wave#L59)

```wave
pub struct Stat {
    dev: u64;
    ino: u64;
    nlink: u64;
    mode: u16;
    bsdflags: i16;
    uid: u32;
    gid: u32;
    padding1: i32;
    rdev: u64;
    atime: i64;
    atime_nsec: i64;
    mtime: i64;
    mtime_nsec: i64;
    ctime: i64;
    ctime_nsec: i64;
    birthtime: i64;
    birthtime_nsec: i64;
    size: i64;
    blocks: i64;
    blksize: i32;
    flags: u32;
    generation: u64;
    file_revision: u64;
    spare: array<u64, 9>;
}
```

[Declaration](sys/freebsd/common/fs.wave#L86)

```wave
pub fun open(path: str, flags: i32, mode: i32) -> i64
```

[Declaration](sys/freebsd/common/fs.wave#L90)

```wave
pub fun close(fd: i64) -> i64
```

[Declaration](sys/freebsd/common/fs.wave#L94)

```wave
pub fun dup(fd: i64) -> i64
```

[Declaration](sys/freebsd/common/fs.wave#L98)

```wave
pub fun pipe(fds: ptr<i32>) -> i64
```

[Declaration](sys/freebsd/common/fs.wave#L102)

```wave
pub fun dup2(oldfd: i64, newfd: i64) -> i64
```

[Declaration](sys/freebsd/common/fs.wave#L106)

```wave
pub fun fsync(fd: i64) -> i64
```

[Declaration](sys/freebsd/common/fs.wave#L110)

```wave
pub fun fcntl(fd: i64, cmd: i32, arg: i64) -> i64
```

[Declaration](sys/freebsd/common/fs.wave#L114)

```wave
pub fun read(fd: i64, buf: ptr<u8>, len: i64) -> i64
```

[Declaration](sys/freebsd/common/fs.wave#L118)

```wave
pub fun write(fd: i64, buf: ptr<u8>, len: i64) -> i64
```

[Declaration](sys/freebsd/common/fs.wave#L122)

```wave
pub fun getcwd(buf: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/freebsd/common/fs.wave#L126)

```wave
pub fun chdir(path: str) -> i64
```

[Declaration](sys/freebsd/common/fs.wave#L130)

```wave
pub fun access(path: str, mode: i32) -> i64
```

[Declaration](sys/freebsd/common/fs.wave#L134)

```wave
pub fun lseek(fd: i64, offset: i64, whence: i32) -> i64
```

[Declaration](sys/freebsd/common/fs.wave#L138)

```wave
pub fun unlink(path: str) -> i64
```

[Declaration](sys/freebsd/common/fs.wave#L142)

```wave
pub fun mkdir(path: str, mode: i32) -> i64
```

[Declaration](sys/freebsd/common/fs.wave#L146)

```wave
pub fun rmdir(path: str) -> i64
```

[Declaration](sys/freebsd/common/fs.wave#L150)

```wave
pub fun rename_path(old_path: str, new_path: str) -> i64
```

[Declaration](sys/freebsd/common/fs.wave#L154)

```wave
pub fun stat(path: str, st: ptr<Stat>) -> i64
```

[Declaration](sys/freebsd/common/fs.wave#L158)

```wave
pub fun fstat(fd: i64, st: ptr<Stat>) -> i64
```

[Declaration](sys/freebsd/common/fs.wave#L163)

```wave
pub fun ftruncate(fd: i64, length: i64) -> i64
```

[Declaration](sys/freebsd/common/fs.wave#L171)

```wave
pub fun same_file(first_fd: i64, second_fd: i64) -> i64
```

## `std::sys::freebsd::common::memory`

[Source](sys/freebsd/common/memory.wave)

[Declaration](sys/freebsd/common/memory.wave#L34)

```wave
pub const PROT_READ: i32 = 1;
```

[Declaration](sys/freebsd/common/memory.wave#L35)

```wave
pub const PROT_WRITE: i32 = 2;
```

[Declaration](sys/freebsd/common/memory.wave#L36)

```wave
pub const MAP_PRIVATE: i32 = 2;
```

[Declaration](sys/freebsd/common/memory.wave#L37)

```wave
pub const MAP_ANON: i32 = 0x1000;
```

[Declaration](sys/freebsd/common/memory.wave#L38)

```wave
pub const MAP_ANONYMOUS: i32 = MAP_ANON;
```

[Declaration](sys/freebsd/common/memory.wave#L40)

```wave
pub fun mmap(
    addr: ptr<i8>,
    length: i64,
    prot: i32,
    flags: i32,
    fd: i64,
    offset: i64
) -> ptr<i8>
```

[Declaration](sys/freebsd/common/memory.wave#L59)

```wave
pub fun munmap(addr: ptr<i8>, length: i64) -> i64
```

[Declaration](sys/freebsd/common/memory.wave#L63)

```wave
pub fun brk(addr: ptr<i8>) -> ptr<i8>
```

[Declaration](sys/freebsd/common/memory.wave#L67)

```wave
pub fun sys_alloc(size: i64) -> ptr<u8>
```

[Declaration](sys/freebsd/common/memory.wave#L88)

```wave
pub fun sys_free(p: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/freebsd/common/memory.wave#L96)

```wave
pub fun sys_page_size() -> i64
```

## `std::sys::freebsd::common::process`

[Source](sys/freebsd/common/process.wave)

[Declaration](sys/freebsd/common/process.wave#L33)

```wave
pub fun exit(code: i32) -> !
```

[Declaration](sys/freebsd/common/process.wave#L38)

```wave
pub fun getpid() -> i64
```

[Declaration](sys/freebsd/common/process.wave#L42)

```wave
pub fun getppid() -> i64
```

[Declaration](sys/freebsd/common/process.wave#L46)

```wave
pub fun fork() -> i64
```

[Declaration](sys/freebsd/common/process.wave#L50)

```wave
pub fun execve(
    path: str,
    argv: ptr<ptr<i8>>,
    envp: ptr<ptr<i8>>
) -> i64
```

[Declaration](sys/freebsd/common/process.wave#L58)

```wave
pub fun waitpid(pid: i64, status: ptr<i32>, options: i32) -> i64
```

[Declaration](sys/freebsd/common/process.wave#L65)

```wave
pub fun kill(pid: i64, sig: i32) -> i64
```

## `std::sys::freebsd::common::socket`

[Source](sys/freebsd/common/socket.wave)

[Declaration](sys/freebsd/common/socket.wave#L37)

```wave
pub const AF_INET: i32 = 2;
```

[Declaration](sys/freebsd/common/socket.wave#L38)

```wave
pub const AF_INET6: i32 = 28;
```

[Declaration](sys/freebsd/common/socket.wave#L40)

```wave
pub const SOCK_STREAM: i32 = 1;
```

[Declaration](sys/freebsd/common/socket.wave#L41)

```wave
pub const SOCK_DGRAM: i32 = 2;
```

[Declaration](sys/freebsd/common/socket.wave#L43)

```wave
pub const IPPROTO_IP: i32 = 0;
```

[Declaration](sys/freebsd/common/socket.wave#L44)

```wave
pub const IPPROTO_IPV6: i32 = 41;
```

[Declaration](sys/freebsd/common/socket.wave#L45)

```wave
pub const IPPROTO_TCP: i32 = 6;
```

[Declaration](sys/freebsd/common/socket.wave#L46)

```wave
pub const IPPROTO_UDP: i32 = 17;
```

[Declaration](sys/freebsd/common/socket.wave#L48)

```wave
pub const SHUT_RD: i32 = 0;
```

[Declaration](sys/freebsd/common/socket.wave#L49)

```wave
pub const SHUT_WR: i32 = 1;
```

[Declaration](sys/freebsd/common/socket.wave#L50)

```wave
pub const SHUT_RDWR: i32 = 2;
```

[Declaration](sys/freebsd/common/socket.wave#L52)

```wave
pub const SOL_SOCKET: i32 = 0xFFFF;
```

[Declaration](sys/freebsd/common/socket.wave#L53)

```wave
pub const SO_REUSEADDR: i32 = 0x0004;
```

[Declaration](sys/freebsd/common/socket.wave#L54)

```wave
pub const SO_REUSEPORT: i32 = 0x0200;
```

[Declaration](sys/freebsd/common/socket.wave#L55)

```wave
pub const SO_ERROR: i32 = 0x1007;
```

[Declaration](sys/freebsd/common/socket.wave#L56)

```wave
pub const SO_RCVBUF: i32 = 0x1002;
```

[Declaration](sys/freebsd/common/socket.wave#L57)

```wave
pub const SO_SNDBUF: i32 = 0x1001;
```

[Declaration](sys/freebsd/common/socket.wave#L58)

```wave
pub const SO_BROADCAST: i32 = 0x0020;
```

[Declaration](sys/freebsd/common/socket.wave#L59)

```wave
pub const SO_KEEPALIVE: i32 = 0x0008;
```

[Declaration](sys/freebsd/common/socket.wave#L60)

```wave
pub const TCP_NODELAY: i32 = 1;
```

[Declaration](sys/freebsd/common/socket.wave#L61)

```wave
pub const IP_TTL: i32 = 4;
```

[Declaration](sys/freebsd/common/socket.wave#L62)

```wave
pub const IP_MULTICAST_IF: i32 = 9;
```

[Declaration](sys/freebsd/common/socket.wave#L63)

```wave
pub const IP_MULTICAST_TTL: i32 = 10;
```

[Declaration](sys/freebsd/common/socket.wave#L64)

```wave
pub const IP_MULTICAST_LOOP: i32 = 11;
```

[Declaration](sys/freebsd/common/socket.wave#L65)

```wave
pub const IP_ADD_MEMBERSHIP: i32 = 12;
```

[Declaration](sys/freebsd/common/socket.wave#L66)

```wave
pub const IP_DROP_MEMBERSHIP: i32 = 13;
```

[Declaration](sys/freebsd/common/socket.wave#L67)

```wave
pub const IPV6_MULTICAST_IF: i32 = 9;
```

[Declaration](sys/freebsd/common/socket.wave#L68)

```wave
pub const IPV6_MULTICAST_HOPS: i32 = 10;
```

[Declaration](sys/freebsd/common/socket.wave#L69)

```wave
pub const IPV6_MULTICAST_LOOP: i32 = 11;
```

[Declaration](sys/freebsd/common/socket.wave#L70)

```wave
pub const IPV6_JOIN_GROUP: i32 = 12;
```

[Declaration](sys/freebsd/common/socket.wave#L71)

```wave
pub const IPV6_LEAVE_GROUP: i32 = 13;
```

[Declaration](sys/freebsd/common/socket.wave#L72)

```wave
pub const MSG_NOSIGNAL: i32 = 0x20000;
```

[Declaration](sys/freebsd/common/socket.wave#L73)

```wave
pub const MSG_DONTWAIT: i32 = 128;
```

[Declaration](sys/freebsd/common/socket.wave#L75)

```wave
pub const POLLIN: i16 = 0x001;
```

[Declaration](sys/freebsd/common/socket.wave#L76)

```wave
pub const POLLOUT: i16 = 0x004;
```

[Declaration](sys/freebsd/common/socket.wave#L77)

```wave
pub const POLLERR: i16 = 0x008;
```

[Declaration](sys/freebsd/common/socket.wave#L78)

```wave
pub const POLLHUP: i16 = 0x010;
```

[Declaration](sys/freebsd/common/socket.wave#L79)

```wave
pub const POLLNVAL: i16 = 0x020;
```

[Declaration](sys/freebsd/common/socket.wave#L81)

```wave
pub struct PollFd {
    fd: i32;
    events: i16;
    revents: i16;
}
```

[Declaration](sys/freebsd/common/socket.wave#L87)

```wave
pub fun socket(domain: i32, ty: i32, protocol: i32) -> i64
```

[Declaration](sys/freebsd/common/socket.wave#L91)

```wave
pub fun bind(fd: i64, addr: ptr<i8>, len: i32) -> i64
```

[Declaration](sys/freebsd/common/socket.wave#L95)

```wave
pub fun listen(fd: i64, backlog: i32) -> i64
```

[Declaration](sys/freebsd/common/socket.wave#L99)

```wave
pub fun accept(fd: i64, addr: ptr<i8>, len: ptr<i32>) -> i64
```

[Declaration](sys/freebsd/common/socket.wave#L103)

```wave
pub fun connect(fd: i64, addr: ptr<i8>, len: i32) -> i64
```

[Declaration](sys/freebsd/common/socket.wave#L107)

```wave
pub fun getsockname(fd: i64, addr: ptr<i8>, len: ptr<i32>) -> i64
```

[Declaration](sys/freebsd/common/socket.wave#L111)

```wave
pub fun getpeername(fd: i64, addr: ptr<i8>, len: ptr<i32>) -> i64
```

[Declaration](sys/freebsd/common/socket.wave#L115)

```wave
pub fun shutdown(fd: i64, how: i32) -> i64
```

[Declaration](sys/freebsd/common/socket.wave#L119)

```wave
pub fun close_socket(fd: i64) -> i64
```

[Declaration](sys/freebsd/common/socket.wave#L121)

```wave
pub fun setsockopt(
    fd: i64,
    level: i32,
    optname: i32,
    optval: ptr<i8>,
    optlen: i32
) -> i64
```

[Declaration](sys/freebsd/common/socket.wave#L138)

```wave
pub fun getsockopt(
    fd: i64,
    level: i32,
    optname: i32,
    optval: ptr<i8>,
    optlen: ptr<i32>
) -> i64
```

[Declaration](sys/freebsd/common/socket.wave#L155)

```wave
pub fun poll(fds: ptr<PollFd>, nfds: i64, timeout_ms: i32) -> i64
```

[Declaration](sys/freebsd/common/socket.wave#L159)

```wave
pub fun send(fd: i64, buf: ptr<u8>, len: i64, flags: i32) -> i64
```

[Declaration](sys/freebsd/common/socket.wave#L163)

```wave
pub fun recv(fd: i64, buf: ptr<u8>, len: i64, flags: i32) -> i64
```

[Declaration](sys/freebsd/common/socket.wave#L167)

```wave
pub fun sendto(
    fd: i64,
    buf: ptr<u8>,
    len: i64,
    flags: i32,
    addr: ptr<i8>,
    addrlen: i32
) -> i64
```

[Declaration](sys/freebsd/common/socket.wave#L186)

```wave
pub fun recvfrom(
    fd: i64,
    buf: ptr<u8>,
    len: i64,
    flags: i32,
    addr: ptr<i8>,
    addrlen: ptr<i32>
) -> i64
```

## `std::sys::freebsd::common::time`

[Source](sys/freebsd/common/time.wave)

[Declaration](sys/freebsd/common/time.wave#L33)

```wave
pub struct TimeSpec {
    sec: i64;
    nsec: i64;
}
```

[Declaration](sys/freebsd/common/time.wave#L38)

```wave
pub fun nanosleep(req: ptr<TimeSpec>, rem: ptr<TimeSpec>) -> i64
```

[Declaration](sys/freebsd/common/time.wave#L42)

```wave
pub fun clock_gettime(clock_id: i32, tp: ptr<TimeSpec>) -> i64
```

## `std::sys::freebsd::common::tty`

[Source](sys/freebsd/common/tty.wave)

[Declaration](sys/freebsd/common/tty.wave#L33)

```wave
pub const TTY_SYS_IOCTL: i64 = 54;
```

[Declaration](sys/freebsd/common/tty.wave#L34)

```wave
pub const TTY_SYS_FCNTL: i64 = 92;
```

[Declaration](sys/freebsd/common/tty.wave#L36)

```wave
pub const TTY_TCGETS: i64 = 0x402c7413;
```

[Declaration](sys/freebsd/common/tty.wave#L37)

```wave
pub const TTY_TCSETS: i64 = 0x802c7414;
```

[Declaration](sys/freebsd/common/tty.wave#L38)

```wave
pub const TTY_TCSETSW: i64 = 0x802c7415;
```

[Declaration](sys/freebsd/common/tty.wave#L39)

```wave
pub const TTY_TCSETSF: i64 = 0x802c7416;
```

[Declaration](sys/freebsd/common/tty.wave#L41)

```wave
pub const TTY_TCSANOW: i32 = 0;
```

[Declaration](sys/freebsd/common/tty.wave#L42)

```wave
pub const TTY_TCSADRAIN: i32 = 1;
```

[Declaration](sys/freebsd/common/tty.wave#L43)

```wave
pub const TTY_TCSAFLUSH: i32 = 2;
```

[Declaration](sys/freebsd/common/tty.wave#L45)

```wave
pub const TTY_F_GETFL: i32 = 3;
```

[Declaration](sys/freebsd/common/tty.wave#L46)

```wave
pub const TTY_F_SETFL: i32 = 4;
```

[Declaration](sys/freebsd/common/tty.wave#L47)

```wave
pub const TTY_O_NONBLOCK: i32 = 4;
```

[Declaration](sys/freebsd/common/tty.wave#L49)

```wave
pub const TTY_ICANON: u32 = 0x00000100;
```

[Declaration](sys/freebsd/common/tty.wave#L50)

```wave
pub const TTY_ECHO: u32 = 0x00000008;
```

[Declaration](sys/freebsd/common/tty.wave#L51)

```wave
pub const TTY_VMIN_IDX: i32 = 16;
```

[Declaration](sys/freebsd/common/tty.wave#L52)

```wave
pub const TTY_VTIME_IDX: i32 = 17;
```

[Declaration](sys/freebsd/common/tty.wave#L54)

```wave
pub struct Termios {
    c_iflag: u32;
    c_oflag: u32;
    c_cflag: u32;
    c_lflag: u32;
    c_cc: array<u8, 20>;
    c_ispeed: u32;
    c_ospeed: u32;
}
```

[Declaration](sys/freebsd/common/tty.wave#L64)

```wave
pub struct TtyRawState {
    term: Termios;
    flags: i32;
    active: i32;
}
```

[Declaration](sys/freebsd/common/tty.wave#L70)

```wave
pub fun tty_getattr(fd: i32, t: ptr<Termios>) -> i64
```

[Declaration](sys/freebsd/common/tty.wave#L74)

```wave
pub fun tty_setattr(fd: i32, action: i32, t: ptr<Termios>) -> i64
```

[Declaration](sys/freebsd/common/tty.wave#L87)

```wave
pub fun tty_getfl(fd: i32) -> i64
```

[Declaration](sys/freebsd/common/tty.wave#L91)

```wave
pub fun tty_setfl(fd: i32, flags: i32) -> i64
```

[Declaration](sys/freebsd/common/tty.wave#L95)

```wave
pub fun tty_enable_raw_nonblock(fd: i32, st: ptr<TtyRawState>) -> i64
```

[Declaration](sys/freebsd/common/tty.wave#L132)

```wave
pub fun tty_restore(fd: i32, st: ptr<TtyRawState>)
```

## `std::sys::freebsd::env`

[Source](sys/freebsd/env.wave)

[Declaration](sys/freebsd/env.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::freebsd::amd64::env")::{env_read};
```

[Declaration](sys/freebsd/env.wave#L11)

```wave
#[target(arch="aarch64")]
pub import("std::sys::freebsd::arm64::env")::{
    env_read
};
```

[Declaration](sys/freebsd/env.wave#L16)

```wave
#[target(arch="riscv64")]
pub import("std::sys::freebsd::riscv64::env")::{
    env_read
};
```

## `std::sys::freebsd::event`

[Source](sys/freebsd/event.wave)

[Declaration](sys/freebsd/event.wave#L6)

```wave
pub const EVENT_BACKEND_KIND: i32 = 2;
```

[Declaration](sys/freebsd/event.wave#L7)

```wave
pub const EVENT_READABLE: i32 = 1;
```

[Declaration](sys/freebsd/event.wave#L8)

```wave
pub const EVENT_WRITABLE: i32 = 2;
```

[Declaration](sys/freebsd/event.wave#L9)

```wave
pub const EVENT_ERROR: i32 = 4;
```

[Declaration](sys/freebsd/event.wave#L10)

```wave
pub const EVENT_HANGUP: i32 = 8;
```

[Declaration](sys/freebsd/event.wave#L18)

```wave
pub struct NativeEvent { fd: i64; token: u64; flags: i32; }
```

[Declaration](sys/freebsd/event.wave#L19)

```wave
pub struct NativeKevent {
    ident: u64; filter: i16; flags: u16; filter_flags: u32;
    data: i64; user_data: ptr<i8>;
    ext: array<u64, 4>;
}
```

[Declaration](sys/freebsd/event.wave#L24)

```wave
pub struct NativeTimeSpec { sec: i64; nsec: i64; }
```

[Declaration](sys/freebsd/event.wave#L62)

```wave
pub fun event_create(capacity: i32) -> i64
```

[Declaration](sys/freebsd/event.wave#L66)

```wave
pub fun event_add(handle: i64, fd: i64, token: u64, flags: i32) -> i64
```

[Declaration](sys/freebsd/event.wave#L69)

```wave
pub fun event_modify(handle: i64, fd: i64, token: u64, flags: i32) -> i64
```

[Declaration](sys/freebsd/event.wave#L75)

```wave
pub fun event_remove(handle: i64, fd: i64) -> i64
```

[Declaration](sys/freebsd/event.wave#L76)

```wave
pub fun event_wait(
    handle: i64, output: ptr<NativeEvent>, capacity: i32, timeout_ms: i32
) -> i64
```

[Declaration](sys/freebsd/event.wave#L106)

```wave
pub fun event_close(handle: i64) -> i64
```

## `std::sys::freebsd::fs`

[Source](sys/freebsd/fs.wave)

[Declaration](sys/freebsd/fs.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::freebsd::amd64::fs")::{
    FS_O_RDONLY, FS_O_WRONLY, FS_O_RDWR, FS_O_CREAT, FS_O_EXCL, FS_O_TRUNC, FS_O_APPEND,
    FS_O_NONBLOCK, FS_F_OK, FS_X_OK, FS_W_OK, FS_R_OK, FS_SEEK_SET, FS_SEEK_CUR, FS_SEEK_END,
    FS_F_GETFL, FS_F_SETFL, open, close, dup, dup2, pipe, fsync, fcntl, read, write, getcwd,
    chdir, access, lseek, unlink, mkdir, rmdir, rename_path, Stat, stat, fstat, ftruncate, same_file,
};
```

[Declaration](sys/freebsd/fs.wave#L16)

```wave
#[target(arch="aarch64")]
pub import("std::sys::freebsd::arm64::fs")::{
    FS_O_RDONLY, FS_O_WRONLY, FS_O_RDWR, FS_O_NONBLOCK, FS_O_APPEND, FS_O_CREAT, FS_O_TRUNC,
    FS_O_EXCL, FS_F_OK, FS_X_OK, FS_W_OK, FS_R_OK, FS_SEEK_SET, FS_SEEK_CUR, FS_SEEK_END,
    FS_F_GETFL, FS_F_SETFL, Stat, open, close, dup, pipe, dup2, fsync, fcntl, read, write,
    getcwd, chdir, access, lseek, unlink, mkdir, rmdir, rename_path, stat, fstat, ftruncate, same_file
};
```

[Declaration](sys/freebsd/fs.wave#L24)

```wave
#[target(arch="riscv64")]
pub import("std::sys::freebsd::riscv64::fs")::{
    FS_O_RDONLY, FS_O_WRONLY, FS_O_RDWR, FS_O_NONBLOCK, FS_O_APPEND, FS_O_CREAT, FS_O_TRUNC,
    FS_O_EXCL, FS_F_OK, FS_X_OK, FS_W_OK, FS_R_OK, FS_SEEK_SET, FS_SEEK_CUR, FS_SEEK_END,
    FS_F_GETFL, FS_F_SETFL, Stat, open, close, dup, pipe, dup2, fsync, fcntl, read, write,
    getcwd, chdir, access, lseek, unlink, mkdir, rmdir, rename_path, stat, fstat, ftruncate, same_file
};
```

## `std::sys::freebsd::interfaces`

[Source](sys/freebsd/interfaces.wave)

[Declaration](sys/freebsd/interfaces.wave#L1)

```wave
pub struct NativeIfAddrs {
    next: ptr<NativeIfAddrs>;
    name: ptr<u8>;
    flags: u32;
    address: ptr<i8>;
    netmask: ptr<i8>;
    destination: ptr<i8>;
    data: ptr<i8>;
}
```

[Declaration](sys/freebsd/interfaces.wave#L11)

```wave
pub struct NativeSockAddrHeader { len: u8; family: u8; }
```

[Declaration](sys/freebsd/interfaces.wave#L18)

```wave
pub fun interfaces_open(output: ptr<ptr<NativeIfAddrs>>) -> i64
```

[Declaration](sys/freebsd/interfaces.wave#L25)

```wave
pub fun interfaces_close(first: ptr<NativeIfAddrs>)
```

[Declaration](sys/freebsd/interfaces.wave#L28)

```wave
pub fun interface_next(value: ptr<NativeIfAddrs>) -> ptr<NativeIfAddrs>
```

[Declaration](sys/freebsd/interfaces.wave#L29)

```wave
pub fun interface_name(value: ptr<NativeIfAddrs>) -> ptr<u8>
```

[Declaration](sys/freebsd/interfaces.wave#L30)

```wave
pub fun interface_flags(value: ptr<NativeIfAddrs>) -> u32
```

[Declaration](sys/freebsd/interfaces.wave#L31)

```wave
pub fun interface_address(value: ptr<NativeIfAddrs>) -> ptr<i8>
```

[Declaration](sys/freebsd/interfaces.wave#L32)

```wave
pub fun interface_family(value: ptr<NativeIfAddrs>) -> i32
```

[Declaration](sys/freebsd/interfaces.wave#L38)

```wave
pub fun interface_index(name: ptr<u8>) -> u32
```

## `std::sys::freebsd::local_addr`

[Source](sys/freebsd/local_addr.wave)

[Declaration](sys/freebsd/local_addr.wave#L1)

```wave
pub const AF_LOCAL: i32 = 1;
```

[Declaration](sys/freebsd/local_addr.wave#L2)

```wave
pub const NATIVE_LOCAL_PATH_CAPACITY: i32 = 104;
```

[Declaration](sys/freebsd/local_addr.wave#L4)

```wave
pub struct NativeLocalAddr {
    len: u8;
    family: u8;
    path: array<u8, 104>;
}
```

[Declaration](sys/freebsd/local_addr.wave#L10)

```wave
pub fun native_local_addr(path: str, output: ptr<NativeLocalAddr>) -> i32
```

[Declaration](sys/freebsd/local_addr.wave#L26)

```wave
pub fun native_local_abstract(name: str, output: ptr<NativeLocalAddr>) -> i32
```

## `std::sys::freebsd::memory`

[Source](sys/freebsd/memory.wave)

[Declaration](sys/freebsd/memory.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::freebsd::amd64::memory")::{
    PROT_READ, PROT_WRITE, MAP_PRIVATE, MAP_ANONYMOUS, mmap, munmap, brk, sys_alloc, sys_free,
    sys_page_size,
};
```

[Declaration](sys/freebsd/memory.wave#L14)

```wave
#[target(arch="aarch64")]
pub import("std::sys::freebsd::arm64::memory")::{
    PROT_READ, PROT_WRITE, MAP_PRIVATE, MAP_ANON, MAP_ANONYMOUS, mmap, munmap, brk, sys_alloc,
    sys_free, sys_page_size
};
```

[Declaration](sys/freebsd/memory.wave#L20)

```wave
#[target(arch="riscv64")]
pub import("std::sys::freebsd::riscv64::memory")::{
    PROT_READ, PROT_WRITE, MAP_PRIVATE, MAP_ANON, MAP_ANONYMOUS, mmap, munmap, brk, sys_alloc,
    sys_free, sys_page_size
};
```

## `std::sys::freebsd::process`

[Source](sys/freebsd/process.wave)

[Declaration](sys/freebsd/process.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::freebsd::amd64::process")::{
    exit, getpid, getppid, fork, execve, waitpid, kill,
};
```

[Declaration](sys/freebsd/process.wave#L13)

```wave
#[target(arch="aarch64")]
pub import("std::sys::freebsd::arm64::process")::{
    exit, getpid, getppid, fork, execve, waitpid, kill
};
```

[Declaration](sys/freebsd/process.wave#L18)

```wave
#[target(arch="riscv64")]
pub import("std::sys::freebsd::riscv64::process")::{
    exit, getpid, getppid, fork, execve, waitpid, kill
};
```

## `std::sys::freebsd::resolver`

[Source](sys/freebsd/resolver.wave)

[Declaration](sys/freebsd/resolver.wave#L3)

```wave
pub struct NativeAddrInfo {
    flags: i32;
    family: i32;
    socket_type: i32;
    protocol: i32;
    address_length: u32;
    canonical_name: ptr<i8>;
    address: ptr<i8>;
    next: ptr<NativeAddrInfo>;
}
```

[Declaration](sys/freebsd/resolver.wave#L19)

```wave
pub fun resolver_open(
    host: str, service: str, socket_type: i32, protocol: i32,
    result: ptr<ptr<NativeAddrInfo>>
) -> i64
```

[Declaration](sys/freebsd/resolver.wave#L36)

```wave
pub fun resolver_close(result: ptr<NativeAddrInfo>)
```

[Declaration](sys/freebsd/resolver.wave#L40)

```wave
pub fun resolver_family(result: ptr<NativeAddrInfo>) -> i32
```

[Declaration](sys/freebsd/resolver.wave#L41)

```wave
pub fun resolver_address(result: ptr<NativeAddrInfo>) -> ptr<i8>
```

[Declaration](sys/freebsd/resolver.wave#L42)

```wave
pub fun resolver_address_length(result: ptr<NativeAddrInfo>) -> i64
```

[Declaration](sys/freebsd/resolver.wave#L45)

```wave
pub fun resolver_next(result: ptr<NativeAddrInfo>) -> ptr<NativeAddrInfo>
```

## `std::sys::freebsd::riscv64::env`

[Source](sys/freebsd/riscv64/env.wave)

[Declaration](sys/freebsd/riscv64/env.wave#L6)

```wave
pub import("std::sys::freebsd::common::env")::{
    ERR_NOT_IMPLEMENTED, env_read
};
```

## `std::sys::freebsd::riscv64::fs`

[Source](sys/freebsd/riscv64/fs.wave)

[Declaration](sys/freebsd/riscv64/fs.wave#L6)

```wave
pub import("std::sys::freebsd::common::fs")::{
    FS_O_RDONLY, FS_O_WRONLY, FS_O_RDWR, FS_O_NONBLOCK, FS_O_APPEND, FS_O_CREAT, FS_O_TRUNC,
    FS_O_EXCL, FS_F_OK, FS_X_OK, FS_W_OK, FS_R_OK, FS_SEEK_SET, FS_SEEK_CUR, FS_SEEK_END,
    FS_F_GETFL, FS_F_SETFL, Stat, open, close, dup, pipe, dup2, fsync, fcntl, read, write,
    getcwd, chdir, access, lseek, unlink, mkdir, rmdir, rename_path, stat, fstat, ftruncate, same_file
};
```

## `std::sys::freebsd::riscv64::memory`

[Source](sys/freebsd/riscv64/memory.wave)

[Declaration](sys/freebsd/riscv64/memory.wave#L6)

```wave
pub import("std::sys::freebsd::common::memory")::{
    PROT_READ, PROT_WRITE, MAP_PRIVATE, MAP_ANON, MAP_ANONYMOUS, mmap, munmap, brk, sys_alloc,
    sys_free, sys_page_size
};
```

## `std::sys::freebsd::riscv64::process`

[Source](sys/freebsd/riscv64/process.wave)

[Declaration](sys/freebsd/riscv64/process.wave#L6)

```wave
pub import("std::sys::freebsd::common::process")::{
    exit, getpid, getppid, fork, execve, waitpid, kill
};
```

## `std::sys::freebsd::riscv64::socket`

[Source](sys/freebsd/riscv64/socket.wave)

[Declaration](sys/freebsd/riscv64/socket.wave#L6)

```wave
pub import("std::sys::freebsd::common::socket")::{
    AF_INET, AF_INET6, SOCK_STREAM, SOCK_DGRAM, IPPROTO_IP, IPPROTO_IPV6, IPPROTO_TCP,
    IPPROTO_UDP, SHUT_RD, SHUT_WR, SHUT_RDWR, SOL_SOCKET, SO_REUSEADDR, SO_REUSEPORT, SO_ERROR,
    SO_RCVBUF, SO_SNDBUF, SO_BROADCAST, SO_KEEPALIVE, TCP_NODELAY, IP_TTL, IP_MULTICAST_IF,
    IP_MULTICAST_TTL, IP_MULTICAST_LOOP, IP_ADD_MEMBERSHIP, IP_DROP_MEMBERSHIP,
    IPV6_MULTICAST_IF, IPV6_MULTICAST_HOPS, IPV6_MULTICAST_LOOP, IPV6_JOIN_GROUP,
    IPV6_LEAVE_GROUP, MSG_NOSIGNAL, MSG_DONTWAIT, POLLIN, POLLOUT, POLLERR, POLLHUP, POLLNVAL, PollFd, socket,
    bind, listen, accept, connect, getsockname, getpeername, shutdown, close_socket, setsockopt,
    getsockopt, poll, send, recv, sendto, recvfrom
};
```

## `std::sys::freebsd::riscv64::syscall`

[Source](sys/freebsd/riscv64/syscall.wave)

[Declaration](sys/freebsd/riscv64/syscall.wave#L7)

```wave
pub fun syscall0(id: i64) -> i64
```

[Declaration](sys/freebsd/riscv64/syscall.wave#L26)

```wave
pub fun syscall1(id: i64, a1: i64) -> i64
```

[Declaration](sys/freebsd/riscv64/syscall.wave#L45)

```wave
pub fun syscall2(id: i64, a1: i64, a2: i64) -> i64
```

[Declaration](sys/freebsd/riscv64/syscall.wave#L65)

```wave
pub fun syscall3(id: i64, a1: i64, a2: i64, a3: i64) -> i64
```

[Declaration](sys/freebsd/riscv64/syscall.wave#L86)

```wave
pub fun syscall4(id: i64, a1: i64, a2: i64, a3: i64, a4: i64) -> i64
```

[Declaration](sys/freebsd/riscv64/syscall.wave#L108)

```wave
pub fun syscall5(id: i64, a1: i64, a2: i64, a3: i64, a4: i64, a5: i64) -> i64
```

[Declaration](sys/freebsd/riscv64/syscall.wave#L131)

```wave
pub fun syscall6(id: i64, a1: i64, a2: i64, a3: i64, a4: i64, a5: i64, a6: i64) -> i64
```

## `std::sys::freebsd::riscv64::time`

[Source](sys/freebsd/riscv64/time.wave)

[Declaration](sys/freebsd/riscv64/time.wave#L6)

```wave
pub import("std::sys::freebsd::common::time")::{
    TimeSpec, nanosleep, clock_gettime
};
```

## `std::sys::freebsd::riscv64::tty`

[Source](sys/freebsd/riscv64/tty.wave)

[Declaration](sys/freebsd/riscv64/tty.wave#L6)

```wave
pub import("std::sys::freebsd::common::tty")::{
    TTY_SYS_IOCTL, TTY_SYS_FCNTL, TTY_TCGETS, TTY_TCSETS, TTY_TCSETSW, TTY_TCSETSF, TTY_TCSANOW,
    TTY_TCSADRAIN, TTY_TCSAFLUSH, TTY_F_GETFL, TTY_F_SETFL, TTY_O_NONBLOCK, TTY_ICANON,
    TTY_ECHO, TTY_VMIN_IDX, TTY_VTIME_IDX, Termios, TtyRawState, tty_getattr, tty_setattr,
    tty_getfl, tty_setfl, tty_enable_raw_nonblock, tty_restore
};
```

## `std::sys::freebsd::socket`

[Source](sys/freebsd/socket.wave)

[Declaration](sys/freebsd/socket.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::freebsd::amd64::socket")::{
    AF_INET, AF_INET6, SOCK_STREAM, SOCK_DGRAM, IPPROTO_IP, IPPROTO_IPV6, IPPROTO_TCP,
    IPPROTO_UDP, SHUT_RD, SHUT_WR, SHUT_RDWR, SOL_SOCKET, SO_REUSEADDR, SO_REUSEPORT, SO_ERROR,
    SO_RCVBUF, SO_SNDBUF, SO_BROADCAST, SO_KEEPALIVE, TCP_NODELAY, IP_TTL, IP_MULTICAST_IF,
    IP_MULTICAST_TTL, IP_MULTICAST_LOOP, IP_ADD_MEMBERSHIP, IP_DROP_MEMBERSHIP,
    IPV6_MULTICAST_IF, IPV6_MULTICAST_HOPS, IPV6_MULTICAST_LOOP, IPV6_JOIN_GROUP,
    IPV6_LEAVE_GROUP, MSG_NOSIGNAL, MSG_DONTWAIT, POLLIN, POLLOUT, POLLERR, POLLHUP, POLLNVAL, PollFd, socket,
    bind, listen, accept, connect, getsockname, getpeername, shutdown, close_socket, setsockopt,
    getsockopt, poll, send, recv, sendto, recvfrom,
};
```

[Declaration](sys/freebsd/socket.wave#L20)

```wave
#[target(arch="aarch64")]
pub import("std::sys::freebsd::arm64::socket")::{
    AF_INET, AF_INET6, SOCK_STREAM, SOCK_DGRAM, IPPROTO_IP, IPPROTO_IPV6, IPPROTO_TCP,
    IPPROTO_UDP, SHUT_RD, SHUT_WR, SHUT_RDWR, SOL_SOCKET, SO_REUSEADDR, SO_REUSEPORT, SO_ERROR,
    SO_RCVBUF, SO_SNDBUF, SO_BROADCAST, SO_KEEPALIVE, TCP_NODELAY, IP_TTL, IP_MULTICAST_IF,
    IP_MULTICAST_TTL, IP_MULTICAST_LOOP, IP_ADD_MEMBERSHIP, IP_DROP_MEMBERSHIP,
    IPV6_MULTICAST_IF, IPV6_MULTICAST_HOPS, IPV6_MULTICAST_LOOP, IPV6_JOIN_GROUP,
    IPV6_LEAVE_GROUP, MSG_NOSIGNAL, MSG_DONTWAIT, POLLIN, POLLOUT, POLLERR, POLLHUP, POLLNVAL, PollFd, socket,
    bind, listen, accept, connect, getsockname, getpeername, shutdown, close_socket, setsockopt,
    getsockopt, poll, send, recv, sendto, recvfrom
};
```

[Declaration](sys/freebsd/socket.wave#L32)

```wave
#[target(arch="riscv64")]
pub import("std::sys::freebsd::riscv64::socket")::{
    AF_INET, AF_INET6, SOCK_STREAM, SOCK_DGRAM, IPPROTO_IP, IPPROTO_IPV6, IPPROTO_TCP,
    IPPROTO_UDP, SHUT_RD, SHUT_WR, SHUT_RDWR, SOL_SOCKET, SO_REUSEADDR, SO_REUSEPORT, SO_ERROR,
    SO_RCVBUF, SO_SNDBUF, SO_BROADCAST, SO_KEEPALIVE, TCP_NODELAY, IP_TTL, IP_MULTICAST_IF,
    IP_MULTICAST_TTL, IP_MULTICAST_LOOP, IP_ADD_MEMBERSHIP, IP_DROP_MEMBERSHIP,
    IPV6_MULTICAST_IF, IPV6_MULTICAST_HOPS, IPV6_MULTICAST_LOOP, IPV6_JOIN_GROUP,
    IPV6_LEAVE_GROUP, MSG_NOSIGNAL, MSG_DONTWAIT, POLLIN, POLLOUT, POLLERR, POLLHUP, POLLNVAL, PollFd, socket,
    bind, listen, accept, connect, getsockname, getpeername, shutdown, close_socket, setsockopt,
    getsockopt, poll, send, recv, sendto, recvfrom
};
```

## `std::sys::freebsd::socket_addr`

[Source](sys/freebsd/socket_addr.wave)

[Declaration](sys/freebsd/socket_addr.wave#L8)

```wave
pub const NATIVE_SOCKADDR_V4_LEN: i32 = 16;
```

[Declaration](sys/freebsd/socket_addr.wave#L9)

```wave
pub const NATIVE_SOCKADDR_V6_LEN: i32 = 28;
```

[Declaration](sys/freebsd/socket_addr.wave#L11)

```wave
pub struct NativeSockAddrV4 {
    len: u8;
    family: u8;
    port: u16;
    addr: u32;
    zero: array<u8, 8>;
}
```

[Declaration](sys/freebsd/socket_addr.wave#L19)

```wave
pub struct NativeSockAddrV6 {
    len: u8;
    family: u8;
    port: u16;
    flowinfo: u32;
    addr: array<u8, 16>;
    scope_id: u32;
}
```

[Declaration](sys/freebsd/socket_addr.wave#L28)

```wave
pub fun native_sockaddr_v4(ip_network: u32, port_network: u16) -> NativeSockAddrV4
```

[Declaration](sys/freebsd/socket_addr.wave#L38)

```wave
pub fun native_sockaddr_v4_ip(value: NativeSockAddrV4) -> u32
```

[Declaration](sys/freebsd/socket_addr.wave#L39)

```wave
pub fun native_sockaddr_v4_port(value: NativeSockAddrV4) -> u16
```

[Declaration](sys/freebsd/socket_addr.wave#L41)

```wave
pub fun native_sockaddr_v6(
    ip: ptr<u8>, port_network: u16, flowinfo: u32, scope_id: u32
) -> NativeSockAddrV6
```

[Declaration](sys/freebsd/socket_addr.wave#L55)

```wave
pub fun native_sockaddr_v6_copy_ip(value: NativeSockAddrV6, dst: ptr<u8>)
```

[Declaration](sys/freebsd/socket_addr.wave#L61)

```wave
pub fun native_sockaddr_v6_port(value: NativeSockAddrV6) -> u16
```

## `std::sys::freebsd::syscall`

[Source](sys/freebsd/syscall.wave)

[Declaration](sys/freebsd/syscall.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::freebsd::amd64::syscall")::{syscall0, syscall1, syscall2, syscall3, syscall4, syscall5, syscall6};
```

[Declaration](sys/freebsd/syscall.wave#L11)

```wave
#[target(arch="aarch64")]
pub import("std::sys::freebsd::arm64::syscall")::{syscall0, syscall1, syscall2, syscall3, syscall4, syscall5, syscall6};
```

[Declaration](sys/freebsd/syscall.wave#L14)

```wave
#[target(arch="riscv64")]
pub import("std::sys::freebsd::riscv64::syscall")::{syscall0, syscall1, syscall2, syscall3, syscall4, syscall5, syscall6};
```

## `std::sys::freebsd::time`

[Source](sys/freebsd/time.wave)

[Declaration](sys/freebsd/time.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::freebsd::amd64::time")::{TimeSpec, nanosleep, clock_gettime};
```

[Declaration](sys/freebsd/time.wave#L11)

```wave
#[target(arch="aarch64")]
pub import("std::sys::freebsd::arm64::time")::{
    TimeSpec, nanosleep, clock_gettime
};
```

[Declaration](sys/freebsd/time.wave#L16)

```wave
#[target(arch="riscv64")]
pub import("std::sys::freebsd::riscv64::time")::{
    TimeSpec, nanosleep, clock_gettime
};
```

## `std::sys::freebsd::tty`

[Source](sys/freebsd/tty.wave)

[Declaration](sys/freebsd/tty.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::freebsd::amd64::tty")::{
    TTY_SYS_IOCTL, TTY_SYS_FCNTL, TTY_TCGETS, TTY_TCSETS, TTY_TCSETSW, TTY_TCSETSF, TTY_TCSANOW,
    TTY_TCSADRAIN, TTY_TCSAFLUSH, TTY_F_GETFL, TTY_F_SETFL, TTY_O_NONBLOCK, TTY_ICANON,
    TTY_ECHO, TTY_VTIME_IDX, TTY_VMIN_IDX, Termios, TtyRawState, tty_getattr, tty_setattr,
    tty_getfl, tty_setfl, tty_enable_raw_nonblock, tty_restore,
};
```

[Declaration](sys/freebsd/tty.wave#L16)

```wave
#[target(arch="aarch64")]
pub import("std::sys::freebsd::arm64::tty")::{
    TTY_SYS_IOCTL, TTY_SYS_FCNTL, TTY_TCGETS, TTY_TCSETS, TTY_TCSETSW, TTY_TCSETSF, TTY_TCSANOW,
    TTY_TCSADRAIN, TTY_TCSAFLUSH, TTY_F_GETFL, TTY_F_SETFL, TTY_O_NONBLOCK, TTY_ICANON,
    TTY_ECHO, TTY_VMIN_IDX, TTY_VTIME_IDX, Termios, TtyRawState, tty_getattr, tty_setattr,
    tty_getfl, tty_setfl, tty_enable_raw_nonblock, tty_restore
};
```

[Declaration](sys/freebsd/tty.wave#L24)

```wave
#[target(arch="riscv64")]
pub import("std::sys::freebsd::riscv64::tty")::{
    TTY_SYS_IOCTL, TTY_SYS_FCNTL, TTY_TCGETS, TTY_TCSETS, TTY_TCSETSW, TTY_TCSETSF, TTY_TCSANOW,
    TTY_TCSADRAIN, TTY_TCSAFLUSH, TTY_F_GETFL, TTY_F_SETFL, TTY_O_NONBLOCK, TTY_ICANON,
    TTY_ECHO, TTY_VMIN_IDX, TTY_VTIME_IDX, Termios, TtyRawState, tty_getattr, tty_setattr,
    tty_getfl, tty_setfl, tty_enable_raw_nonblock, tty_restore
};
```

## `std::sys::freebsd::vector_io`

[Source](sys/freebsd/vector_io.wave)

[Declaration](sys/freebsd/vector_io.wave#L3)

```wave
pub struct NativeIoSlice { data: ptr<u8>; length: u64; }
```

[Declaration](sys/freebsd/vector_io.wave#L4)

```wave
pub struct NativeMessage {
    name: ptr<i8>;
    name_length: u32;
    vectors: ptr<NativeIoSlice>;
    vector_count: i32;
    control: ptr<i8>;
    control_length: u32;
    flags: i32;
}
```

[Declaration](sys/freebsd/vector_io.wave#L14)

```wave
pub fun native_io_slice(data: ptr<u8>, length: i64, output: ptr<NativeIoSlice>) -> i64
```

[Declaration](sys/freebsd/vector_io.wave#L25)

```wave
pub fun vector_send(fd: i64, vectors: ptr<NativeIoSlice>, count: i32, flags: i32) -> i64
```

[Declaration](sys/freebsd/vector_io.wave#L29)

```wave
pub fun vector_recv(fd: i64, vectors: ptr<NativeIoSlice>, count: i32, flags: i32) -> i64
```

## `std::sys::fs`

[Source](sys/fs.wave)

[Declaration](sys/fs.wave#L23)

```wave
#[target(os="linux")]
pub import("std::sys::linux::fs")::{
    FS_O_RDONLY,
    FS_O_WRONLY,
    FS_O_RDWR,
    FS_O_CREAT,
    FS_O_EXCL,
    FS_O_TRUNC,
    FS_O_APPEND,
    FS_O_NONBLOCK,
    FS_F_OK,
    FS_X_OK,
    FS_W_OK,
    FS_R_OK,
    FS_SEEK_SET,
    FS_SEEK_CUR,
    FS_SEEK_END,
    FS_F_GETFL,
    FS_F_SETFL,
    open,
    close,
    dup,
    dup2,
    pipe,
    fsync,
    fcntl,
    read,
    write,
    getcwd,
    chdir,
    access,
    lseek,
    unlink,
    mkdir,
    rmdir,
    rename_path,
    Stat,
    stat,
    fstat, ftruncate, same_file,
};
```

[Declaration](sys/fs.wave#L64)

```wave
#[target(os="macos")]
pub import("std::sys::macos::fs")::{
    FS_O_RDONLY,
    FS_O_WRONLY,
    FS_O_RDWR,
    FS_O_NONBLOCK,
    FS_O_APPEND,
    FS_O_CREAT,
    FS_O_TRUNC,
    FS_O_EXCL,
    FS_F_OK,
    FS_X_OK,
    FS_W_OK,
    FS_R_OK,
    FS_SEEK_SET,
    FS_SEEK_CUR,
    FS_SEEK_END,
    FS_F_GETFL,
    FS_F_SETFL,
    Stat,
    open,
    close,
    dup,
    pipe,
    dup2,
    fsync,
    fcntl,
    read,
    write,
    getcwd,
    chdir,
    access,
    lseek,
    unlink,
    mkdir,
    rmdir,
    rename_path,
    stat,
    fstat, ftruncate, same_file,
};
```

[Declaration](sys/fs.wave#L105)

```wave
#[target(os="windows")]
pub import("std::sys::windows::fs")::{
    FS_O_RDONLY, FS_O_WRONLY, FS_O_RDWR, FS_O_CREAT, FS_O_EXCL,
    FS_O_TRUNC, FS_O_APPEND, FS_O_NONBLOCK,
    FS_F_OK, FS_X_OK, FS_W_OK, FS_R_OK,
    FS_SEEK_SET, FS_SEEK_CUR, FS_SEEK_END, FS_F_GETFL, FS_F_SETFL,
    open, close, dup, dup2, pipe, fsync, fcntl, read, write, getcwd,
    chdir, access, lseek, unlink, mkdir, rmdir, rename_path, Stat, stat, fstat, ftruncate, same_file,
};
```

[Declaration](sys/fs.wave#L115)

```wave
#[target(os="freebsd")]
pub import("std::sys::freebsd::fs")::{
    FS_O_RDONLY, FS_O_WRONLY, FS_O_RDWR, FS_O_CREAT, FS_O_EXCL,
    FS_O_TRUNC, FS_O_APPEND, FS_O_NONBLOCK,
    FS_F_OK, FS_X_OK, FS_W_OK, FS_R_OK,
    FS_SEEK_SET, FS_SEEK_CUR, FS_SEEK_END, FS_F_GETFL, FS_F_SETFL,
    open, close, dup, dup2, pipe, fsync, fcntl, read, write, getcwd,
    chdir, access, lseek, unlink, mkdir, rmdir, rename_path, Stat, stat, fstat, ftruncate, same_file,
};
```

[Declaration](sys/fs.wave#L125)

```wave
#[target(os="wasi")]
pub import("std::sys::wasm::fs")::{
    FS_O_RDONLY, FS_O_WRONLY, FS_O_RDWR, FS_O_CREAT, FS_O_EXCL,
    FS_O_TRUNC, FS_O_APPEND, FS_O_NONBLOCK,
    FS_F_OK, FS_X_OK, FS_W_OK, FS_R_OK,
    FS_SEEK_SET, FS_SEEK_CUR, FS_SEEK_END, FS_F_GETFL, FS_F_SETFL,
    open, close, dup, dup2, pipe, fsync, fcntl, read, write, getcwd,
    chdir, access, lseek, unlink, mkdir, rmdir, rename_path, Stat, stat, fstat, ftruncate, same_file,
};
```

## `std::sys::interfaces`

[Source](sys/interfaces.wave)

[Declaration](sys/interfaces.wave#L2)

```wave
#[target(os="linux")]
pub import("std::sys::linux::interfaces")::{
    NativeIfAddrs, interfaces_open, interfaces_close, interface_next,
    interface_name, interface_flags, interface_address, interface_family, interface_index,
};
```

[Declaration](sys/interfaces.wave#L7)

```wave
#[target(os="macos")]
pub import("std::sys::macos::interfaces")::{
    NativeIfAddrs, interfaces_open, interfaces_close, interface_next,
    interface_name, interface_flags, interface_address, interface_family, interface_index,
};
```

[Declaration](sys/interfaces.wave#L12)

```wave
#[target(os="freebsd")]
pub import("std::sys::freebsd::interfaces")::{
    NativeIfAddrs, interfaces_open, interfaces_close, interface_next,
    interface_name, interface_flags, interface_address, interface_family, interface_index,
};
```

[Declaration](sys/interfaces.wave#L17)

```wave
#[target(os="windows")]
pub import("std::sys::windows::interfaces")::{
    NativeIfAddrs, interfaces_open, interfaces_close, interface_next,
    interface_name, interface_flags, interface_address, interface_family, interface_index,
};
```

## `std::sys::linux::amd64::env`

[Source](sys/linux/amd64/env.wave)

[Declaration](sys/linux/amd64/env.wave#L60)

```wave
pub fun env_read(buf: ptr<u8>, cap: i64) -> i64
```

## `std::sys::linux::amd64::fs`

[Source](sys/linux/amd64/fs.wave)

[Declaration](sys/linux/amd64/fs.wave#L41)

```wave
pub const FS_O_RDONLY: i32 = 0;
```

[Declaration](sys/linux/amd64/fs.wave#L42)

```wave
pub const FS_O_WRONLY: i32 = 1;
```

[Declaration](sys/linux/amd64/fs.wave#L43)

```wave
pub const FS_O_RDWR: i32 = 2;
```

[Declaration](sys/linux/amd64/fs.wave#L44)

```wave
pub const FS_O_CREAT: i32 = 64;
```

[Declaration](sys/linux/amd64/fs.wave#L45)

```wave
pub const FS_O_EXCL: i32 = 128;
```

[Declaration](sys/linux/amd64/fs.wave#L46)

```wave
pub const FS_O_TRUNC: i32 = 512;
```

[Declaration](sys/linux/amd64/fs.wave#L47)

```wave
pub const FS_O_APPEND: i32 = 1024;
```

[Declaration](sys/linux/amd64/fs.wave#L48)

```wave
pub const FS_O_NONBLOCK: i32 = 2048;
```

[Declaration](sys/linux/amd64/fs.wave#L51)

```wave
pub const FS_F_OK: i32 = 0;
```

[Declaration](sys/linux/amd64/fs.wave#L52)

```wave
pub const FS_X_OK: i32 = 1;
```

[Declaration](sys/linux/amd64/fs.wave#L53)

```wave
pub const FS_W_OK: i32 = 2;
```

[Declaration](sys/linux/amd64/fs.wave#L54)

```wave
pub const FS_R_OK: i32 = 4;
```

[Declaration](sys/linux/amd64/fs.wave#L57)

```wave
pub const FS_SEEK_SET: i32 = 0;
```

[Declaration](sys/linux/amd64/fs.wave#L58)

```wave
pub const FS_SEEK_CUR: i32 = 1;
```

[Declaration](sys/linux/amd64/fs.wave#L59)

```wave
pub const FS_SEEK_END: i32 = 2;
```

[Declaration](sys/linux/amd64/fs.wave#L62)

```wave
pub const FS_F_GETFL: i32 = 3;
```

[Declaration](sys/linux/amd64/fs.wave#L63)

```wave
pub const FS_F_SETFL: i32 = 4;
```

[Declaration](sys/linux/amd64/fs.wave#L69)

```wave
pub fun open(path: str, flags: i32, mode: i32) -> i64
```

[Declaration](sys/linux/amd64/fs.wave#L74)

```wave
pub fun close(fd: i64) -> i64
```

[Declaration](sys/linux/amd64/fs.wave#L79)

```wave
pub fun dup(fd: i64) -> i64
```

[Declaration](sys/linux/amd64/fs.wave#L84)

```wave
pub fun dup2(oldfd: i64, newfd: i64) -> i64
```

[Declaration](sys/linux/amd64/fs.wave#L89)

```wave
pub fun pipe(fds: ptr<i32>) -> i64
```

[Declaration](sys/linux/amd64/fs.wave#L94)

```wave
pub fun fsync(fd: i64) -> i64
```

[Declaration](sys/linux/amd64/fs.wave#L99)

```wave
pub fun fcntl(fd: i64, cmd: i32, arg: i64) -> i64
```

[Declaration](sys/linux/amd64/fs.wave#L109)

```wave
pub fun read(fd: i64, buf: ptr<u8>, len: i64) -> i64
```

[Declaration](sys/linux/amd64/fs.wave#L114)

```wave
pub fun write(fd: i64, buf: ptr<u8>, len: i64) -> i64
```

[Declaration](sys/linux/amd64/fs.wave#L119)

```wave
pub fun getcwd(buf: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/linux/amd64/fs.wave#L124)

```wave
pub fun chdir(path: str) -> i64
```

[Declaration](sys/linux/amd64/fs.wave#L129)

```wave
pub fun access(path: str, mode: i32) -> i64
```

[Declaration](sys/linux/amd64/fs.wave#L139)

```wave
pub fun lseek(fd: i64, offset: i64, whence: i32) -> i64
```

[Declaration](sys/linux/amd64/fs.wave#L149)

```wave
pub fun unlink(path: str) -> i64
```

[Declaration](sys/linux/amd64/fs.wave#L154)

```wave
pub fun mkdir(path: str, mode: i32) -> i64
```

[Declaration](sys/linux/amd64/fs.wave#L159)

```wave
pub fun rmdir(path: str) -> i64
```

[Declaration](sys/linux/amd64/fs.wave#L164)

```wave
pub fun rename_path(old_path: str, new_path: str) -> i64
```

[Declaration](sys/linux/amd64/fs.wave#L175)

```wave
pub struct Stat {
    dev: i64;
    ino: i64;
    nlink: i64;
    mode: i32;
    uid: i32;
    gid: i32;
    pad0: i32;
    rdev: i64;
    size: i64;
    blksize: i64;
    blocks: i64;
    atime: i64;
    atime_nsec: i64;
    mtime: i64;
    mtime_nsec: i64;
    ctime: i64;
    ctime_nsec: i64;
    unused0: i64;
    unused1: i64;
    unused2: i64;
}
```

[Declaration](sys/linux/amd64/fs.wave#L198)

```wave
pub fun stat(path: str, st: ptr<Stat>) -> i64
```

[Declaration](sys/linux/amd64/fs.wave#L203)

```wave
pub fun fstat(fd: i64, st: ptr<Stat>) -> i64
```

[Declaration](sys/linux/amd64/fs.wave#L209)

```wave
pub fun ftruncate(fd: i64, length: i64) -> i64
```

[Declaration](sys/linux/amd64/fs.wave#L217)

```wave
pub fun same_file(first_fd: i64, second_fd: i64) -> i64
```

## `std::sys::linux::amd64::memory`

[Source](sys/linux/amd64/memory.wave)

[Declaration](sys/linux/amd64/memory.wave#L40)

```wave
pub const PROT_READ: i32 = 1;
```

[Declaration](sys/linux/amd64/memory.wave#L41)

```wave
pub const PROT_WRITE: i32 = 2;
```

[Declaration](sys/linux/amd64/memory.wave#L42)

```wave
pub const MAP_PRIVATE: i32 = 2;
```

[Declaration](sys/linux/amd64/memory.wave#L43)

```wave
pub const MAP_ANONYMOUS: i32 = 32;
```

[Declaration](sys/linux/amd64/memory.wave#L50)

```wave
pub fun mmap(
    addr: ptr<i8>,
    length: i64,
    prot: i32,
    flags: i32,
    fd: i64,
    offset: i64
) -> ptr<i8>
```

[Declaration](sys/linux/amd64/memory.wave#L70)

```wave
pub fun munmap(addr: ptr<i8>, length: i64) -> i64
```

[Declaration](sys/linux/amd64/memory.wave#L80)

```wave
pub fun brk(addr: ptr<i8>) -> ptr<i8>
```

[Declaration](sys/linux/amd64/memory.wave#L85)

```wave
pub fun sys_alloc(size: i64) -> ptr<u8>
```

[Declaration](sys/linux/amd64/memory.wave#L106)

```wave
pub fun sys_free(p: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/linux/amd64/memory.wave#L114)

```wave
pub fun sys_page_size() -> i64
```

## `std::sys::linux::amd64::process`

[Source](sys/linux/amd64/process.wave)

[Declaration](sys/linux/amd64/process.wave#L43)

```wave
pub fun exit(code: i32) -> !
```

[Declaration](sys/linux/amd64/process.wave#L49)

```wave
pub fun getpid() -> i64
```

[Declaration](sys/linux/amd64/process.wave#L54)

```wave
pub fun getppid() -> i64
```

[Declaration](sys/linux/amd64/process.wave#L64)

```wave
pub fun fork() -> i64
```

[Declaration](sys/linux/amd64/process.wave#L69)

```wave
pub fun execve(
    path: str,
    argv: ptr<ptr<i8>>,
    envp: ptr<ptr<i8>>
) -> i64
```

[Declaration](sys/linux/amd64/process.wave#L83)

```wave
pub fun waitpid(pid: i64, status: ptr<i32>, options: i32) -> i64
```

[Declaration](sys/linux/amd64/process.wave#L97)

```wave
pub fun kill(pid: i64, sig: i32) -> i64
```

## `std::sys::linux::amd64::socket`

[Source](sys/linux/amd64/socket.wave)

[Declaration](sys/linux/amd64/socket.wave#L45)

```wave
pub const AF_INET: i32 = 2;
```

[Declaration](sys/linux/amd64/socket.wave#L46)

```wave
pub const AF_INET6: i32 = 10;
```

[Declaration](sys/linux/amd64/socket.wave#L49)

```wave
pub const SOCK_STREAM: i32 = 1;
```

[Declaration](sys/linux/amd64/socket.wave#L50)

```wave
pub const SOCK_DGRAM: i32 = 2;
```

[Declaration](sys/linux/amd64/socket.wave#L53)

```wave
pub const IPPROTO_IP: i32 = 0;
```

[Declaration](sys/linux/amd64/socket.wave#L54)

```wave
pub const IPPROTO_IPV6: i32 = 41;
```

[Declaration](sys/linux/amd64/socket.wave#L55)

```wave
pub const IPPROTO_TCP: i32 = 6;
```

[Declaration](sys/linux/amd64/socket.wave#L56)

```wave
pub const IPPROTO_UDP: i32 = 17;
```

[Declaration](sys/linux/amd64/socket.wave#L59)

```wave
pub const SHUT_RD: i32 = 0;
```

[Declaration](sys/linux/amd64/socket.wave#L60)

```wave
pub const SHUT_WR: i32 = 1;
```

[Declaration](sys/linux/amd64/socket.wave#L61)

```wave
pub const SHUT_RDWR: i32 = 2;
```

[Declaration](sys/linux/amd64/socket.wave#L64)

```wave
pub const SOL_SOCKET: i32 = 1;
```

[Declaration](sys/linux/amd64/socket.wave#L65)

```wave
pub const SO_REUSEADDR: i32 = 2;
```

[Declaration](sys/linux/amd64/socket.wave#L66)

```wave
pub const SO_REUSEPORT: i32 = 15;
```

[Declaration](sys/linux/amd64/socket.wave#L67)

```wave
pub const SO_ERROR: i32 = 4;
```

[Declaration](sys/linux/amd64/socket.wave#L68)

```wave
pub const SO_RCVBUF: i32 = 8;
```

[Declaration](sys/linux/amd64/socket.wave#L69)

```wave
pub const SO_SNDBUF: i32 = 7;
```

[Declaration](sys/linux/amd64/socket.wave#L70)

```wave
pub const SO_BROADCAST: i32 = 6;
```

[Declaration](sys/linux/amd64/socket.wave#L71)

```wave
pub const SO_KEEPALIVE: i32 = 9;
```

[Declaration](sys/linux/amd64/socket.wave#L72)

```wave
pub const TCP_NODELAY: i32 = 1;
```

[Declaration](sys/linux/amd64/socket.wave#L73)

```wave
pub const IP_TTL: i32 = 2;
```

[Declaration](sys/linux/amd64/socket.wave#L74)

```wave
pub const IP_MULTICAST_IF: i32 = 32;
```

[Declaration](sys/linux/amd64/socket.wave#L75)

```wave
pub const IP_MULTICAST_TTL: i32 = 33;
```

[Declaration](sys/linux/amd64/socket.wave#L76)

```wave
pub const IP_MULTICAST_LOOP: i32 = 34;
```

[Declaration](sys/linux/amd64/socket.wave#L77)

```wave
pub const IP_ADD_MEMBERSHIP: i32 = 35;
```

[Declaration](sys/linux/amd64/socket.wave#L78)

```wave
pub const IP_DROP_MEMBERSHIP: i32 = 36;
```

[Declaration](sys/linux/amd64/socket.wave#L79)

```wave
pub const IPV6_MULTICAST_IF: i32 = 17;
```

[Declaration](sys/linux/amd64/socket.wave#L80)

```wave
pub const IPV6_MULTICAST_HOPS: i32 = 18;
```

[Declaration](sys/linux/amd64/socket.wave#L81)

```wave
pub const IPV6_MULTICAST_LOOP: i32 = 19;
```

[Declaration](sys/linux/amd64/socket.wave#L82)

```wave
pub const IPV6_JOIN_GROUP: i32 = 20;
```

[Declaration](sys/linux/amd64/socket.wave#L83)

```wave
pub const IPV6_LEAVE_GROUP: i32 = 21;
```

[Declaration](sys/linux/amd64/socket.wave#L84)

```wave
pub const MSG_NOSIGNAL: i32 = 0x4000;
```

[Declaration](sys/linux/amd64/socket.wave#L85)

```wave
pub const MSG_DONTWAIT: i32 = 64;
```

[Declaration](sys/linux/amd64/socket.wave#L88)

```wave
pub const POLLIN: i16 = 0x001;
```

[Declaration](sys/linux/amd64/socket.wave#L89)

```wave
pub const POLLOUT: i16 = 0x004;
```

[Declaration](sys/linux/amd64/socket.wave#L90)

```wave
pub const POLLERR: i16 = 0x008;
```

[Declaration](sys/linux/amd64/socket.wave#L91)

```wave
pub const POLLHUP: i16 = 0x010;
```

[Declaration](sys/linux/amd64/socket.wave#L92)

```wave
pub const POLLNVAL: i16 = 0x020;
```

[Declaration](sys/linux/amd64/socket.wave#L94)

```wave
pub struct PollFd {
    fd: i32;
    events: i16;
    revents: i16;
}
```

[Declaration](sys/linux/amd64/socket.wave#L105)

```wave
pub fun socket(domain: i32, ty: i32, protocol: i32) -> i64
```

[Declaration](sys/linux/amd64/socket.wave#L110)

```wave
pub fun bind(fd: i64, addr: ptr<i8>, len: i32) -> i64
```

[Declaration](sys/linux/amd64/socket.wave#L115)

```wave
pub fun listen(fd: i64, backlog: i32) -> i64
```

[Declaration](sys/linux/amd64/socket.wave#L120)

```wave
pub fun accept(fd: i64, addr: ptr<i8>, len: ptr<i32>) -> i64
```

[Declaration](sys/linux/amd64/socket.wave#L125)

```wave
pub fun connect(fd: i64, addr: ptr<i8>, len: i32) -> i64
```

[Declaration](sys/linux/amd64/socket.wave#L130)

```wave
pub fun getsockname(fd: i64, addr: ptr<i8>, len: ptr<i32>) -> i64
```

[Declaration](sys/linux/amd64/socket.wave#L134)

```wave
pub fun getpeername(fd: i64, addr: ptr<i8>, len: ptr<i32>) -> i64
```

[Declaration](sys/linux/amd64/socket.wave#L138)

```wave
pub fun shutdown(fd: i64, how: i32) -> i64
```

[Declaration](sys/linux/amd64/socket.wave#L143)

```wave
pub fun close_socket(fd: i64) -> i64
```

[Declaration](sys/linux/amd64/socket.wave#L145)

```wave
pub fun setsockopt(
    fd: i64,
    level: i32,
    optname: i32,
    optval: ptr<i8>,
    optlen: i32
) -> i64
```

[Declaration](sys/linux/amd64/socket.wave#L163)

```wave
pub fun getsockopt(
    fd: i64,
    level: i32,
    optname: i32,
    optval: ptr<i8>,
    optlen: ptr<i32>
) -> i64
```

[Declaration](sys/linux/amd64/socket.wave#L181)

```wave
pub fun poll(fds: ptr<PollFd>, nfds: i64, timeout_ms: i32) -> i64
```

[Declaration](sys/linux/amd64/socket.wave#L191)

```wave
pub fun send(fd: i64, buf: ptr<u8>, len: i64, flags: i32) -> i64
```

[Declaration](sys/linux/amd64/socket.wave#L195)

```wave
pub fun recv(fd: i64, buf: ptr<u8>, len: i64, flags: i32) -> i64
```

[Declaration](sys/linux/amd64/socket.wave#L199)

```wave
pub fun sendto(
    fd: i64,
    buf: ptr<u8>,
    len: i64,
    flags: i32,
    addr: ptr<i8>,
    addrlen: i32
) -> i64
```

[Declaration](sys/linux/amd64/socket.wave#L219)

```wave
pub fun recvfrom(
    fd: i64,
    buf: ptr<u8>,
    len: i64,
    flags: i32,
    addr: ptr<i8>,
    addrlen: ptr<i32>
) -> i64
```

## `std::sys::linux::amd64::syscall`

[Source](sys/linux/amd64/syscall.wave)

[Declaration](sys/linux/amd64/syscall.wave#L35)

```wave
pub fun syscall0(id: i64) -> i64
```

[Declaration](sys/linux/amd64/syscall.wave#L49)

```wave
pub fun syscall1(id: i64, a1: i64) -> i64
```

[Declaration](sys/linux/amd64/syscall.wave#L64)

```wave
pub fun syscall2(id: i64, a1: i64, a2: i64) -> i64
```

[Declaration](sys/linux/amd64/syscall.wave#L80)

```wave
pub fun syscall3(id: i64, a1: i64, a2: i64, a3: i64) -> i64
```

[Declaration](sys/linux/amd64/syscall.wave#L97)

```wave
pub fun syscall4(id: i64, a1: i64, a2: i64, a3: i64, a4: i64) -> i64
```

[Declaration](sys/linux/amd64/syscall.wave#L115)

```wave
pub fun syscall5(id: i64, a1: i64, a2: i64, a3: i64, a4: i64, a5: i64) -> i64
```

[Declaration](sys/linux/amd64/syscall.wave#L134)

```wave
pub fun syscall6(
    id: i64,
    a1: i64,
    a2: i64,
    a3: i64,
    a4: i64,
    a5: i64,
    a6: i64
) -> i64
```

## `std::sys::linux::amd64::time`

[Source](sys/linux/amd64/time.wave)

[Declaration](sys/linux/amd64/time.wave#L43)

```wave
pub struct TimeSpec {
    sec: i64;
    nsec: i64;
}
```

[Declaration](sys/linux/amd64/time.wave#L53)

```wave
pub fun nanosleep(req: ptr<TimeSpec>, rem: ptr<TimeSpec>) -> i64
```

[Declaration](sys/linux/amd64/time.wave#L63)

```wave
pub fun clock_gettime(clock_id: i32, tp: ptr<TimeSpec>) -> i64
```

## `std::sys::linux::amd64::tty`

[Source](sys/linux/amd64/tty.wave)

[Declaration](sys/linux/amd64/tty.wave#L37)

```wave
pub const TTY_SYS_IOCTL: i64 = 16;
```

[Declaration](sys/linux/amd64/tty.wave#L38)

```wave
pub const TTY_SYS_FCNTL: i64 = 72;
```

[Declaration](sys/linux/amd64/tty.wave#L40)

```wave
pub const TTY_TCGETS: i64 = 0x5401;
```

[Declaration](sys/linux/amd64/tty.wave#L41)

```wave
pub const TTY_TCSETS: i64 = 0x5402;
```

[Declaration](sys/linux/amd64/tty.wave#L42)

```wave
pub const TTY_TCSETSW: i64 = 0x5403;
```

[Declaration](sys/linux/amd64/tty.wave#L43)

```wave
pub const TTY_TCSETSF: i64 = 0x5404;
```

[Declaration](sys/linux/amd64/tty.wave#L45)

```wave
pub const TTY_TCSANOW: i32 = 0;
```

[Declaration](sys/linux/amd64/tty.wave#L46)

```wave
pub const TTY_TCSADRAIN: i32 = 1;
```

[Declaration](sys/linux/amd64/tty.wave#L47)

```wave
pub const TTY_TCSAFLUSH: i32 = 2;
```

[Declaration](sys/linux/amd64/tty.wave#L49)

```wave
pub const TTY_F_GETFL: i32 = 3;
```

[Declaration](sys/linux/amd64/tty.wave#L50)

```wave
pub const TTY_F_SETFL: i32 = 4;
```

[Declaration](sys/linux/amd64/tty.wave#L51)

```wave
pub const TTY_O_NONBLOCK: i32 = 2048;
```

[Declaration](sys/linux/amd64/tty.wave#L53)

```wave
pub const TTY_ICANON: u32 = 2;
```

[Declaration](sys/linux/amd64/tty.wave#L54)

```wave
pub const TTY_ECHO: u32 = 8;
```

[Declaration](sys/linux/amd64/tty.wave#L55)

```wave
pub const TTY_VTIME_IDX: i32 = 5;
```

[Declaration](sys/linux/amd64/tty.wave#L56)

```wave
pub const TTY_VMIN_IDX: i32 = 6;
```

[Declaration](sys/linux/amd64/tty.wave#L58)

```wave
pub struct Termios {
    c_iflag: u32;
    c_oflag: u32;
    c_cflag: u32;
    c_lflag: u32;
    c_line: u8;
    c_cc: array<u8, 19>;
}
```

[Declaration](sys/linux/amd64/tty.wave#L67)

```wave
pub struct TtyRawState {
    term: Termios;
    flags: i32;
    active: i32;
}
```

[Declaration](sys/linux/amd64/tty.wave#L73)

```wave
pub fun tty_getattr(fd: i32, t: ptr<Termios>) -> i64
```

[Declaration](sys/linux/amd64/tty.wave#L77)

```wave
pub fun tty_setattr(fd: i32, action: i32, t: ptr<Termios>) -> i64
```

[Declaration](sys/linux/amd64/tty.wave#L90)

```wave
pub fun tty_getfl(fd: i32) -> i64
```

[Declaration](sys/linux/amd64/tty.wave#L94)

```wave
pub fun tty_setfl(fd: i32, flags: i32) -> i64
```

[Declaration](sys/linux/amd64/tty.wave#L98)

```wave
pub fun tty_enable_raw_nonblock(fd: i32, st: ptr<TtyRawState>) -> i64
```

[Declaration](sys/linux/amd64/tty.wave#L133)

```wave
pub fun tty_restore(fd: i32, st: ptr<TtyRawState>)
```

## `std::sys::linux::arm64::env`

[Source](sys/linux/arm64/env.wave)

[Declaration](sys/linux/arm64/env.wave#L60)

```wave
pub fun env_read(buf: ptr<u8>, cap: i64) -> i64
```

## `std::sys::linux::arm64::fs`

[Source](sys/linux/arm64/fs.wave)

[Declaration](sys/linux/arm64/fs.wave#L32)

```wave
pub const FS_O_RDONLY: i32 = 0;
```

[Declaration](sys/linux/arm64/fs.wave#L33)

```wave
pub const FS_O_WRONLY: i32 = 1;
```

[Declaration](sys/linux/arm64/fs.wave#L34)

```wave
pub const FS_O_RDWR: i32 = 2;
```

[Declaration](sys/linux/arm64/fs.wave#L35)

```wave
pub const FS_O_CREAT: i32 = 64;
```

[Declaration](sys/linux/arm64/fs.wave#L36)

```wave
pub const FS_O_EXCL: i32 = 128;
```

[Declaration](sys/linux/arm64/fs.wave#L37)

```wave
pub const FS_O_TRUNC: i32 = 512;
```

[Declaration](sys/linux/arm64/fs.wave#L38)

```wave
pub const FS_O_APPEND: i32 = 1024;
```

[Declaration](sys/linux/arm64/fs.wave#L39)

```wave
pub const FS_O_NONBLOCK: i32 = 2048;
```

[Declaration](sys/linux/arm64/fs.wave#L42)

```wave
pub const FS_F_OK: i32 = 0;
```

[Declaration](sys/linux/arm64/fs.wave#L43)

```wave
pub const FS_X_OK: i32 = 1;
```

[Declaration](sys/linux/arm64/fs.wave#L44)

```wave
pub const FS_W_OK: i32 = 2;
```

[Declaration](sys/linux/arm64/fs.wave#L45)

```wave
pub const FS_R_OK: i32 = 4;
```

[Declaration](sys/linux/arm64/fs.wave#L48)

```wave
pub const FS_SEEK_SET: i32 = 0;
```

[Declaration](sys/linux/arm64/fs.wave#L49)

```wave
pub const FS_SEEK_CUR: i32 = 1;
```

[Declaration](sys/linux/arm64/fs.wave#L50)

```wave
pub const FS_SEEK_END: i32 = 2;
```

[Declaration](sys/linux/arm64/fs.wave#L53)

```wave
pub const FS_F_GETFL: i32 = 3;
```

[Declaration](sys/linux/arm64/fs.wave#L54)

```wave
pub const FS_F_SETFL: i32 = 4;
```

[Declaration](sys/linux/arm64/fs.wave#L63)

```wave
pub fun open(path: str, flags: i32, mode: i32) -> i64
```

[Declaration](sys/linux/arm64/fs.wave#L67)

```wave
pub fun close(fd: i64) -> i64
```

[Declaration](sys/linux/arm64/fs.wave#L71)

```wave
pub fun dup(fd: i64) -> i64
```

[Declaration](sys/linux/arm64/fs.wave#L75)

```wave
pub fun dup2(oldfd: i64, newfd: i64) -> i64
```

[Declaration](sys/linux/arm64/fs.wave#L85)

```wave
pub fun pipe(fds: ptr<i32>) -> i64
```

[Declaration](sys/linux/arm64/fs.wave#L89)

```wave
pub fun fsync(fd: i64) -> i64
```

[Declaration](sys/linux/arm64/fs.wave#L93)

```wave
pub fun fcntl(fd: i64, cmd: i32, arg: i64) -> i64
```

[Declaration](sys/linux/arm64/fs.wave#L102)

```wave
pub fun read(fd: i64, buf: ptr<u8>, len: i64) -> i64
```

[Declaration](sys/linux/arm64/fs.wave#L106)

```wave
pub fun write(fd: i64, buf: ptr<u8>, len: i64) -> i64
```

[Declaration](sys/linux/arm64/fs.wave#L110)

```wave
pub fun getcwd(buf: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/linux/arm64/fs.wave#L114)

```wave
pub fun chdir(path: str) -> i64
```

[Declaration](sys/linux/arm64/fs.wave#L118)

```wave
pub fun access(path: str, mode: i32) -> i64
```

[Declaration](sys/linux/arm64/fs.wave#L127)

```wave
pub fun lseek(fd: i64, offset: i64, whence: i32) -> i64
```

[Declaration](sys/linux/arm64/fs.wave#L136)

```wave
pub fun unlink(path: str) -> i64
```

[Declaration](sys/linux/arm64/fs.wave#L140)

```wave
pub fun mkdir(path: str, mode: i32) -> i64
```

[Declaration](sys/linux/arm64/fs.wave#L144)

```wave
pub fun rmdir(path: str) -> i64
```

[Declaration](sys/linux/arm64/fs.wave#L148)

```wave
pub fun rename_path(old_path: str, new_path: str) -> i64
```

[Declaration](sys/linux/arm64/fs.wave#L164)

```wave
pub struct Stat {
    dev: i64;
    ino: i64;
    mode: i32;
    nlink: i32;
    uid: i32;
    gid: i32;
    rdev: i64;
    pad1: i64;
    size: i64;
    blksize: i32;
    pad2: i32;
    blocks: i64;
    atime: i64;
    atime_nsec: i64;
    mtime: i64;
    mtime_nsec: i64;
    ctime: i64;
    ctime_nsec: i64;
    unused0: i32;
    unused1: i32;
}
```

[Declaration](sys/linux/arm64/fs.wave#L187)

```wave
pub fun stat(path: str, st: ptr<Stat>) -> i64
```

[Declaration](sys/linux/arm64/fs.wave#L191)

```wave
pub fun fstat(fd: i64, st: ptr<Stat>) -> i64
```

[Declaration](sys/linux/arm64/fs.wave#L196)

```wave
pub fun ftruncate(fd: i64, length: i64) -> i64
```

[Declaration](sys/linux/arm64/fs.wave#L204)

```wave
pub fun same_file(first_fd: i64, second_fd: i64) -> i64
```

## `std::sys::linux::arm64::memory`

[Source](sys/linux/arm64/memory.wave)

[Declaration](sys/linux/arm64/memory.wave#L33)

```wave
pub const PROT_READ: i32 = 1;
```

[Declaration](sys/linux/arm64/memory.wave#L34)

```wave
pub const PROT_WRITE: i32 = 2;
```

[Declaration](sys/linux/arm64/memory.wave#L35)

```wave
pub const MAP_PRIVATE: i32 = 2;
```

[Declaration](sys/linux/arm64/memory.wave#L36)

```wave
pub const MAP_ANONYMOUS: i32 = 32;
```

[Declaration](sys/linux/arm64/memory.wave#L43)

```wave
pub fun mmap(
    addr: ptr<i8>,
    length: i64,
    prot: i32,
    flags: i32,
    fd: i64,
    offset: i64
) -> ptr<i8>
```

[Declaration](sys/linux/arm64/memory.wave#L62)

```wave
pub fun munmap(addr: ptr<i8>, length: i64) -> i64
```

[Declaration](sys/linux/arm64/memory.wave#L71)

```wave
pub fun brk(addr: ptr<i8>) -> ptr<i8>
```

[Declaration](sys/linux/arm64/memory.wave#L75)

```wave
pub fun sys_alloc(size: i64) -> ptr<u8>
```

[Declaration](sys/linux/arm64/memory.wave#L96)

```wave
pub fun sys_free(p: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/linux/arm64/memory.wave#L104)

```wave
pub fun sys_page_size() -> i64
```

## `std::sys::linux::arm64::process`

[Source](sys/linux/arm64/process.wave)

[Declaration](sys/linux/arm64/process.wave#L36)

```wave
pub fun exit(code: i32) -> !
```

[Declaration](sys/linux/arm64/process.wave#L41)

```wave
pub fun getpid() -> i64
```

[Declaration](sys/linux/arm64/process.wave#L45)

```wave
pub fun getppid() -> i64
```

[Declaration](sys/linux/arm64/process.wave#L54)

```wave
pub fun fork() -> i64
```

[Declaration](sys/linux/arm64/process.wave#L59)

```wave
pub fun execve(
    path: str,
    argv: ptr<ptr<i8>>,
    envp: ptr<ptr<i8>>
) -> i64
```

[Declaration](sys/linux/arm64/process.wave#L72)

```wave
pub fun waitpid(pid: i64, status: ptr<i32>, options: i32) -> i64
```

[Declaration](sys/linux/arm64/process.wave#L84)

```wave
pub fun kill(pid: i64, sig: i32) -> i64
```

## `std::sys::linux::arm64::socket`

[Source](sys/linux/arm64/socket.wave)

[Declaration](sys/linux/arm64/socket.wave#L38)

```wave
pub const AF_INET: i32 = 2;
```

[Declaration](sys/linux/arm64/socket.wave#L39)

```wave
pub const AF_INET6: i32 = 10;
```

[Declaration](sys/linux/arm64/socket.wave#L42)

```wave
pub const SOCK_STREAM: i32 = 1;
```

[Declaration](sys/linux/arm64/socket.wave#L43)

```wave
pub const SOCK_DGRAM: i32 = 2;
```

[Declaration](sys/linux/arm64/socket.wave#L46)

```wave
pub const IPPROTO_IP: i32 = 0;
```

[Declaration](sys/linux/arm64/socket.wave#L47)

```wave
pub const IPPROTO_IPV6: i32 = 41;
```

[Declaration](sys/linux/arm64/socket.wave#L48)

```wave
pub const IPPROTO_TCP: i32 = 6;
```

[Declaration](sys/linux/arm64/socket.wave#L49)

```wave
pub const IPPROTO_UDP: i32 = 17;
```

[Declaration](sys/linux/arm64/socket.wave#L52)

```wave
pub const SHUT_RD: i32 = 0;
```

[Declaration](sys/linux/arm64/socket.wave#L53)

```wave
pub const SHUT_WR: i32 = 1;
```

[Declaration](sys/linux/arm64/socket.wave#L54)

```wave
pub const SHUT_RDWR: i32 = 2;
```

[Declaration](sys/linux/arm64/socket.wave#L57)

```wave
pub const SOL_SOCKET: i32 = 1;
```

[Declaration](sys/linux/arm64/socket.wave#L58)

```wave
pub const SO_REUSEADDR: i32 = 2;
```

[Declaration](sys/linux/arm64/socket.wave#L59)

```wave
pub const SO_REUSEPORT: i32 = 15;
```

[Declaration](sys/linux/arm64/socket.wave#L60)

```wave
pub const SO_ERROR: i32 = 4;
```

[Declaration](sys/linux/arm64/socket.wave#L61)

```wave
pub const SO_RCVBUF: i32 = 8;
```

[Declaration](sys/linux/arm64/socket.wave#L62)

```wave
pub const SO_SNDBUF: i32 = 7;
```

[Declaration](sys/linux/arm64/socket.wave#L63)

```wave
pub const SO_BROADCAST: i32 = 6;
```

[Declaration](sys/linux/arm64/socket.wave#L64)

```wave
pub const SO_KEEPALIVE: i32 = 9;
```

[Declaration](sys/linux/arm64/socket.wave#L65)

```wave
pub const TCP_NODELAY: i32 = 1;
```

[Declaration](sys/linux/arm64/socket.wave#L66)

```wave
pub const IP_TTL: i32 = 2;
```

[Declaration](sys/linux/arm64/socket.wave#L67)

```wave
pub const IP_MULTICAST_IF: i32 = 32;
```

[Declaration](sys/linux/arm64/socket.wave#L68)

```wave
pub const IP_MULTICAST_TTL: i32 = 33;
```

[Declaration](sys/linux/arm64/socket.wave#L69)

```wave
pub const IP_MULTICAST_LOOP: i32 = 34;
```

[Declaration](sys/linux/arm64/socket.wave#L70)

```wave
pub const IP_ADD_MEMBERSHIP: i32 = 35;
```

[Declaration](sys/linux/arm64/socket.wave#L71)

```wave
pub const IP_DROP_MEMBERSHIP: i32 = 36;
```

[Declaration](sys/linux/arm64/socket.wave#L72)

```wave
pub const IPV6_MULTICAST_IF: i32 = 17;
```

[Declaration](sys/linux/arm64/socket.wave#L73)

```wave
pub const IPV6_MULTICAST_HOPS: i32 = 18;
```

[Declaration](sys/linux/arm64/socket.wave#L74)

```wave
pub const IPV6_MULTICAST_LOOP: i32 = 19;
```

[Declaration](sys/linux/arm64/socket.wave#L75)

```wave
pub const IPV6_JOIN_GROUP: i32 = 20;
```

[Declaration](sys/linux/arm64/socket.wave#L76)

```wave
pub const IPV6_LEAVE_GROUP: i32 = 21;
```

[Declaration](sys/linux/arm64/socket.wave#L77)

```wave
pub const MSG_NOSIGNAL: i32 = 0x4000;
```

[Declaration](sys/linux/arm64/socket.wave#L78)

```wave
pub const MSG_DONTWAIT: i32 = 64;
```

[Declaration](sys/linux/arm64/socket.wave#L81)

```wave
pub const POLLIN: i16 = 0x001;
```

[Declaration](sys/linux/arm64/socket.wave#L82)

```wave
pub const POLLOUT: i16 = 0x004;
```

[Declaration](sys/linux/arm64/socket.wave#L83)

```wave
pub const POLLERR: i16 = 0x008;
```

[Declaration](sys/linux/arm64/socket.wave#L84)

```wave
pub const POLLHUP: i16 = 0x010;
```

[Declaration](sys/linux/arm64/socket.wave#L85)

```wave
pub const POLLNVAL: i16 = 0x020;
```

[Declaration](sys/linux/arm64/socket.wave#L87)

```wave
pub struct PollFd {
    fd: i32;
    events: i16;
    revents: i16;
}
```

[Declaration](sys/linux/arm64/socket.wave#L103)

```wave
pub fun socket(domain: i32, ty: i32, protocol: i32) -> i64
```

[Declaration](sys/linux/arm64/socket.wave#L107)

```wave
pub fun bind(fd: i64, addr: ptr<i8>, len: i32) -> i64
```

[Declaration](sys/linux/arm64/socket.wave#L111)

```wave
pub fun listen(fd: i64, backlog: i32) -> i64
```

[Declaration](sys/linux/arm64/socket.wave#L115)

```wave
pub fun accept(fd: i64, addr: ptr<i8>, len: ptr<i32>) -> i64
```

[Declaration](sys/linux/arm64/socket.wave#L119)

```wave
pub fun connect(fd: i64, addr: ptr<i8>, len: i32) -> i64
```

[Declaration](sys/linux/arm64/socket.wave#L123)

```wave
pub fun getsockname(fd: i64, addr: ptr<i8>, len: ptr<i32>) -> i64
```

[Declaration](sys/linux/arm64/socket.wave#L127)

```wave
pub fun getpeername(fd: i64, addr: ptr<i8>, len: ptr<i32>) -> i64
```

[Declaration](sys/linux/arm64/socket.wave#L131)

```wave
pub fun shutdown(fd: i64, how: i32) -> i64
```

[Declaration](sys/linux/arm64/socket.wave#L135)

```wave
pub fun close_socket(fd: i64) -> i64
```

[Declaration](sys/linux/arm64/socket.wave#L137)

```wave
pub fun setsockopt(
    fd: i64,
    level: i32,
    optname: i32,
    optval: ptr<i8>,
    optlen: i32
) -> i64
```

[Declaration](sys/linux/arm64/socket.wave#L154)

```wave
pub fun getsockopt(
    fd: i64,
    level: i32,
    optname: i32,
    optval: ptr<i8>,
    optlen: ptr<i32>
) -> i64
```

[Declaration](sys/linux/arm64/socket.wave#L171)

```wave
pub fun poll(fds: ptr<PollFd>, nfds: i64, timeout_ms: i32) -> i64
```

[Declaration](sys/linux/arm64/socket.wave#L187)

```wave
pub fun send(fd: i64, buf: ptr<u8>, len: i64, flags: i32) -> i64
```

[Declaration](sys/linux/arm64/socket.wave#L191)

```wave
pub fun recv(fd: i64, buf: ptr<u8>, len: i64, flags: i32) -> i64
```

[Declaration](sys/linux/arm64/socket.wave#L195)

```wave
pub fun sendto(
    fd: i64,
    buf: ptr<u8>,
    len: i64,
    flags: i32,
    addr: ptr<i8>,
    addrlen: i32
) -> i64
```

[Declaration](sys/linux/arm64/socket.wave#L214)

```wave
pub fun recvfrom(
    fd: i64,
    buf: ptr<u8>,
    len: i64,
    flags: i32,
    addr: ptr<i8>,
    addrlen: ptr<i32>
) -> i64
```

## `std::sys::linux::arm64::syscall`

[Source](sys/linux/arm64/syscall.wave)

[Declaration](sys/linux/arm64/syscall.wave#L22)

```wave
pub fun syscall0(id: i64) -> i64
```

[Declaration](sys/linux/arm64/syscall.wave#L32)

```wave
pub fun syscall1(id: i64, a1: i64) -> i64
```

[Declaration](sys/linux/arm64/syscall.wave#L43)

```wave
pub fun syscall2(id: i64, a1: i64, a2: i64) -> i64
```

[Declaration](sys/linux/arm64/syscall.wave#L55)

```wave
pub fun syscall3(id: i64, a1: i64, a2: i64, a3: i64) -> i64
```

[Declaration](sys/linux/arm64/syscall.wave#L68)

```wave
pub fun syscall4(id: i64, a1: i64, a2: i64, a3: i64, a4: i64) -> i64
```

[Declaration](sys/linux/arm64/syscall.wave#L82)

```wave
pub fun syscall5(id: i64, a1: i64, a2: i64, a3: i64, a4: i64, a5: i64) -> i64
```

[Declaration](sys/linux/arm64/syscall.wave#L97)

```wave
pub fun syscall6(
    id: i64,
    a1: i64,
    a2: i64,
    a3: i64,
    a4: i64,
    a5: i64,
    a6: i64
) -> i64
```

## `std::sys::linux::arm64::time`

[Source](sys/linux/arm64/time.wave)

[Declaration](sys/linux/arm64/time.wave#L36)

```wave
pub struct TimeSpec {
    sec: i64;
    nsec: i64;
}
```

[Declaration](sys/linux/arm64/time.wave#L46)

```wave
pub fun nanosleep(req: ptr<TimeSpec>, rem: ptr<TimeSpec>) -> i64
```

[Declaration](sys/linux/arm64/time.wave#L55)

```wave
pub fun clock_gettime(clock_id: i32, tp: ptr<TimeSpec>) -> i64
```

## `std::sys::linux::arm64::tty`

[Source](sys/linux/arm64/tty.wave)

[Declaration](sys/linux/arm64/tty.wave#L31)

```wave
pub const TTY_SYS_IOCTL: i64 = 29;
```

[Declaration](sys/linux/arm64/tty.wave#L32)

```wave
pub const TTY_SYS_FCNTL: i64 = 25;
```

[Declaration](sys/linux/arm64/tty.wave#L34)

```wave
pub const TTY_TCGETS: i64 = 0x5401;
```

[Declaration](sys/linux/arm64/tty.wave#L35)

```wave
pub const TTY_TCSETS: i64 = 0x5402;
```

[Declaration](sys/linux/arm64/tty.wave#L36)

```wave
pub const TTY_TCSETSW: i64 = 0x5403;
```

[Declaration](sys/linux/arm64/tty.wave#L37)

```wave
pub const TTY_TCSETSF: i64 = 0x5404;
```

[Declaration](sys/linux/arm64/tty.wave#L39)

```wave
pub const TTY_TCSANOW: i32 = 0;
```

[Declaration](sys/linux/arm64/tty.wave#L40)

```wave
pub const TTY_TCSADRAIN: i32 = 1;
```

[Declaration](sys/linux/arm64/tty.wave#L41)

```wave
pub const TTY_TCSAFLUSH: i32 = 2;
```

[Declaration](sys/linux/arm64/tty.wave#L43)

```wave
pub const TTY_F_GETFL: i32 = 3;
```

[Declaration](sys/linux/arm64/tty.wave#L44)

```wave
pub const TTY_F_SETFL: i32 = 4;
```

[Declaration](sys/linux/arm64/tty.wave#L45)

```wave
pub const TTY_O_NONBLOCK: i32 = 2048;
```

[Declaration](sys/linux/arm64/tty.wave#L47)

```wave
pub const TTY_ICANON: u32 = 2;
```

[Declaration](sys/linux/arm64/tty.wave#L48)

```wave
pub const TTY_ECHO: u32 = 8;
```

[Declaration](sys/linux/arm64/tty.wave#L49)

```wave
pub const TTY_VTIME_IDX: i32 = 5;
```

[Declaration](sys/linux/arm64/tty.wave#L50)

```wave
pub const TTY_VMIN_IDX: i32 = 6;
```

[Declaration](sys/linux/arm64/tty.wave#L52)

```wave
pub struct Termios {
    c_iflag: u32;
    c_oflag: u32;
    c_cflag: u32;
    c_lflag: u32;
    c_line: u8;
    c_cc: array<u8, 19>;
}
```

[Declaration](sys/linux/arm64/tty.wave#L61)

```wave
pub struct TtyRawState {
    term: Termios;
    flags: i32;
    active: i32;
}
```

[Declaration](sys/linux/arm64/tty.wave#L67)

```wave
pub fun tty_getattr(fd: i32, t: ptr<Termios>) -> i64
```

[Declaration](sys/linux/arm64/tty.wave#L71)

```wave
pub fun tty_setattr(fd: i32, action: i32, t: ptr<Termios>) -> i64
```

[Declaration](sys/linux/arm64/tty.wave#L84)

```wave
pub fun tty_getfl(fd: i32) -> i64
```

[Declaration](sys/linux/arm64/tty.wave#L88)

```wave
pub fun tty_setfl(fd: i32, flags: i32) -> i64
```

[Declaration](sys/linux/arm64/tty.wave#L92)

```wave
pub fun tty_enable_raw_nonblock(fd: i32, st: ptr<TtyRawState>) -> i64
```

[Declaration](sys/linux/arm64/tty.wave#L127)

```wave
pub fun tty_restore(fd: i32, st: ptr<TtyRawState>)
```

## `std::sys::linux::env`

[Source](sys/linux/env.wave)

[Declaration](sys/linux/env.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::linux::amd64::env")::{env_read};
```

[Declaration](sys/linux/env.wave#L11)

```wave
#[target(arch="aarch64")]
pub import("std::sys::linux::arm64::env")::{env_read};
```

[Declaration](sys/linux/env.wave#L14)

```wave
#[target(arch="riscv64")]
pub import("std::sys::linux::riscv64::env")::{env_read};
```

[Declaration](sys/linux/env.wave#L17)

```wave
#[target(arch="loongarch64")]
pub import("std::sys::linux::loong64::env")::{env_read};
```

## `std::sys::linux::event`

[Source](sys/linux/event.wave)

[Declaration](sys/linux/event.wave#L3)

```wave
pub const EVENT_BACKEND_KIND: i32 = 1;
```

[Declaration](sys/linux/event.wave#L4)

```wave
pub const EVENT_READABLE: i32 = 1;
```

[Declaration](sys/linux/event.wave#L5)

```wave
pub const EVENT_WRITABLE: i32 = 2;
```

[Declaration](sys/linux/event.wave#L6)

```wave
pub const EVENT_ERROR: i32 = 4;
```

[Declaration](sys/linux/event.wave#L7)

```wave
pub const EVENT_HANGUP: i32 = 8;
```

[Declaration](sys/linux/event.wave#L18)

```wave
pub struct NativeEvent { fd: i64; token: u64; flags: i32; }
```

[Declaration](sys/linux/event.wave#L115)

```wave
pub fun event_create(capacity: i32) -> i64
```

[Declaration](sys/linux/event.wave#L119)

```wave
pub fun event_add(handle: i64, fd: i64, token: u64, flags: i32) -> i64
```

[Declaration](sys/linux/event.wave#L123)

```wave
pub fun event_modify(handle: i64, fd: i64, token: u64, flags: i32) -> i64
```

[Declaration](sys/linux/event.wave#L127)

```wave
pub fun event_remove(handle: i64, fd: i64) -> i64
```

[Declaration](sys/linux/event.wave#L130)

```wave
pub fun event_wait(
    handle: i64, output: ptr<NativeEvent>, capacity: i32, timeout_ms: i32
) -> i64
```

[Declaration](sys/linux/event.wave#L154)

```wave
pub fun event_close(handle: i64) -> i64
```

## `std::sys::linux::fs`

[Source](sys/linux/fs.wave)

[Declaration](sys/linux/fs.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::linux::amd64::fs")::{
    FS_O_RDONLY, FS_O_WRONLY, FS_O_RDWR, FS_O_CREAT, FS_O_EXCL,
    FS_O_TRUNC, FS_O_APPEND, FS_O_NONBLOCK,
    FS_F_OK, FS_X_OK, FS_W_OK, FS_R_OK,
    FS_SEEK_SET, FS_SEEK_CUR, FS_SEEK_END, FS_F_GETFL, FS_F_SETFL,
    open, close, dup, dup2, pipe, fsync, fcntl, read, write, getcwd,
    chdir, access, lseek, unlink, mkdir, rmdir, rename_path, Stat, stat, fstat, ftruncate, same_file,
};
```

[Declaration](sys/linux/fs.wave#L18)

```wave
#[target(arch="aarch64")]
pub import("std::sys::linux::arm64::fs")::{
    FS_O_RDONLY, FS_O_WRONLY, FS_O_RDWR, FS_O_CREAT, FS_O_EXCL,
    FS_O_TRUNC, FS_O_APPEND, FS_O_NONBLOCK,
    FS_F_OK, FS_X_OK, FS_W_OK, FS_R_OK,
    FS_SEEK_SET, FS_SEEK_CUR, FS_SEEK_END, FS_F_GETFL, FS_F_SETFL,
    open, close, dup, dup2, pipe, fsync, fcntl, read, write, getcwd,
    chdir, access, lseek, unlink, mkdir, rmdir, rename_path, Stat, stat, fstat, ftruncate, same_file,
};
```

[Declaration](sys/linux/fs.wave#L28)

```wave
#[target(arch="riscv64")]
pub import("std::sys::linux::riscv64::fs")::{
    FS_O_RDONLY, FS_O_WRONLY, FS_O_RDWR, FS_O_CREAT, FS_O_EXCL,
    FS_O_TRUNC, FS_O_APPEND, FS_O_NONBLOCK,
    FS_F_OK, FS_X_OK, FS_W_OK, FS_R_OK,
    FS_SEEK_SET, FS_SEEK_CUR, FS_SEEK_END, FS_F_GETFL, FS_F_SETFL,
    open, close, dup, dup2, pipe, fsync, fcntl, read, write, getcwd,
    chdir, access, lseek, unlink, mkdir, rmdir, rename_path, Stat, stat, fstat, ftruncate, same_file,
};
```

[Declaration](sys/linux/fs.wave#L38)

```wave
#[target(arch="loongarch64")]
pub import("std::sys::linux::loong64::fs")::{
    FS_O_RDONLY, FS_O_WRONLY, FS_O_RDWR, FS_O_CREAT, FS_O_EXCL,
    FS_O_TRUNC, FS_O_APPEND, FS_O_NONBLOCK,
    FS_F_OK, FS_X_OK, FS_W_OK, FS_R_OK,
    FS_SEEK_SET, FS_SEEK_CUR, FS_SEEK_END, FS_F_GETFL, FS_F_SETFL,
    open, close, dup, dup2, pipe, fsync, fcntl, read, write, getcwd,
    chdir, access, lseek, unlink, mkdir, rmdir, rename_path, Stat, stat, fstat, ftruncate, same_file,
};
```

## `std::sys::linux::interfaces`

[Source](sys/linux/interfaces.wave)

[Declaration](sys/linux/interfaces.wave#L1)

```wave
pub struct NativeIfAddrs {
    next: ptr<NativeIfAddrs>;
    name: ptr<u8>;
    flags: u32;
    address: ptr<i8>;
    netmask: ptr<i8>;
    destination: ptr<i8>;
    data: ptr<i8>;
}
```

[Declaration](sys/linux/interfaces.wave#L11)

```wave
pub struct NativeSockAddrHeader { family: u16; }
```

[Declaration](sys/linux/interfaces.wave#L18)

```wave
pub fun interfaces_open(output: ptr<ptr<NativeIfAddrs>>) -> i64
```

[Declaration](sys/linux/interfaces.wave#L25)

```wave
pub fun interfaces_close(first: ptr<NativeIfAddrs>)
```

[Declaration](sys/linux/interfaces.wave#L28)

```wave
pub fun interface_next(value: ptr<NativeIfAddrs>) -> ptr<NativeIfAddrs>
```

[Declaration](sys/linux/interfaces.wave#L29)

```wave
pub fun interface_name(value: ptr<NativeIfAddrs>) -> ptr<u8>
```

[Declaration](sys/linux/interfaces.wave#L30)

```wave
pub fun interface_flags(value: ptr<NativeIfAddrs>) -> u32
```

[Declaration](sys/linux/interfaces.wave#L31)

```wave
pub fun interface_address(value: ptr<NativeIfAddrs>) -> ptr<i8>
```

[Declaration](sys/linux/interfaces.wave#L32)

```wave
pub fun interface_family(value: ptr<NativeIfAddrs>) -> i32
```

[Declaration](sys/linux/interfaces.wave#L38)

```wave
pub fun interface_index(name: ptr<u8>) -> u32
```

## `std::sys::linux::local_addr`

[Source](sys/linux/local_addr.wave)

[Declaration](sys/linux/local_addr.wave#L1)

```wave
pub const AF_LOCAL: i32 = 1;
```

[Declaration](sys/linux/local_addr.wave#L2)

```wave
pub const NATIVE_LOCAL_PATH_CAPACITY: i32 = 108;
```

[Declaration](sys/linux/local_addr.wave#L4)

```wave
pub struct NativeLocalAddr {
    family: u16;
    path: array<u8, 108>;
}
```

[Declaration](sys/linux/local_addr.wave#L9)

```wave
pub fun native_local_addr(path: str, output: ptr<NativeLocalAddr>) -> i32
```

[Declaration](sys/linux/local_addr.wave#L24)

```wave
pub fun native_local_abstract(name: str, output: ptr<NativeLocalAddr>) -> i32
```

## `std::sys::linux::loong64::env`

[Source](sys/linux/loong64/env.wave)

[Declaration](sys/linux/loong64/env.wave#L5)

```wave
pub import("std::sys::linux::riscv64::env")::{env_read};
```

## `std::sys::linux::loong64::fs`

[Source](sys/linux/loong64/fs.wave)

[Declaration](sys/linux/loong64/fs.wave#L6)

```wave
pub import("std::sys::linux::riscv64::fs")::{
    FS_O_RDONLY, FS_O_WRONLY, FS_O_RDWR, FS_O_CREAT, FS_O_EXCL,
    FS_O_TRUNC, FS_O_APPEND, FS_O_NONBLOCK,
    FS_F_OK, FS_X_OK, FS_W_OK, FS_R_OK,
    FS_SEEK_SET, FS_SEEK_CUR, FS_SEEK_END, FS_F_GETFL, FS_F_SETFL,
    open, close, dup, dup2, pipe, fsync, fcntl, read, write, getcwd,
    chdir, access, lseek, unlink, mkdir, rmdir, rename_path, Stat, stat, fstat, ftruncate, same_file,
};
```

## `std::sys::linux::loong64::memory`

[Source](sys/linux/loong64/memory.wave)

[Declaration](sys/linux/loong64/memory.wave#L6)

```wave
pub import("std::sys::linux::riscv64::memory")::{
    PROT_READ, PROT_WRITE, MAP_PRIVATE, MAP_ANONYMOUS,
    mmap, munmap, brk, sys_alloc, sys_free, sys_page_size,
};
```

## `std::sys::linux::loong64::process`

[Source](sys/linux/loong64/process.wave)

[Declaration](sys/linux/loong64/process.wave#L6)

```wave
pub import("std::sys::linux::riscv64::process")::{
    exit, getpid, getppid, fork, execve, waitpid, kill,
};
```

## `std::sys::linux::loong64::socket`

[Source](sys/linux/loong64/socket.wave)

[Declaration](sys/linux/loong64/socket.wave#L6)

```wave
pub import("std::sys::linux::riscv64::socket")::{
    AF_INET, AF_INET6, SOCK_STREAM, SOCK_DGRAM,
    IPPROTO_IP, IPPROTO_IPV6, IPPROTO_TCP, IPPROTO_UDP,
    SHUT_RD, SHUT_WR, SHUT_RDWR, SOL_SOCKET, SO_REUSEADDR, SO_REUSEPORT,
    SO_ERROR, SO_RCVBUF, SO_SNDBUF, SO_BROADCAST, SO_KEEPALIVE,
    TCP_NODELAY, IP_TTL,
    IP_MULTICAST_IF, IP_MULTICAST_TTL, IP_MULTICAST_LOOP,
    IP_ADD_MEMBERSHIP, IP_DROP_MEMBERSHIP,
    IPV6_MULTICAST_IF, IPV6_MULTICAST_HOPS, IPV6_MULTICAST_LOOP,
    IPV6_JOIN_GROUP, IPV6_LEAVE_GROUP, MSG_NOSIGNAL, MSG_DONTWAIT,
    POLLIN, POLLOUT, POLLERR, POLLHUP, POLLNVAL, PollFd,
    socket, bind, listen, accept, connect, getsockname, getpeername,
    shutdown, close_socket, setsockopt, getsockopt,
    poll, send, recv, sendto, recvfrom,
};
```

## `std::sys::linux::loong64::syscall`

[Source](sys/linux/loong64/syscall.wave)

[Declaration](sys/linux/loong64/syscall.wave#L9)

```wave
pub fun syscall0(id: i64) -> i64
```

[Declaration](sys/linux/loong64/syscall.wave#L19)

```wave
pub fun syscall1(id: i64, a1: i64) -> i64
```

[Declaration](sys/linux/loong64/syscall.wave#L30)

```wave
pub fun syscall2(id: i64, a1: i64, a2: i64) -> i64
```

[Declaration](sys/linux/loong64/syscall.wave#L42)

```wave
pub fun syscall3(id: i64, a1: i64, a2: i64, a3: i64) -> i64
```

[Declaration](sys/linux/loong64/syscall.wave#L55)

```wave
pub fun syscall4(id: i64, a1: i64, a2: i64, a3: i64, a4: i64) -> i64
```

[Declaration](sys/linux/loong64/syscall.wave#L69)

```wave
pub fun syscall5(id: i64, a1: i64, a2: i64, a3: i64, a4: i64, a5: i64) -> i64
```

[Declaration](sys/linux/loong64/syscall.wave#L84)

```wave
pub fun syscall6(
    id: i64,
    a1: i64,
    a2: i64,
    a3: i64,
    a4: i64,
    a5: i64,
    a6: i64
) -> i64
```

## `std::sys::linux::loong64::time`

[Source](sys/linux/loong64/time.wave)

[Declaration](sys/linux/loong64/time.wave#L6)

```wave
pub import("std::sys::linux::riscv64::time")::{TimeSpec, nanosleep, clock_gettime};
```

## `std::sys::linux::loong64::tty`

[Source](sys/linux/loong64/tty.wave)

[Declaration](sys/linux/loong64/tty.wave#L6)

```wave
pub import("std::sys::linux::riscv64::tty")::{
    TTY_SYS_IOCTL, TTY_SYS_FCNTL, TTY_TCGETS, TTY_TCSETS, TTY_TCSETSW,
    TTY_TCSETSF, TTY_TCSANOW, TTY_TCSADRAIN, TTY_TCSAFLUSH,
    TTY_F_GETFL, TTY_F_SETFL, TTY_O_NONBLOCK, TTY_ICANON, TTY_ECHO,
    TTY_VTIME_IDX, TTY_VMIN_IDX, Termios, TtyRawState,
    tty_getattr, tty_setattr, tty_getfl, tty_setfl,
    tty_enable_raw_nonblock, tty_restore,
};
```

## `std::sys::linux::memory`

[Source](sys/linux/memory.wave)

[Declaration](sys/linux/memory.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::linux::amd64::memory")::{
    PROT_READ, PROT_WRITE, MAP_PRIVATE, MAP_ANONYMOUS,
    mmap, munmap, brk, sys_alloc, sys_free, sys_page_size,
};
```

[Declaration](sys/linux/memory.wave#L14)

```wave
#[target(arch="aarch64")]
pub import("std::sys::linux::arm64::memory")::{
    PROT_READ, PROT_WRITE, MAP_PRIVATE, MAP_ANONYMOUS,
    mmap, munmap, brk, sys_alloc, sys_free, sys_page_size,
};
```

[Declaration](sys/linux/memory.wave#L20)

```wave
#[target(arch="riscv64")]
pub import("std::sys::linux::riscv64::memory")::{
    PROT_READ, PROT_WRITE, MAP_PRIVATE, MAP_ANONYMOUS,
    mmap, munmap, brk, sys_alloc, sys_free, sys_page_size,
};
```

[Declaration](sys/linux/memory.wave#L26)

```wave
#[target(arch="loongarch64")]
pub import("std::sys::linux::loong64::memory")::{
    PROT_READ, PROT_WRITE, MAP_PRIVATE, MAP_ANONYMOUS,
    mmap, munmap, brk, sys_alloc, sys_free, sys_page_size,
};
```

## `std::sys::linux::process`

[Source](sys/linux/process.wave)

[Declaration](sys/linux/process.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::linux::amd64::process")::{
    exit, getpid, getppid, fork, execve, waitpid, kill,
};
```

[Declaration](sys/linux/process.wave#L13)

```wave
#[target(arch="aarch64")]
pub import("std::sys::linux::arm64::process")::{
    exit, getpid, getppid, fork, execve, waitpid, kill,
};
```

[Declaration](sys/linux/process.wave#L18)

```wave
#[target(arch="riscv64")]
pub import("std::sys::linux::riscv64::process")::{
    exit, getpid, getppid, fork, execve, waitpid, kill,
};
```

[Declaration](sys/linux/process.wave#L23)

```wave
#[target(arch="loongarch64")]
pub import("std::sys::linux::loong64::process")::{
    exit, getpid, getppid, fork, execve, waitpid, kill,
};
```

## `std::sys::linux::resolver`

[Source](sys/linux/resolver.wave)

[Declaration](sys/linux/resolver.wave#L43)

```wave
pub struct NativeAddrInfo {
    family: i32;
    address_length: u32;
    v4: NativeSockAddrV4;
    v6: NativeSockAddrV6;
    next: ptr<NativeAddrInfo>;
}
```

[Declaration](sys/linux/resolver.wave#L74)

```wave
pub fun resolver_open(
    host: str, service: str, socket_type: i32, protocol: i32,
    result: ptr<ptr<NativeAddrInfo>>
) -> i64
```

[Declaration](sys/linux/resolver.wave#L170)

```wave
pub fun resolver_close(result: ptr<NativeAddrInfo>)
```

[Declaration](sys/linux/resolver.wave#L175)

```wave
pub fun resolver_family(result: ptr<NativeAddrInfo>) -> i32
```

[Declaration](sys/linux/resolver.wave#L178)

```wave
pub fun resolver_address(result: ptr<NativeAddrInfo>) -> ptr<i8>
```

[Declaration](sys/linux/resolver.wave#L184)

```wave
pub fun resolver_address_length(result: ptr<NativeAddrInfo>) -> i64
```

[Declaration](sys/linux/resolver.wave#L187)

```wave
pub fun resolver_next(result: ptr<NativeAddrInfo>) -> ptr<NativeAddrInfo>
```

## `std::sys::linux::riscv64::env`

[Source](sys/linux/riscv64/env.wave)

[Declaration](sys/linux/riscv64/env.wave#L60)

```wave
pub fun env_read(buf: ptr<u8>, cap: i64) -> i64
```

## `std::sys::linux::riscv64::fs`

[Source](sys/linux/riscv64/fs.wave)

[Declaration](sys/linux/riscv64/fs.wave#L32)

```wave
pub const FS_O_RDONLY: i32 = 0;
```

[Declaration](sys/linux/riscv64/fs.wave#L33)

```wave
pub const FS_O_WRONLY: i32 = 1;
```

[Declaration](sys/linux/riscv64/fs.wave#L34)

```wave
pub const FS_O_RDWR: i32 = 2;
```

[Declaration](sys/linux/riscv64/fs.wave#L35)

```wave
pub const FS_O_CREAT: i32 = 64;
```

[Declaration](sys/linux/riscv64/fs.wave#L36)

```wave
pub const FS_O_EXCL: i32 = 128;
```

[Declaration](sys/linux/riscv64/fs.wave#L37)

```wave
pub const FS_O_TRUNC: i32 = 512;
```

[Declaration](sys/linux/riscv64/fs.wave#L38)

```wave
pub const FS_O_APPEND: i32 = 1024;
```

[Declaration](sys/linux/riscv64/fs.wave#L39)

```wave
pub const FS_O_NONBLOCK: i32 = 2048;
```

[Declaration](sys/linux/riscv64/fs.wave#L42)

```wave
pub const FS_F_OK: i32 = 0;
```

[Declaration](sys/linux/riscv64/fs.wave#L43)

```wave
pub const FS_X_OK: i32 = 1;
```

[Declaration](sys/linux/riscv64/fs.wave#L44)

```wave
pub const FS_W_OK: i32 = 2;
```

[Declaration](sys/linux/riscv64/fs.wave#L45)

```wave
pub const FS_R_OK: i32 = 4;
```

[Declaration](sys/linux/riscv64/fs.wave#L48)

```wave
pub const FS_SEEK_SET: i32 = 0;
```

[Declaration](sys/linux/riscv64/fs.wave#L49)

```wave
pub const FS_SEEK_CUR: i32 = 1;
```

[Declaration](sys/linux/riscv64/fs.wave#L50)

```wave
pub const FS_SEEK_END: i32 = 2;
```

[Declaration](sys/linux/riscv64/fs.wave#L53)

```wave
pub const FS_F_GETFL: i32 = 3;
```

[Declaration](sys/linux/riscv64/fs.wave#L54)

```wave
pub const FS_F_SETFL: i32 = 4;
```

[Declaration](sys/linux/riscv64/fs.wave#L63)

```wave
pub fun open(path: str, flags: i32, mode: i32) -> i64
```

[Declaration](sys/linux/riscv64/fs.wave#L67)

```wave
pub fun close(fd: i64) -> i64
```

[Declaration](sys/linux/riscv64/fs.wave#L71)

```wave
pub fun dup(fd: i64) -> i64
```

[Declaration](sys/linux/riscv64/fs.wave#L75)

```wave
pub fun dup2(oldfd: i64, newfd: i64) -> i64
```

[Declaration](sys/linux/riscv64/fs.wave#L85)

```wave
pub fun pipe(fds: ptr<i32>) -> i64
```

[Declaration](sys/linux/riscv64/fs.wave#L89)

```wave
pub fun fsync(fd: i64) -> i64
```

[Declaration](sys/linux/riscv64/fs.wave#L93)

```wave
pub fun fcntl(fd: i64, cmd: i32, arg: i64) -> i64
```

[Declaration](sys/linux/riscv64/fs.wave#L102)

```wave
pub fun read(fd: i64, buf: ptr<u8>, len: i64) -> i64
```

[Declaration](sys/linux/riscv64/fs.wave#L106)

```wave
pub fun write(fd: i64, buf: ptr<u8>, len: i64) -> i64
```

[Declaration](sys/linux/riscv64/fs.wave#L110)

```wave
pub fun getcwd(buf: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/linux/riscv64/fs.wave#L114)

```wave
pub fun chdir(path: str) -> i64
```

[Declaration](sys/linux/riscv64/fs.wave#L118)

```wave
pub fun access(path: str, mode: i32) -> i64
```

[Declaration](sys/linux/riscv64/fs.wave#L127)

```wave
pub fun lseek(fd: i64, offset: i64, whence: i32) -> i64
```

[Declaration](sys/linux/riscv64/fs.wave#L136)

```wave
pub fun unlink(path: str) -> i64
```

[Declaration](sys/linux/riscv64/fs.wave#L140)

```wave
pub fun mkdir(path: str, mode: i32) -> i64
```

[Declaration](sys/linux/riscv64/fs.wave#L144)

```wave
pub fun rmdir(path: str) -> i64
```

[Declaration](sys/linux/riscv64/fs.wave#L148)

```wave
pub fun rename_path(old_path: str, new_path: str) -> i64
```

[Declaration](sys/linux/riscv64/fs.wave#L164)

```wave
pub struct Stat {
    dev: i64;
    ino: i64;
    mode: i32;
    nlink: i32;
    uid: i32;
    gid: i32;
    rdev: i64;
    pad1: i64;
    size: i64;
    blksize: i32;
    pad2: i32;
    blocks: i64;
    atime: i64;
    atime_nsec: i64;
    mtime: i64;
    mtime_nsec: i64;
    ctime: i64;
    ctime_nsec: i64;
    unused0: i32;
    unused1: i32;
}
```

[Declaration](sys/linux/riscv64/fs.wave#L187)

```wave
pub fun stat(path: str, st: ptr<Stat>) -> i64
```

[Declaration](sys/linux/riscv64/fs.wave#L191)

```wave
pub fun fstat(fd: i64, st: ptr<Stat>) -> i64
```

[Declaration](sys/linux/riscv64/fs.wave#L196)

```wave
pub fun ftruncate(fd: i64, length: i64) -> i64
```

[Declaration](sys/linux/riscv64/fs.wave#L204)

```wave
pub fun same_file(first_fd: i64, second_fd: i64) -> i64
```

## `std::sys::linux::riscv64::memory`

[Source](sys/linux/riscv64/memory.wave)

[Declaration](sys/linux/riscv64/memory.wave#L33)

```wave
pub const PROT_READ: i32 = 1;
```

[Declaration](sys/linux/riscv64/memory.wave#L34)

```wave
pub const PROT_WRITE: i32 = 2;
```

[Declaration](sys/linux/riscv64/memory.wave#L35)

```wave
pub const MAP_PRIVATE: i32 = 2;
```

[Declaration](sys/linux/riscv64/memory.wave#L36)

```wave
pub const MAP_ANONYMOUS: i32 = 32;
```

[Declaration](sys/linux/riscv64/memory.wave#L43)

```wave
pub fun mmap(
    addr: ptr<i8>,
    length: i64,
    prot: i32,
    flags: i32,
    fd: i64,
    offset: i64
) -> ptr<i8>
```

[Declaration](sys/linux/riscv64/memory.wave#L62)

```wave
pub fun munmap(addr: ptr<i8>, length: i64) -> i64
```

[Declaration](sys/linux/riscv64/memory.wave#L71)

```wave
pub fun brk(addr: ptr<i8>) -> ptr<i8>
```

[Declaration](sys/linux/riscv64/memory.wave#L75)

```wave
pub fun sys_alloc(size: i64) -> ptr<u8>
```

[Declaration](sys/linux/riscv64/memory.wave#L96)

```wave
pub fun sys_free(p: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/linux/riscv64/memory.wave#L104)

```wave
pub fun sys_page_size() -> i64
```

## `std::sys::linux::riscv64::process`

[Source](sys/linux/riscv64/process.wave)

[Declaration](sys/linux/riscv64/process.wave#L36)

```wave
pub fun exit(code: i32) -> !
```

[Declaration](sys/linux/riscv64/process.wave#L41)

```wave
pub fun getpid() -> i64
```

[Declaration](sys/linux/riscv64/process.wave#L45)

```wave
pub fun getppid() -> i64
```

[Declaration](sys/linux/riscv64/process.wave#L54)

```wave
pub fun fork() -> i64
```

[Declaration](sys/linux/riscv64/process.wave#L59)

```wave
pub fun execve(
    path: str,
    argv: ptr<ptr<i8>>,
    envp: ptr<ptr<i8>>
) -> i64
```

[Declaration](sys/linux/riscv64/process.wave#L72)

```wave
pub fun waitpid(pid: i64, status: ptr<i32>, options: i32) -> i64
```

[Declaration](sys/linux/riscv64/process.wave#L84)

```wave
pub fun kill(pid: i64, sig: i32) -> i64
```

## `std::sys::linux::riscv64::socket`

[Source](sys/linux/riscv64/socket.wave)

[Declaration](sys/linux/riscv64/socket.wave#L38)

```wave
pub const AF_INET: i32 = 2;
```

[Declaration](sys/linux/riscv64/socket.wave#L39)

```wave
pub const AF_INET6: i32 = 10;
```

[Declaration](sys/linux/riscv64/socket.wave#L42)

```wave
pub const SOCK_STREAM: i32 = 1;
```

[Declaration](sys/linux/riscv64/socket.wave#L43)

```wave
pub const SOCK_DGRAM: i32 = 2;
```

[Declaration](sys/linux/riscv64/socket.wave#L46)

```wave
pub const IPPROTO_IP: i32 = 0;
```

[Declaration](sys/linux/riscv64/socket.wave#L47)

```wave
pub const IPPROTO_IPV6: i32 = 41;
```

[Declaration](sys/linux/riscv64/socket.wave#L48)

```wave
pub const IPPROTO_TCP: i32 = 6;
```

[Declaration](sys/linux/riscv64/socket.wave#L49)

```wave
pub const IPPROTO_UDP: i32 = 17;
```

[Declaration](sys/linux/riscv64/socket.wave#L52)

```wave
pub const SHUT_RD: i32 = 0;
```

[Declaration](sys/linux/riscv64/socket.wave#L53)

```wave
pub const SHUT_WR: i32 = 1;
```

[Declaration](sys/linux/riscv64/socket.wave#L54)

```wave
pub const SHUT_RDWR: i32 = 2;
```

[Declaration](sys/linux/riscv64/socket.wave#L57)

```wave
pub const SOL_SOCKET: i32 = 1;
```

[Declaration](sys/linux/riscv64/socket.wave#L58)

```wave
pub const SO_REUSEADDR: i32 = 2;
```

[Declaration](sys/linux/riscv64/socket.wave#L59)

```wave
pub const SO_REUSEPORT: i32 = 15;
```

[Declaration](sys/linux/riscv64/socket.wave#L60)

```wave
pub const SO_ERROR: i32 = 4;
```

[Declaration](sys/linux/riscv64/socket.wave#L61)

```wave
pub const SO_RCVBUF: i32 = 8;
```

[Declaration](sys/linux/riscv64/socket.wave#L62)

```wave
pub const SO_SNDBUF: i32 = 7;
```

[Declaration](sys/linux/riscv64/socket.wave#L63)

```wave
pub const SO_BROADCAST: i32 = 6;
```

[Declaration](sys/linux/riscv64/socket.wave#L64)

```wave
pub const SO_KEEPALIVE: i32 = 9;
```

[Declaration](sys/linux/riscv64/socket.wave#L65)

```wave
pub const TCP_NODELAY: i32 = 1;
```

[Declaration](sys/linux/riscv64/socket.wave#L66)

```wave
pub const IP_TTL: i32 = 2;
```

[Declaration](sys/linux/riscv64/socket.wave#L67)

```wave
pub const IP_MULTICAST_IF: i32 = 32;
```

[Declaration](sys/linux/riscv64/socket.wave#L68)

```wave
pub const IP_MULTICAST_TTL: i32 = 33;
```

[Declaration](sys/linux/riscv64/socket.wave#L69)

```wave
pub const IP_MULTICAST_LOOP: i32 = 34;
```

[Declaration](sys/linux/riscv64/socket.wave#L70)

```wave
pub const IP_ADD_MEMBERSHIP: i32 = 35;
```

[Declaration](sys/linux/riscv64/socket.wave#L71)

```wave
pub const IP_DROP_MEMBERSHIP: i32 = 36;
```

[Declaration](sys/linux/riscv64/socket.wave#L72)

```wave
pub const IPV6_MULTICAST_IF: i32 = 17;
```

[Declaration](sys/linux/riscv64/socket.wave#L73)

```wave
pub const IPV6_MULTICAST_HOPS: i32 = 18;
```

[Declaration](sys/linux/riscv64/socket.wave#L74)

```wave
pub const IPV6_MULTICAST_LOOP: i32 = 19;
```

[Declaration](sys/linux/riscv64/socket.wave#L75)

```wave
pub const IPV6_JOIN_GROUP: i32 = 20;
```

[Declaration](sys/linux/riscv64/socket.wave#L76)

```wave
pub const IPV6_LEAVE_GROUP: i32 = 21;
```

[Declaration](sys/linux/riscv64/socket.wave#L77)

```wave
pub const MSG_NOSIGNAL: i32 = 0x4000;
```

[Declaration](sys/linux/riscv64/socket.wave#L78)

```wave
pub const MSG_DONTWAIT: i32 = 64;
```

[Declaration](sys/linux/riscv64/socket.wave#L81)

```wave
pub const POLLIN: i16 = 0x001;
```

[Declaration](sys/linux/riscv64/socket.wave#L82)

```wave
pub const POLLOUT: i16 = 0x004;
```

[Declaration](sys/linux/riscv64/socket.wave#L83)

```wave
pub const POLLERR: i16 = 0x008;
```

[Declaration](sys/linux/riscv64/socket.wave#L84)

```wave
pub const POLLHUP: i16 = 0x010;
```

[Declaration](sys/linux/riscv64/socket.wave#L85)

```wave
pub const POLLNVAL: i16 = 0x020;
```

[Declaration](sys/linux/riscv64/socket.wave#L87)

```wave
pub struct PollFd {
    fd: i32;
    events: i16;
    revents: i16;
}
```

[Declaration](sys/linux/riscv64/socket.wave#L103)

```wave
pub fun socket(domain: i32, ty: i32, protocol: i32) -> i64
```

[Declaration](sys/linux/riscv64/socket.wave#L107)

```wave
pub fun bind(fd: i64, addr: ptr<i8>, len: i32) -> i64
```

[Declaration](sys/linux/riscv64/socket.wave#L111)

```wave
pub fun listen(fd: i64, backlog: i32) -> i64
```

[Declaration](sys/linux/riscv64/socket.wave#L115)

```wave
pub fun accept(fd: i64, addr: ptr<i8>, len: ptr<i32>) -> i64
```

[Declaration](sys/linux/riscv64/socket.wave#L119)

```wave
pub fun connect(fd: i64, addr: ptr<i8>, len: i32) -> i64
```

[Declaration](sys/linux/riscv64/socket.wave#L123)

```wave
pub fun getsockname(fd: i64, addr: ptr<i8>, len: ptr<i32>) -> i64
```

[Declaration](sys/linux/riscv64/socket.wave#L127)

```wave
pub fun getpeername(fd: i64, addr: ptr<i8>, len: ptr<i32>) -> i64
```

[Declaration](sys/linux/riscv64/socket.wave#L131)

```wave
pub fun shutdown(fd: i64, how: i32) -> i64
```

[Declaration](sys/linux/riscv64/socket.wave#L135)

```wave
pub fun close_socket(fd: i64) -> i64
```

[Declaration](sys/linux/riscv64/socket.wave#L137)

```wave
pub fun setsockopt(
    fd: i64,
    level: i32,
    optname: i32,
    optval: ptr<i8>,
    optlen: i32
) -> i64
```

[Declaration](sys/linux/riscv64/socket.wave#L154)

```wave
pub fun getsockopt(
    fd: i64,
    level: i32,
    optname: i32,
    optval: ptr<i8>,
    optlen: ptr<i32>
) -> i64
```

[Declaration](sys/linux/riscv64/socket.wave#L171)

```wave
pub fun poll(fds: ptr<PollFd>, nfds: i64, timeout_ms: i32) -> i64
```

[Declaration](sys/linux/riscv64/socket.wave#L187)

```wave
pub fun send(fd: i64, buf: ptr<u8>, len: i64, flags: i32) -> i64
```

[Declaration](sys/linux/riscv64/socket.wave#L191)

```wave
pub fun recv(fd: i64, buf: ptr<u8>, len: i64, flags: i32) -> i64
```

[Declaration](sys/linux/riscv64/socket.wave#L195)

```wave
pub fun sendto(
    fd: i64,
    buf: ptr<u8>,
    len: i64,
    flags: i32,
    addr: ptr<i8>,
    addrlen: i32
) -> i64
```

[Declaration](sys/linux/riscv64/socket.wave#L214)

```wave
pub fun recvfrom(
    fd: i64,
    buf: ptr<u8>,
    len: i64,
    flags: i32,
    addr: ptr<i8>,
    addrlen: ptr<i32>
) -> i64
```

## `std::sys::linux::riscv64::syscall`

[Source](sys/linux/riscv64/syscall.wave)

[Declaration](sys/linux/riscv64/syscall.wave#L22)

```wave
pub fun syscall0(id: i64) -> i64
```

[Declaration](sys/linux/riscv64/syscall.wave#L32)

```wave
pub fun syscall1(id: i64, a1: i64) -> i64
```

[Declaration](sys/linux/riscv64/syscall.wave#L43)

```wave
pub fun syscall2(id: i64, a1: i64, a2: i64) -> i64
```

[Declaration](sys/linux/riscv64/syscall.wave#L55)

```wave
pub fun syscall3(id: i64, a1: i64, a2: i64, a3: i64) -> i64
```

[Declaration](sys/linux/riscv64/syscall.wave#L68)

```wave
pub fun syscall4(id: i64, a1: i64, a2: i64, a3: i64, a4: i64) -> i64
```

[Declaration](sys/linux/riscv64/syscall.wave#L82)

```wave
pub fun syscall5(id: i64, a1: i64, a2: i64, a3: i64, a4: i64, a5: i64) -> i64
```

[Declaration](sys/linux/riscv64/syscall.wave#L97)

```wave
pub fun syscall6(
    id: i64,
    a1: i64,
    a2: i64,
    a3: i64,
    a4: i64,
    a5: i64,
    a6: i64
) -> i64
```

## `std::sys::linux::riscv64::time`

[Source](sys/linux/riscv64/time.wave)

[Declaration](sys/linux/riscv64/time.wave#L36)

```wave
pub struct TimeSpec {
    sec: i64;
    nsec: i64;
}
```

[Declaration](sys/linux/riscv64/time.wave#L46)

```wave
pub fun nanosleep(req: ptr<TimeSpec>, rem: ptr<TimeSpec>) -> i64
```

[Declaration](sys/linux/riscv64/time.wave#L55)

```wave
pub fun clock_gettime(clock_id: i32, tp: ptr<TimeSpec>) -> i64
```

## `std::sys::linux::riscv64::tty`

[Source](sys/linux/riscv64/tty.wave)

[Declaration](sys/linux/riscv64/tty.wave#L31)

```wave
pub const TTY_SYS_IOCTL: i64 = 29;
```

[Declaration](sys/linux/riscv64/tty.wave#L32)

```wave
pub const TTY_SYS_FCNTL: i64 = 25;
```

[Declaration](sys/linux/riscv64/tty.wave#L34)

```wave
pub const TTY_TCGETS: i64 = 0x5401;
```

[Declaration](sys/linux/riscv64/tty.wave#L35)

```wave
pub const TTY_TCSETS: i64 = 0x5402;
```

[Declaration](sys/linux/riscv64/tty.wave#L36)

```wave
pub const TTY_TCSETSW: i64 = 0x5403;
```

[Declaration](sys/linux/riscv64/tty.wave#L37)

```wave
pub const TTY_TCSETSF: i64 = 0x5404;
```

[Declaration](sys/linux/riscv64/tty.wave#L39)

```wave
pub const TTY_TCSANOW: i32 = 0;
```

[Declaration](sys/linux/riscv64/tty.wave#L40)

```wave
pub const TTY_TCSADRAIN: i32 = 1;
```

[Declaration](sys/linux/riscv64/tty.wave#L41)

```wave
pub const TTY_TCSAFLUSH: i32 = 2;
```

[Declaration](sys/linux/riscv64/tty.wave#L43)

```wave
pub const TTY_F_GETFL: i32 = 3;
```

[Declaration](sys/linux/riscv64/tty.wave#L44)

```wave
pub const TTY_F_SETFL: i32 = 4;
```

[Declaration](sys/linux/riscv64/tty.wave#L45)

```wave
pub const TTY_O_NONBLOCK: i32 = 2048;
```

[Declaration](sys/linux/riscv64/tty.wave#L47)

```wave
pub const TTY_ICANON: u32 = 2;
```

[Declaration](sys/linux/riscv64/tty.wave#L48)

```wave
pub const TTY_ECHO: u32 = 8;
```

[Declaration](sys/linux/riscv64/tty.wave#L49)

```wave
pub const TTY_VTIME_IDX: i32 = 5;
```

[Declaration](sys/linux/riscv64/tty.wave#L50)

```wave
pub const TTY_VMIN_IDX: i32 = 6;
```

[Declaration](sys/linux/riscv64/tty.wave#L52)

```wave
pub struct Termios {
    c_iflag: u32;
    c_oflag: u32;
    c_cflag: u32;
    c_lflag: u32;
    c_line: u8;
    c_cc: array<u8, 19>;
}
```

[Declaration](sys/linux/riscv64/tty.wave#L61)

```wave
pub struct TtyRawState {
    term: Termios;
    flags: i32;
    active: i32;
}
```

[Declaration](sys/linux/riscv64/tty.wave#L67)

```wave
pub fun tty_getattr(fd: i32, t: ptr<Termios>) -> i64
```

[Declaration](sys/linux/riscv64/tty.wave#L71)

```wave
pub fun tty_setattr(fd: i32, action: i32, t: ptr<Termios>) -> i64
```

[Declaration](sys/linux/riscv64/tty.wave#L84)

```wave
pub fun tty_getfl(fd: i32) -> i64
```

[Declaration](sys/linux/riscv64/tty.wave#L88)

```wave
pub fun tty_setfl(fd: i32, flags: i32) -> i64
```

[Declaration](sys/linux/riscv64/tty.wave#L92)

```wave
pub fun tty_enable_raw_nonblock(fd: i32, st: ptr<TtyRawState>) -> i64
```

[Declaration](sys/linux/riscv64/tty.wave#L127)

```wave
pub fun tty_restore(fd: i32, st: ptr<TtyRawState>)
```

## `std::sys::linux::socket`

[Source](sys/linux/socket.wave)

[Declaration](sys/linux/socket.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::linux::amd64::socket")::{
    AF_INET, AF_INET6, SOCK_STREAM, SOCK_DGRAM,
    IPPROTO_IP, IPPROTO_IPV6, IPPROTO_TCP, IPPROTO_UDP,
    SHUT_RD, SHUT_WR, SHUT_RDWR, SOL_SOCKET, SO_REUSEADDR, SO_REUSEPORT,
    SO_ERROR, SO_RCVBUF, SO_SNDBUF, SO_BROADCAST, SO_KEEPALIVE,
    TCP_NODELAY, IP_TTL,
    IP_MULTICAST_IF, IP_MULTICAST_TTL, IP_MULTICAST_LOOP,
    IP_ADD_MEMBERSHIP, IP_DROP_MEMBERSHIP,
    IPV6_MULTICAST_IF, IPV6_MULTICAST_HOPS, IPV6_MULTICAST_LOOP,
    IPV6_JOIN_GROUP, IPV6_LEAVE_GROUP, MSG_NOSIGNAL, MSG_DONTWAIT,
    POLLIN, POLLOUT, POLLERR, POLLHUP, POLLNVAL, PollFd,
    socket, bind, listen, accept, connect, getsockname, getpeername,
    shutdown, close_socket, setsockopt, getsockopt,
    poll, send, recv, sendto, recvfrom,
};
```

[Declaration](sys/linux/socket.wave#L25)

```wave
#[target(arch="aarch64")]
pub import("std::sys::linux::arm64::socket")::{
    AF_INET, AF_INET6, SOCK_STREAM, SOCK_DGRAM,
    IPPROTO_IP, IPPROTO_IPV6, IPPROTO_TCP, IPPROTO_UDP,
    SHUT_RD, SHUT_WR, SHUT_RDWR, SOL_SOCKET, SO_REUSEADDR, SO_REUSEPORT,
    SO_ERROR, SO_RCVBUF, SO_SNDBUF, SO_BROADCAST, SO_KEEPALIVE,
    TCP_NODELAY, IP_TTL,
    IP_MULTICAST_IF, IP_MULTICAST_TTL, IP_MULTICAST_LOOP,
    IP_ADD_MEMBERSHIP, IP_DROP_MEMBERSHIP,
    IPV6_MULTICAST_IF, IPV6_MULTICAST_HOPS, IPV6_MULTICAST_LOOP,
    IPV6_JOIN_GROUP, IPV6_LEAVE_GROUP, MSG_NOSIGNAL, MSG_DONTWAIT,
    POLLIN, POLLOUT, POLLERR, POLLHUP, POLLNVAL, PollFd,
    socket, bind, listen, accept, connect, getsockname, getpeername,
    shutdown, close_socket, setsockopt, getsockopt,
    poll, send, recv, sendto, recvfrom,
};
```

[Declaration](sys/linux/socket.wave#L42)

```wave
#[target(arch="riscv64")]
pub import("std::sys::linux::riscv64::socket")::{
    AF_INET, AF_INET6, SOCK_STREAM, SOCK_DGRAM,
    IPPROTO_IP, IPPROTO_IPV6, IPPROTO_TCP, IPPROTO_UDP,
    SHUT_RD, SHUT_WR, SHUT_RDWR, SOL_SOCKET, SO_REUSEADDR, SO_REUSEPORT,
    SO_ERROR, SO_RCVBUF, SO_SNDBUF, SO_BROADCAST, SO_KEEPALIVE,
    TCP_NODELAY, IP_TTL,
    IP_MULTICAST_IF, IP_MULTICAST_TTL, IP_MULTICAST_LOOP,
    IP_ADD_MEMBERSHIP, IP_DROP_MEMBERSHIP,
    IPV6_MULTICAST_IF, IPV6_MULTICAST_HOPS, IPV6_MULTICAST_LOOP,
    IPV6_JOIN_GROUP, IPV6_LEAVE_GROUP, MSG_NOSIGNAL, MSG_DONTWAIT,
    POLLIN, POLLOUT, POLLERR, POLLHUP, POLLNVAL, PollFd,
    socket, bind, listen, accept, connect, getsockname, getpeername,
    shutdown, close_socket, setsockopt, getsockopt,
    poll, send, recv, sendto, recvfrom,
};
```

[Declaration](sys/linux/socket.wave#L59)

```wave
#[target(arch="loongarch64")]
pub import("std::sys::linux::loong64::socket")::{
    AF_INET, AF_INET6, SOCK_STREAM, SOCK_DGRAM,
    IPPROTO_IP, IPPROTO_IPV6, IPPROTO_TCP, IPPROTO_UDP,
    SHUT_RD, SHUT_WR, SHUT_RDWR, SOL_SOCKET, SO_REUSEADDR, SO_REUSEPORT,
    SO_ERROR, SO_RCVBUF, SO_SNDBUF, SO_BROADCAST, SO_KEEPALIVE,
    TCP_NODELAY, IP_TTL,
    IP_MULTICAST_IF, IP_MULTICAST_TTL, IP_MULTICAST_LOOP,
    IP_ADD_MEMBERSHIP, IP_DROP_MEMBERSHIP,
    IPV6_MULTICAST_IF, IPV6_MULTICAST_HOPS, IPV6_MULTICAST_LOOP,
    IPV6_JOIN_GROUP, IPV6_LEAVE_GROUP, MSG_NOSIGNAL, MSG_DONTWAIT,
    POLLIN, POLLOUT, POLLERR, POLLHUP, POLLNVAL, PollFd,
    socket, bind, listen, accept, connect, getsockname, getpeername,
    shutdown, close_socket, setsockopt, getsockopt,
    poll, send, recv, sendto, recvfrom,
};
```

## `std::sys::linux::socket_addr`

[Source](sys/linux/socket_addr.wave)

[Declaration](sys/linux/socket_addr.wave#L7)

```wave
pub const NATIVE_SOCKADDR_V4_LEN: i32 = 16;
```

[Declaration](sys/linux/socket_addr.wave#L8)

```wave
pub const NATIVE_SOCKADDR_V6_LEN: i32 = 28;
```

[Declaration](sys/linux/socket_addr.wave#L10)

```wave
pub struct NativeSockAddrV4 {
    family: u16;
    port: u16;
    addr: u32;
    zero: array<u8, 8>;
}
```

[Declaration](sys/linux/socket_addr.wave#L17)

```wave
pub struct NativeSockAddrV6 {
    family: u16;
    port: u16;
    flowinfo: u32;
    addr: array<u8, 16>;
    scope_id: u32;
}
```

[Declaration](sys/linux/socket_addr.wave#L25)

```wave
pub fun native_sockaddr_v4(ip_network: u32, port_network: u16) -> NativeSockAddrV4
```

[Declaration](sys/linux/socket_addr.wave#L34)

```wave
pub fun native_sockaddr_v4_ip(value: NativeSockAddrV4) -> u32
```

[Declaration](sys/linux/socket_addr.wave#L35)

```wave
pub fun native_sockaddr_v4_port(value: NativeSockAddrV4) -> u16
```

[Declaration](sys/linux/socket_addr.wave#L37)

```wave
pub fun native_sockaddr_v6(
    ip: ptr<u8>, port_network: u16, flowinfo: u32, scope_id: u32
) -> NativeSockAddrV6
```

[Declaration](sys/linux/socket_addr.wave#L50)

```wave
pub fun native_sockaddr_v6_copy_ip(value: NativeSockAddrV6, dst: ptr<u8>)
```

[Declaration](sys/linux/socket_addr.wave#L56)

```wave
pub fun native_sockaddr_v6_port(value: NativeSockAddrV6) -> u16
```

## `std::sys::linux::syscall`

[Source](sys/linux/syscall.wave)

[Declaration](sys/linux/syscall.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::linux::amd64::syscall")::{
    syscall0, syscall1, syscall2, syscall3, syscall4, syscall5, syscall6,
};
```

[Declaration](sys/linux/syscall.wave#L13)

```wave
#[target(arch="aarch64")]
pub import("std::sys::linux::arm64::syscall")::{
    syscall0, syscall1, syscall2, syscall3, syscall4, syscall5, syscall6,
};
```

[Declaration](sys/linux/syscall.wave#L18)

```wave
#[target(arch="riscv64")]
pub import("std::sys::linux::riscv64::syscall")::{
    syscall0, syscall1, syscall2, syscall3, syscall4, syscall5, syscall6,
};
```

[Declaration](sys/linux/syscall.wave#L23)

```wave
#[target(arch="loongarch64")]
pub import("std::sys::linux::loong64::syscall")::{
    syscall0, syscall1, syscall2, syscall3, syscall4, syscall5, syscall6,
};
```

## `std::sys::linux::time`

[Source](sys/linux/time.wave)

[Declaration](sys/linux/time.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::linux::amd64::time")::{TimeSpec, nanosleep, clock_gettime};
```

[Declaration](sys/linux/time.wave#L11)

```wave
#[target(arch="aarch64")]
pub import("std::sys::linux::arm64::time")::{TimeSpec, nanosleep, clock_gettime};
```

[Declaration](sys/linux/time.wave#L14)

```wave
#[target(arch="riscv64")]
pub import("std::sys::linux::riscv64::time")::{TimeSpec, nanosleep, clock_gettime};
```

[Declaration](sys/linux/time.wave#L17)

```wave
#[target(arch="loongarch64")]
pub import("std::sys::linux::loong64::time")::{TimeSpec, nanosleep, clock_gettime};
```

## `std::sys::linux::tty`

[Source](sys/linux/tty.wave)

[Declaration](sys/linux/tty.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::linux::amd64::tty")::{
    TTY_SYS_IOCTL, TTY_SYS_FCNTL, TTY_TCGETS, TTY_TCSETS, TTY_TCSETSW,
    TTY_TCSETSF, TTY_TCSANOW, TTY_TCSADRAIN, TTY_TCSAFLUSH,
    TTY_F_GETFL, TTY_F_SETFL, TTY_O_NONBLOCK, TTY_ICANON, TTY_ECHO,
    TTY_VTIME_IDX, TTY_VMIN_IDX, Termios, TtyRawState,
    tty_getattr, tty_setattr, tty_getfl, tty_setfl,
    tty_enable_raw_nonblock, tty_restore,
};
```

[Declaration](sys/linux/tty.wave#L18)

```wave
#[target(arch="aarch64")]
pub import("std::sys::linux::arm64::tty")::{
    TTY_SYS_IOCTL, TTY_SYS_FCNTL, TTY_TCGETS, TTY_TCSETS, TTY_TCSETSW,
    TTY_TCSETSF, TTY_TCSANOW, TTY_TCSADRAIN, TTY_TCSAFLUSH,
    TTY_F_GETFL, TTY_F_SETFL, TTY_O_NONBLOCK, TTY_ICANON, TTY_ECHO,
    TTY_VTIME_IDX, TTY_VMIN_IDX, Termios, TtyRawState,
    tty_getattr, tty_setattr, tty_getfl, tty_setfl,
    tty_enable_raw_nonblock, tty_restore,
};
```

[Declaration](sys/linux/tty.wave#L28)

```wave
#[target(arch="riscv64")]
pub import("std::sys::linux::riscv64::tty")::{
    TTY_SYS_IOCTL, TTY_SYS_FCNTL, TTY_TCGETS, TTY_TCSETS, TTY_TCSETSW,
    TTY_TCSETSF, TTY_TCSANOW, TTY_TCSADRAIN, TTY_TCSAFLUSH,
    TTY_F_GETFL, TTY_F_SETFL, TTY_O_NONBLOCK, TTY_ICANON, TTY_ECHO,
    TTY_VTIME_IDX, TTY_VMIN_IDX, Termios, TtyRawState,
    tty_getattr, tty_setattr, tty_getfl, tty_setfl,
    tty_enable_raw_nonblock, tty_restore,
};
```

[Declaration](sys/linux/tty.wave#L38)

```wave
#[target(arch="loongarch64")]
pub import("std::sys::linux::loong64::tty")::{
    TTY_SYS_IOCTL, TTY_SYS_FCNTL, TTY_TCGETS, TTY_TCSETS, TTY_TCSETSW,
    TTY_TCSETSF, TTY_TCSANOW, TTY_TCSADRAIN, TTY_TCSAFLUSH,
    TTY_F_GETFL, TTY_F_SETFL, TTY_O_NONBLOCK, TTY_ICANON, TTY_ECHO,
    TTY_VTIME_IDX, TTY_VMIN_IDX, Termios, TtyRawState,
    tty_getattr, tty_setattr, tty_getfl, tty_setfl,
    tty_enable_raw_nonblock, tty_restore,
};
```

## `std::sys::linux::vector_io`

[Source](sys/linux/vector_io.wave)

[Declaration](sys/linux/vector_io.wave#L1)

```wave
pub struct NativeIoSlice { data: ptr<u8>; length: u64; }
```

[Declaration](sys/linux/vector_io.wave#L2)

```wave
pub struct NativeMessage {
    name: ptr<i8>;
    name_length: u32;
    vectors: ptr<NativeIoSlice>;
    vector_count: u64;
    control: ptr<i8>;
    control_length: u64;
    flags: i32;
}
```

[Declaration](sys/linux/vector_io.wave#L16)

```wave
pub fun native_io_slice(data: ptr<u8>, length: i64, output: ptr<NativeIoSlice>) -> i64
```

[Declaration](sys/linux/vector_io.wave#L35)

```wave
pub fun vector_send(fd: i64, vectors: ptr<NativeIoSlice>, count: i32, flags: i32) -> i64
```

[Declaration](sys/linux/vector_io.wave#L39)

```wave
pub fun vector_recv(fd: i64, vectors: ptr<NativeIoSlice>, count: i32, flags: i32) -> i64
```

## `std::sys::local_addr`

[Source](sys/local_addr.wave)

[Declaration](sys/local_addr.wave#L2)

```wave
#[target(os="linux")]
pub import("std::sys::linux::local_addr")::{
    AF_LOCAL, NATIVE_LOCAL_PATH_CAPACITY, NativeLocalAddr,
    native_local_addr, native_local_abstract,
};
```

[Declaration](sys/local_addr.wave#L7)

```wave
#[target(os="macos")]
pub import("std::sys::macos::local_addr")::{
    AF_LOCAL, NATIVE_LOCAL_PATH_CAPACITY, NativeLocalAddr,
    native_local_addr, native_local_abstract,
};
```

[Declaration](sys/local_addr.wave#L12)

```wave
#[target(os="freebsd")]
pub import("std::sys::freebsd::local_addr")::{
    AF_LOCAL, NATIVE_LOCAL_PATH_CAPACITY, NativeLocalAddr,
    native_local_addr, native_local_abstract,
};
```

[Declaration](sys/local_addr.wave#L17)

```wave
#[target(os="windows")]
pub import("std::sys::windows::local_addr")::{
    AF_LOCAL, NATIVE_LOCAL_PATH_CAPACITY, NativeLocalAddr,
    native_local_addr, native_local_abstract,
};
```

## `std::sys::macos::amd64::env`

[Source](sys/macos/amd64/env.wave)

[Declaration](sys/macos/amd64/env.wave#L20)

```wave
pub import("std::sys::macos::common::env")::{ERR_NOT_IMPLEMENTED, env_read};
```

## `std::sys::macos::amd64::fs`

[Source](sys/macos/amd64/fs.wave)

[Declaration](sys/macos/amd64/fs.wave#L35)

```wave
pub const FS_O_RDONLY: i32 = 0;
```

[Declaration](sys/macos/amd64/fs.wave#L36)

```wave
pub const FS_O_WRONLY: i32 = 1;
```

[Declaration](sys/macos/amd64/fs.wave#L37)

```wave
pub const FS_O_RDWR: i32 = 2;
```

[Declaration](sys/macos/amd64/fs.wave#L38)

```wave
pub const FS_O_NONBLOCK: i32 = 4;
```

[Declaration](sys/macos/amd64/fs.wave#L39)

```wave
pub const FS_O_APPEND: i32 = 8;
```

[Declaration](sys/macos/amd64/fs.wave#L40)

```wave
pub const FS_O_CREAT: i32 = 512;
```

[Declaration](sys/macos/amd64/fs.wave#L41)

```wave
pub const FS_O_TRUNC: i32 = 1024;
```

[Declaration](sys/macos/amd64/fs.wave#L42)

```wave
pub const FS_O_EXCL: i32 = 2048;
```

[Declaration](sys/macos/amd64/fs.wave#L45)

```wave
pub const FS_F_OK: i32 = 0;
```

[Declaration](sys/macos/amd64/fs.wave#L46)

```wave
pub const FS_X_OK: i32 = 1;
```

[Declaration](sys/macos/amd64/fs.wave#L47)

```wave
pub const FS_W_OK: i32 = 2;
```

[Declaration](sys/macos/amd64/fs.wave#L48)

```wave
pub const FS_R_OK: i32 = 4;
```

[Declaration](sys/macos/amd64/fs.wave#L51)

```wave
pub const FS_SEEK_SET: i32 = 0;
```

[Declaration](sys/macos/amd64/fs.wave#L52)

```wave
pub const FS_SEEK_CUR: i32 = 1;
```

[Declaration](sys/macos/amd64/fs.wave#L53)

```wave
pub const FS_SEEK_END: i32 = 2;
```

[Declaration](sys/macos/amd64/fs.wave#L56)

```wave
pub const FS_F_GETFL: i32 = 3;
```

[Declaration](sys/macos/amd64/fs.wave#L57)

```wave
pub const FS_F_SETFL: i32 = 4;
```

[Declaration](sys/macos/amd64/fs.wave#L62)

```wave
pub struct Stat {
    dev: i32;
    mode: i16;
    nlink: i16;
    ino: i64;
    uid: i32;
    gid: i32;
    rdev: i32;
    pad0: i32;
    atime: i64;
    atime_nsec: i64;
    mtime: i64;
    mtime_nsec: i64;
    ctime: i64;
    ctime_nsec: i64;
    birthtime: i64;
    birthtime_nsec: i64;
    size: i64;
    blocks: i64;
    blksize: i32;
    flags: i32;
    generation: i32;
    spare: i32;
    qspare0: i64;
    qspare1: i64;
}
```

[Declaration](sys/macos/amd64/fs.wave#L89)

```wave
pub fun open(path: str, flags: i32, mode: i32) -> i64
```

[Declaration](sys/macos/amd64/fs.wave#L93)

```wave
pub fun close(fd: i64) -> i64
```

[Declaration](sys/macos/amd64/fs.wave#L97)

```wave
pub fun dup(fd: i64) -> i64
```

[Declaration](sys/macos/amd64/fs.wave#L101)

```wave
pub fun pipe(fds: ptr<i32>) -> i64
```

[Declaration](sys/macos/amd64/fs.wave#L125)

```wave
pub fun dup2(oldfd: i64, newfd: i64) -> i64
```

[Declaration](sys/macos/amd64/fs.wave#L129)

```wave
pub fun fsync(fd: i64) -> i64
```

[Declaration](sys/macos/amd64/fs.wave#L133)

```wave
pub fun fcntl(fd: i64, cmd: i32, arg: i64) -> i64
```

[Declaration](sys/macos/amd64/fs.wave#L137)

```wave
pub fun read(fd: i64, buf: ptr<u8>, len: i64) -> i64
```

[Declaration](sys/macos/amd64/fs.wave#L141)

```wave
pub fun write(fd: i64, buf: ptr<u8>, len: i64) -> i64
```

[Declaration](sys/macos/amd64/fs.wave#L145)

```wave
pub fun getcwd(buf: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/macos/amd64/fs.wave#L180)

```wave
pub fun chdir(path: str) -> i64
```

[Declaration](sys/macos/amd64/fs.wave#L184)

```wave
pub fun access(path: str, mode: i32) -> i64
```

[Declaration](sys/macos/amd64/fs.wave#L188)

```wave
pub fun lseek(fd: i64, offset: i64, whence: i32) -> i64
```

[Declaration](sys/macos/amd64/fs.wave#L192)

```wave
pub fun unlink(path: str) -> i64
```

[Declaration](sys/macos/amd64/fs.wave#L196)

```wave
pub fun mkdir(path: str, mode: i32) -> i64
```

[Declaration](sys/macos/amd64/fs.wave#L200)

```wave
pub fun rmdir(path: str) -> i64
```

[Declaration](sys/macos/amd64/fs.wave#L204)

```wave
pub fun rename_path(old_path: str, new_path: str) -> i64
```

[Declaration](sys/macos/amd64/fs.wave#L208)

```wave
pub fun stat(path: str, st: ptr<Stat>) -> i64
```

[Declaration](sys/macos/amd64/fs.wave#L212)

```wave
pub fun fstat(fd: i64, st: ptr<Stat>) -> i64
```

[Declaration](sys/macos/amd64/fs.wave#L217)

```wave
pub fun ftruncate(fd: i64, length: i64) -> i64
```

[Declaration](sys/macos/amd64/fs.wave#L225)

```wave
pub fun same_file(first_fd: i64, second_fd: i64) -> i64
```

## `std::sys::macos::amd64::memory`

[Source](sys/macos/amd64/memory.wave)

[Declaration](sys/macos/amd64/memory.wave#L36)

```wave
pub const PROT_READ: i32 = 1;
```

[Declaration](sys/macos/amd64/memory.wave#L37)

```wave
pub const PROT_WRITE: i32 = 2;
```

[Declaration](sys/macos/amd64/memory.wave#L38)

```wave
pub const MAP_PRIVATE: i32 = 2;
```

[Declaration](sys/macos/amd64/memory.wave#L39)

```wave
pub const MAP_ANON: i32 = 0x1000;
```

[Declaration](sys/macos/amd64/memory.wave#L40)

```wave
pub const MAP_ANONYMOUS: i32 = MAP_ANON;
```

[Declaration](sys/macos/amd64/memory.wave#L42)

```wave
pub fun mmap(
    addr: ptr<i8>,
    length: i64,
    prot: i32,
    flags: i32,
    fd: i64,
    offset: i64
) -> ptr<i8>
```

[Declaration](sys/macos/amd64/memory.wave#L61)

```wave
pub fun munmap(addr: ptr<i8>, length: i64) -> i64
```

[Declaration](sys/macos/amd64/memory.wave#L65)

```wave
pub fun brk(addr: ptr<i8>) -> ptr<i8>
```

[Declaration](sys/macos/amd64/memory.wave#L69)

```wave
pub fun sys_alloc(size: i64) -> ptr<u8>
```

[Declaration](sys/macos/amd64/memory.wave#L90)

```wave
pub fun sys_free(p: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/macos/amd64/memory.wave#L98)

```wave
pub fun sys_page_size() -> i64
```

## `std::sys::macos::amd64::process`

[Source](sys/macos/amd64/process.wave)

[Declaration](sys/macos/amd64/process.wave#L34)

```wave
pub fun exit(code: i32) -> !
```

[Declaration](sys/macos/amd64/process.wave#L39)

```wave
pub fun getpid() -> i64
```

[Declaration](sys/macos/amd64/process.wave#L43)

```wave
pub fun getppid() -> i64
```

[Declaration](sys/macos/amd64/process.wave#L47)

```wave
pub fun fork() -> i64
```

[Declaration](sys/macos/amd64/process.wave#L66)

```wave
pub fun execve(
    path: str,
    argv: ptr<ptr<i8>>,
    envp: ptr<ptr<i8>>
) -> i64
```

[Declaration](sys/macos/amd64/process.wave#L74)

```wave
pub fun waitpid(pid: i64, status: ptr<i32>, options: i32) -> i64
```

[Declaration](sys/macos/amd64/process.wave#L81)

```wave
pub fun kill(pid: i64, sig: i32) -> i64
```

## `std::sys::macos::amd64::socket`

[Source](sys/macos/amd64/socket.wave)

[Declaration](sys/macos/amd64/socket.wave#L38)

```wave
pub const AF_INET: i32 = 2;
```

[Declaration](sys/macos/amd64/socket.wave#L39)

```wave
pub const AF_INET6: i32 = 30;
```

[Declaration](sys/macos/amd64/socket.wave#L41)

```wave
pub const SOCK_STREAM: i32 = 1;
```

[Declaration](sys/macos/amd64/socket.wave#L42)

```wave
pub const SOCK_DGRAM: i32 = 2;
```

[Declaration](sys/macos/amd64/socket.wave#L44)

```wave
pub const IPPROTO_IP: i32 = 0;
```

[Declaration](sys/macos/amd64/socket.wave#L45)

```wave
pub const IPPROTO_IPV6: i32 = 41;
```

[Declaration](sys/macos/amd64/socket.wave#L46)

```wave
pub const IPPROTO_TCP: i32 = 6;
```

[Declaration](sys/macos/amd64/socket.wave#L47)

```wave
pub const IPPROTO_UDP: i32 = 17;
```

[Declaration](sys/macos/amd64/socket.wave#L49)

```wave
pub const SHUT_RD: i32 = 0;
```

[Declaration](sys/macos/amd64/socket.wave#L50)

```wave
pub const SHUT_WR: i32 = 1;
```

[Declaration](sys/macos/amd64/socket.wave#L51)

```wave
pub const SHUT_RDWR: i32 = 2;
```

[Declaration](sys/macos/amd64/socket.wave#L53)

```wave
pub const SOL_SOCKET: i32 = 0xFFFF;
```

[Declaration](sys/macos/amd64/socket.wave#L54)

```wave
pub const SO_REUSEADDR: i32 = 0x0004;
```

[Declaration](sys/macos/amd64/socket.wave#L55)

```wave
pub const SO_REUSEPORT: i32 = 0x0200;
```

[Declaration](sys/macos/amd64/socket.wave#L56)

```wave
pub const SO_ERROR: i32 = 0x1007;
```

[Declaration](sys/macos/amd64/socket.wave#L57)

```wave
pub const SO_RCVBUF: i32 = 0x1002;
```

[Declaration](sys/macos/amd64/socket.wave#L58)

```wave
pub const SO_SNDBUF: i32 = 0x1001;
```

[Declaration](sys/macos/amd64/socket.wave#L59)

```wave
pub const SO_BROADCAST: i32 = 0x0020;
```

[Declaration](sys/macos/amd64/socket.wave#L60)

```wave
pub const SO_KEEPALIVE: i32 = 0x0008;
```

[Declaration](sys/macos/amd64/socket.wave#L61)

```wave
pub const TCP_NODELAY: i32 = 1;
```

[Declaration](sys/macos/amd64/socket.wave#L62)

```wave
pub const IP_TTL: i32 = 4;
```

[Declaration](sys/macos/amd64/socket.wave#L63)

```wave
pub const IP_MULTICAST_IF: i32 = 9;
```

[Declaration](sys/macos/amd64/socket.wave#L64)

```wave
pub const IP_MULTICAST_TTL: i32 = 10;
```

[Declaration](sys/macos/amd64/socket.wave#L65)

```wave
pub const IP_MULTICAST_LOOP: i32 = 11;
```

[Declaration](sys/macos/amd64/socket.wave#L66)

```wave
pub const IP_ADD_MEMBERSHIP: i32 = 12;
```

[Declaration](sys/macos/amd64/socket.wave#L67)

```wave
pub const IP_DROP_MEMBERSHIP: i32 = 13;
```

[Declaration](sys/macos/amd64/socket.wave#L68)

```wave
pub const IPV6_MULTICAST_IF: i32 = 9;
```

[Declaration](sys/macos/amd64/socket.wave#L69)

```wave
pub const IPV6_MULTICAST_HOPS: i32 = 10;
```

[Declaration](sys/macos/amd64/socket.wave#L70)

```wave
pub const IPV6_MULTICAST_LOOP: i32 = 11;
```

[Declaration](sys/macos/amd64/socket.wave#L71)

```wave
pub const IPV6_JOIN_GROUP: i32 = 12;
```

[Declaration](sys/macos/amd64/socket.wave#L72)

```wave
pub const IPV6_LEAVE_GROUP: i32 = 13;
```

[Declaration](sys/macos/amd64/socket.wave#L73)

```wave
pub const SO_NOSIGPIPE: i32 = 0x1022;
```

[Declaration](sys/macos/amd64/socket.wave#L74)

```wave
pub const MSG_NOSIGNAL: i32 = 0;
```

[Declaration](sys/macos/amd64/socket.wave#L75)

```wave
pub const MSG_DONTWAIT: i32 = 128;
```

[Declaration](sys/macos/amd64/socket.wave#L77)

```wave
pub const POLLIN: i16 = 0x001;
```

[Declaration](sys/macos/amd64/socket.wave#L78)

```wave
pub const POLLOUT: i16 = 0x004;
```

[Declaration](sys/macos/amd64/socket.wave#L79)

```wave
pub const POLLERR: i16 = 0x008;
```

[Declaration](sys/macos/amd64/socket.wave#L80)

```wave
pub const POLLHUP: i16 = 0x010;
```

[Declaration](sys/macos/amd64/socket.wave#L81)

```wave
pub const POLLNVAL: i16 = 0x020;
```

[Declaration](sys/macos/amd64/socket.wave#L83)

```wave
pub struct PollFd {
    fd: i32;
    events: i16;
    revents: i16;
}
```

[Declaration](sys/macos/amd64/socket.wave#L89)

```wave
pub fun socket(domain: i32, ty: i32, protocol: i32) -> i64
```

[Declaration](sys/macos/amd64/socket.wave#L93)

```wave
pub fun bind(fd: i64, addr: ptr<i8>, len: i32) -> i64
```

[Declaration](sys/macos/amd64/socket.wave#L97)

```wave
pub fun listen(fd: i64, backlog: i32) -> i64
```

[Declaration](sys/macos/amd64/socket.wave#L101)

```wave
pub fun accept(fd: i64, addr: ptr<i8>, len: ptr<i32>) -> i64
```

[Declaration](sys/macos/amd64/socket.wave#L105)

```wave
pub fun connect(fd: i64, addr: ptr<i8>, len: i32) -> i64
```

[Declaration](sys/macos/amd64/socket.wave#L109)

```wave
pub fun getsockname(fd: i64, addr: ptr<i8>, len: ptr<i32>) -> i64
```

[Declaration](sys/macos/amd64/socket.wave#L113)

```wave
pub fun getpeername(fd: i64, addr: ptr<i8>, len: ptr<i32>) -> i64
```

[Declaration](sys/macos/amd64/socket.wave#L117)

```wave
pub fun shutdown(fd: i64, how: i32) -> i64
```

[Declaration](sys/macos/amd64/socket.wave#L121)

```wave
pub fun close_socket(fd: i64) -> i64
```

[Declaration](sys/macos/amd64/socket.wave#L123)

```wave
pub fun setsockopt(
    fd: i64,
    level: i32,
    optname: i32,
    optval: ptr<i8>,
    optlen: i32
) -> i64
```

[Declaration](sys/macos/amd64/socket.wave#L140)

```wave
pub fun getsockopt(
    fd: i64,
    level: i32,
    optname: i32,
    optval: ptr<i8>,
    optlen: ptr<i32>
) -> i64
```

[Declaration](sys/macos/amd64/socket.wave#L157)

```wave
pub fun poll(fds: ptr<PollFd>, nfds: i64, timeout_ms: i32) -> i64
```

[Declaration](sys/macos/amd64/socket.wave#L161)

```wave
pub fun send(fd: i64, buf: ptr<u8>, len: i64, flags: i32) -> i64
```

[Declaration](sys/macos/amd64/socket.wave#L165)

```wave
pub fun recv(fd: i64, buf: ptr<u8>, len: i64, flags: i32) -> i64
```

[Declaration](sys/macos/amd64/socket.wave#L169)

```wave
pub fun sendto(
    fd: i64,
    buf: ptr<u8>,
    len: i64,
    flags: i32,
    addr: ptr<i8>,
    addrlen: i32
) -> i64
```

[Declaration](sys/macos/amd64/socket.wave#L195)

```wave
pub fun recvfrom(
    fd: i64,
    buf: ptr<u8>,
    len: i64,
    flags: i32,
    addr: ptr<i8>,
    addrlen: ptr<i32>
) -> i64
```

## `std::sys::macos::amd64::syscall`

[Source](sys/macos/amd64/syscall.wave)

[Declaration](sys/macos/amd64/syscall.wave#L31)

```wave
pub const MACOS_SYSCALL_BASE: i64 = 0x2000000;
```

[Declaration](sys/macos/amd64/syscall.wave#L33)

```wave
pub fun syscall0(id: i64) -> i64
```

[Declaration](sys/macos/amd64/syscall.wave#L46)

```wave
pub fun syscall1(id: i64, a1: i64) -> i64
```

[Declaration](sys/macos/amd64/syscall.wave#L60)

```wave
pub fun syscall2(id: i64, a1: i64, a2: i64) -> i64
```

[Declaration](sys/macos/amd64/syscall.wave#L75)

```wave
pub fun syscall3(id: i64, a1: i64, a2: i64, a3: i64) -> i64
```

[Declaration](sys/macos/amd64/syscall.wave#L91)

```wave
pub fun syscall4(id: i64, a1: i64, a2: i64, a3: i64, a4: i64) -> i64
```

[Declaration](sys/macos/amd64/syscall.wave#L108)

```wave
pub fun syscall5(id: i64, a1: i64, a2: i64, a3: i64, a4: i64, a5: i64) -> i64
```

[Declaration](sys/macos/amd64/syscall.wave#L126)

```wave
pub fun syscall6(id: i64, a1: i64, a2: i64, a3: i64, a4: i64, a5: i64, a6: i64) -> i64
```

## `std::sys::macos::amd64::time`

[Source](sys/macos/amd64/time.wave)

[Declaration](sys/macos/amd64/time.wave#L39)

```wave
pub struct TimeSpec {
    sec: i64;
    nsec: i64;
}
```

[Declaration](sys/macos/amd64/time.wave#L105)

```wave
pub fun nanosleep(req: ptr<TimeSpec>, rem: ptr<TimeSpec>) -> i64
```

[Declaration](sys/macos/amd64/time.wave#L174)

```wave
pub fun clock_gettime(clock_id: i32, tp: ptr<TimeSpec>) -> i64
```

## `std::sys::macos::amd64::tty`

[Source](sys/macos/amd64/tty.wave)

[Declaration](sys/macos/amd64/tty.wave#L34)

```wave
pub const TTY_SYS_IOCTL: i64 = 54;
```

[Declaration](sys/macos/amd64/tty.wave#L35)

```wave
pub const TTY_SYS_FCNTL: i64 = 92;
```

[Declaration](sys/macos/amd64/tty.wave#L37)

```wave
pub const TTY_TCGETS: i64 = 0x40487413;
```

[Declaration](sys/macos/amd64/tty.wave#L38)

```wave
pub const TTY_TCSETS: i64 = 0x80487414;
```

[Declaration](sys/macos/amd64/tty.wave#L39)

```wave
pub const TTY_TCSETSW: i64 = 0x80487415;
```

[Declaration](sys/macos/amd64/tty.wave#L40)

```wave
pub const TTY_TCSETSF: i64 = 0x80487416;
```

[Declaration](sys/macos/amd64/tty.wave#L42)

```wave
pub const TTY_TCSANOW: i32 = 0;
```

[Declaration](sys/macos/amd64/tty.wave#L43)

```wave
pub const TTY_TCSADRAIN: i32 = 1;
```

[Declaration](sys/macos/amd64/tty.wave#L44)

```wave
pub const TTY_TCSAFLUSH: i32 = 2;
```

[Declaration](sys/macos/amd64/tty.wave#L46)

```wave
pub const TTY_F_GETFL: i32 = 3;
```

[Declaration](sys/macos/amd64/tty.wave#L47)

```wave
pub const TTY_F_SETFL: i32 = 4;
```

[Declaration](sys/macos/amd64/tty.wave#L48)

```wave
pub const TTY_O_NONBLOCK: i32 = 4;
```

[Declaration](sys/macos/amd64/tty.wave#L50)

```wave
pub const TTY_ICANON: u64 = 0x00000100;
```

[Declaration](sys/macos/amd64/tty.wave#L51)

```wave
pub const TTY_ECHO: u64 = 0x00000008;
```

[Declaration](sys/macos/amd64/tty.wave#L53)

```wave
pub struct Termios {
    c_iflag: u64;
    c_oflag: u64;
    c_cflag: u64;
    c_lflag: u64;
    c_cc: array<u8, 20>;
    c_ispeed: u64;
    c_ospeed: u64;
}
```

[Declaration](sys/macos/amd64/tty.wave#L63)

```wave
pub struct TtyRawState {
    term: Termios;
    flags: i32;
    active: i32;
}
```

[Declaration](sys/macos/amd64/tty.wave#L69)

```wave
pub fun tty_getattr(fd: i32, t: ptr<Termios>) -> i64
```

[Declaration](sys/macos/amd64/tty.wave#L73)

```wave
pub fun tty_setattr(fd: i32, action: i32, t: ptr<Termios>) -> i64
```

[Declaration](sys/macos/amd64/tty.wave#L86)

```wave
pub fun tty_getfl(fd: i32) -> i64
```

[Declaration](sys/macos/amd64/tty.wave#L90)

```wave
pub fun tty_setfl(fd: i32, flags: i32) -> i64
```

[Declaration](sys/macos/amd64/tty.wave#L94)

```wave
pub fun tty_enable_raw_nonblock(fd: i32, st: ptr<TtyRawState>) -> i64
```

[Declaration](sys/macos/amd64/tty.wave#L131)

```wave
pub fun tty_restore(fd: i32, st: ptr<TtyRawState>)
```

## `std::sys::macos::arm64::env`

[Source](sys/macos/arm64/env.wave)

[Declaration](sys/macos/arm64/env.wave#L20)

```wave
pub import("std::sys::macos::common::env")::{ERR_NOT_IMPLEMENTED, env_read};
```

## `std::sys::macos::arm64::fs`

[Source](sys/macos/arm64/fs.wave)

[Declaration](sys/macos/arm64/fs.wave#L34)

```wave
pub const FS_O_RDONLY: i32 = 0;
```

[Declaration](sys/macos/arm64/fs.wave#L35)

```wave
pub const FS_O_WRONLY: i32 = 1;
```

[Declaration](sys/macos/arm64/fs.wave#L36)

```wave
pub const FS_O_RDWR: i32 = 2;
```

[Declaration](sys/macos/arm64/fs.wave#L37)

```wave
pub const FS_O_NONBLOCK: i32 = 4;
```

[Declaration](sys/macos/arm64/fs.wave#L38)

```wave
pub const FS_O_APPEND: i32 = 8;
```

[Declaration](sys/macos/arm64/fs.wave#L39)

```wave
pub const FS_O_CREAT: i32 = 512;
```

[Declaration](sys/macos/arm64/fs.wave#L40)

```wave
pub const FS_O_TRUNC: i32 = 1024;
```

[Declaration](sys/macos/arm64/fs.wave#L41)

```wave
pub const FS_O_EXCL: i32 = 2048;
```

[Declaration](sys/macos/arm64/fs.wave#L44)

```wave
pub const FS_F_OK: i32 = 0;
```

[Declaration](sys/macos/arm64/fs.wave#L45)

```wave
pub const FS_X_OK: i32 = 1;
```

[Declaration](sys/macos/arm64/fs.wave#L46)

```wave
pub const FS_W_OK: i32 = 2;
```

[Declaration](sys/macos/arm64/fs.wave#L47)

```wave
pub const FS_R_OK: i32 = 4;
```

[Declaration](sys/macos/arm64/fs.wave#L50)

```wave
pub const FS_SEEK_SET: i32 = 0;
```

[Declaration](sys/macos/arm64/fs.wave#L51)

```wave
pub const FS_SEEK_CUR: i32 = 1;
```

[Declaration](sys/macos/arm64/fs.wave#L52)

```wave
pub const FS_SEEK_END: i32 = 2;
```

[Declaration](sys/macos/arm64/fs.wave#L55)

```wave
pub const FS_F_GETFL: i32 = 3;
```

[Declaration](sys/macos/arm64/fs.wave#L56)

```wave
pub const FS_F_SETFL: i32 = 4;
```

[Declaration](sys/macos/arm64/fs.wave#L61)

```wave
pub struct Stat {
    dev: i32;
    mode: i16;
    nlink: i16;
    ino: i64;
    uid: i32;
    gid: i32;
    rdev: i32;
    pad0: i32;
    atime: i64;
    atime_nsec: i64;
    mtime: i64;
    mtime_nsec: i64;
    ctime: i64;
    ctime_nsec: i64;
    birthtime: i64;
    birthtime_nsec: i64;
    size: i64;
    blocks: i64;
    blksize: i32;
    flags: i32;
    generation: i32;
    spare: i32;
    qspare0: i64;
    qspare1: i64;
}
```

[Declaration](sys/macos/arm64/fs.wave#L88)

```wave
pub fun open(path: str, flags: i32, mode: i32) -> i64
```

[Declaration](sys/macos/arm64/fs.wave#L92)

```wave
pub fun close(fd: i64) -> i64
```

[Declaration](sys/macos/arm64/fs.wave#L96)

```wave
pub fun dup(fd: i64) -> i64
```

[Declaration](sys/macos/arm64/fs.wave#L100)

```wave
pub fun pipe(fds: ptr<i32>) -> i64
```

[Declaration](sys/macos/arm64/fs.wave#L123)

```wave
pub fun dup2(oldfd: i64, newfd: i64) -> i64
```

[Declaration](sys/macos/arm64/fs.wave#L127)

```wave
pub fun fsync(fd: i64) -> i64
```

[Declaration](sys/macos/arm64/fs.wave#L131)

```wave
pub fun fcntl(fd: i64, cmd: i32, arg: i64) -> i64
```

[Declaration](sys/macos/arm64/fs.wave#L135)

```wave
pub fun read(fd: i64, buf: ptr<u8>, len: i64) -> i64
```

[Declaration](sys/macos/arm64/fs.wave#L139)

```wave
pub fun write(fd: i64, buf: ptr<u8>, len: i64) -> i64
```

[Declaration](sys/macos/arm64/fs.wave#L143)

```wave
pub fun getcwd(buf: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/macos/arm64/fs.wave#L178)

```wave
pub fun chdir(path: str) -> i64
```

[Declaration](sys/macos/arm64/fs.wave#L182)

```wave
pub fun access(path: str, mode: i32) -> i64
```

[Declaration](sys/macos/arm64/fs.wave#L186)

```wave
pub fun lseek(fd: i64, offset: i64, whence: i32) -> i64
```

[Declaration](sys/macos/arm64/fs.wave#L190)

```wave
pub fun unlink(path: str) -> i64
```

[Declaration](sys/macos/arm64/fs.wave#L194)

```wave
pub fun mkdir(path: str, mode: i32) -> i64
```

[Declaration](sys/macos/arm64/fs.wave#L198)

```wave
pub fun rmdir(path: str) -> i64
```

[Declaration](sys/macos/arm64/fs.wave#L202)

```wave
pub fun rename_path(old_path: str, new_path: str) -> i64
```

[Declaration](sys/macos/arm64/fs.wave#L206)

```wave
pub fun stat(path: str, st: ptr<Stat>) -> i64
```

[Declaration](sys/macos/arm64/fs.wave#L210)

```wave
pub fun fstat(fd: i64, st: ptr<Stat>) -> i64
```

[Declaration](sys/macos/arm64/fs.wave#L215)

```wave
pub fun ftruncate(fd: i64, length: i64) -> i64
```

[Declaration](sys/macos/arm64/fs.wave#L223)

```wave
pub fun same_file(first_fd: i64, second_fd: i64) -> i64
```

## `std::sys::macos::arm64::memory`

[Source](sys/macos/arm64/memory.wave)

[Declaration](sys/macos/arm64/memory.wave#L35)

```wave
pub const PROT_READ: i32 = 1;
```

[Declaration](sys/macos/arm64/memory.wave#L36)

```wave
pub const PROT_WRITE: i32 = 2;
```

[Declaration](sys/macos/arm64/memory.wave#L37)

```wave
pub const MAP_PRIVATE: i32 = 2;
```

[Declaration](sys/macos/arm64/memory.wave#L38)

```wave
pub const MAP_ANON: i32 = 0x1000;
```

[Declaration](sys/macos/arm64/memory.wave#L39)

```wave
pub const MAP_ANONYMOUS: i32 = MAP_ANON;
```

[Declaration](sys/macos/arm64/memory.wave#L41)

```wave
pub fun mmap(
    addr: ptr<i8>,
    length: i64,
    prot: i32,
    flags: i32,
    fd: i64,
    offset: i64
) -> ptr<i8>
```

[Declaration](sys/macos/arm64/memory.wave#L60)

```wave
pub fun munmap(addr: ptr<i8>, length: i64) -> i64
```

[Declaration](sys/macos/arm64/memory.wave#L64)

```wave
pub fun brk(addr: ptr<i8>) -> ptr<i8>
```

[Declaration](sys/macos/arm64/memory.wave#L68)

```wave
pub fun sys_alloc(size: i64) -> ptr<u8>
```

[Declaration](sys/macos/arm64/memory.wave#L89)

```wave
pub fun sys_free(p: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/macos/arm64/memory.wave#L97)

```wave
pub fun sys_page_size() -> i64
```

## `std::sys::macos::arm64::process`

[Source](sys/macos/arm64/process.wave)

[Declaration](sys/macos/arm64/process.wave#L33)

```wave
pub fun exit(code: i32) -> !
```

[Declaration](sys/macos/arm64/process.wave#L38)

```wave
pub fun getpid() -> i64
```

[Declaration](sys/macos/arm64/process.wave#L42)

```wave
pub fun getppid() -> i64
```

[Declaration](sys/macos/arm64/process.wave#L46)

```wave
pub fun fork() -> i64
```

[Declaration](sys/macos/arm64/process.wave#L65)

```wave
pub fun execve(
    path: str,
    argv: ptr<ptr<i8>>,
    envp: ptr<ptr<i8>>
) -> i64
```

[Declaration](sys/macos/arm64/process.wave#L73)

```wave
pub fun waitpid(pid: i64, status: ptr<i32>, options: i32) -> i64
```

[Declaration](sys/macos/arm64/process.wave#L80)

```wave
pub fun kill(pid: i64, sig: i32) -> i64
```

## `std::sys::macos::arm64::socket`

[Source](sys/macos/arm64/socket.wave)

[Declaration](sys/macos/arm64/socket.wave#L37)

```wave
pub const AF_INET: i32 = 2;
```

[Declaration](sys/macos/arm64/socket.wave#L38)

```wave
pub const AF_INET6: i32 = 30;
```

[Declaration](sys/macos/arm64/socket.wave#L40)

```wave
pub const SOCK_STREAM: i32 = 1;
```

[Declaration](sys/macos/arm64/socket.wave#L41)

```wave
pub const SOCK_DGRAM: i32 = 2;
```

[Declaration](sys/macos/arm64/socket.wave#L43)

```wave
pub const IPPROTO_IP: i32 = 0;
```

[Declaration](sys/macos/arm64/socket.wave#L44)

```wave
pub const IPPROTO_IPV6: i32 = 41;
```

[Declaration](sys/macos/arm64/socket.wave#L45)

```wave
pub const IPPROTO_TCP: i32 = 6;
```

[Declaration](sys/macos/arm64/socket.wave#L46)

```wave
pub const IPPROTO_UDP: i32 = 17;
```

[Declaration](sys/macos/arm64/socket.wave#L48)

```wave
pub const SHUT_RD: i32 = 0;
```

[Declaration](sys/macos/arm64/socket.wave#L49)

```wave
pub const SHUT_WR: i32 = 1;
```

[Declaration](sys/macos/arm64/socket.wave#L50)

```wave
pub const SHUT_RDWR: i32 = 2;
```

[Declaration](sys/macos/arm64/socket.wave#L52)

```wave
pub const SOL_SOCKET: i32 = 0xFFFF;
```

[Declaration](sys/macos/arm64/socket.wave#L53)

```wave
pub const SO_REUSEADDR: i32 = 0x0004;
```

[Declaration](sys/macos/arm64/socket.wave#L54)

```wave
pub const SO_REUSEPORT: i32 = 0x0200;
```

[Declaration](sys/macos/arm64/socket.wave#L55)

```wave
pub const SO_ERROR: i32 = 0x1007;
```

[Declaration](sys/macos/arm64/socket.wave#L56)

```wave
pub const SO_RCVBUF: i32 = 0x1002;
```

[Declaration](sys/macos/arm64/socket.wave#L57)

```wave
pub const SO_SNDBUF: i32 = 0x1001;
```

[Declaration](sys/macos/arm64/socket.wave#L58)

```wave
pub const SO_BROADCAST: i32 = 0x0020;
```

[Declaration](sys/macos/arm64/socket.wave#L59)

```wave
pub const SO_KEEPALIVE: i32 = 0x0008;
```

[Declaration](sys/macos/arm64/socket.wave#L60)

```wave
pub const TCP_NODELAY: i32 = 1;
```

[Declaration](sys/macos/arm64/socket.wave#L61)

```wave
pub const IP_TTL: i32 = 4;
```

[Declaration](sys/macos/arm64/socket.wave#L62)

```wave
pub const IP_MULTICAST_IF: i32 = 9;
```

[Declaration](sys/macos/arm64/socket.wave#L63)

```wave
pub const IP_MULTICAST_TTL: i32 = 10;
```

[Declaration](sys/macos/arm64/socket.wave#L64)

```wave
pub const IP_MULTICAST_LOOP: i32 = 11;
```

[Declaration](sys/macos/arm64/socket.wave#L65)

```wave
pub const IP_ADD_MEMBERSHIP: i32 = 12;
```

[Declaration](sys/macos/arm64/socket.wave#L66)

```wave
pub const IP_DROP_MEMBERSHIP: i32 = 13;
```

[Declaration](sys/macos/arm64/socket.wave#L67)

```wave
pub const IPV6_MULTICAST_IF: i32 = 9;
```

[Declaration](sys/macos/arm64/socket.wave#L68)

```wave
pub const IPV6_MULTICAST_HOPS: i32 = 10;
```

[Declaration](sys/macos/arm64/socket.wave#L69)

```wave
pub const IPV6_MULTICAST_LOOP: i32 = 11;
```

[Declaration](sys/macos/arm64/socket.wave#L70)

```wave
pub const IPV6_JOIN_GROUP: i32 = 12;
```

[Declaration](sys/macos/arm64/socket.wave#L71)

```wave
pub const IPV6_LEAVE_GROUP: i32 = 13;
```

[Declaration](sys/macos/arm64/socket.wave#L72)

```wave
pub const SO_NOSIGPIPE: i32 = 0x1022;
```

[Declaration](sys/macos/arm64/socket.wave#L73)

```wave
pub const MSG_NOSIGNAL: i32 = 0;
```

[Declaration](sys/macos/arm64/socket.wave#L74)

```wave
pub const MSG_DONTWAIT: i32 = 128;
```

[Declaration](sys/macos/arm64/socket.wave#L76)

```wave
pub const POLLIN: i16 = 0x001;
```

[Declaration](sys/macos/arm64/socket.wave#L77)

```wave
pub const POLLOUT: i16 = 0x004;
```

[Declaration](sys/macos/arm64/socket.wave#L78)

```wave
pub const POLLERR: i16 = 0x008;
```

[Declaration](sys/macos/arm64/socket.wave#L79)

```wave
pub const POLLHUP: i16 = 0x010;
```

[Declaration](sys/macos/arm64/socket.wave#L80)

```wave
pub const POLLNVAL: i16 = 0x020;
```

[Declaration](sys/macos/arm64/socket.wave#L82)

```wave
pub struct PollFd {
    fd: i32;
    events: i16;
    revents: i16;
}
```

[Declaration](sys/macos/arm64/socket.wave#L88)

```wave
pub fun socket(domain: i32, ty: i32, protocol: i32) -> i64
```

[Declaration](sys/macos/arm64/socket.wave#L92)

```wave
pub fun bind(fd: i64, addr: ptr<i8>, len: i32) -> i64
```

[Declaration](sys/macos/arm64/socket.wave#L96)

```wave
pub fun listen(fd: i64, backlog: i32) -> i64
```

[Declaration](sys/macos/arm64/socket.wave#L100)

```wave
pub fun accept(fd: i64, addr: ptr<i8>, len: ptr<i32>) -> i64
```

[Declaration](sys/macos/arm64/socket.wave#L104)

```wave
pub fun connect(fd: i64, addr: ptr<i8>, len: i32) -> i64
```

[Declaration](sys/macos/arm64/socket.wave#L108)

```wave
pub fun getsockname(fd: i64, addr: ptr<i8>, len: ptr<i32>) -> i64
```

[Declaration](sys/macos/arm64/socket.wave#L112)

```wave
pub fun getpeername(fd: i64, addr: ptr<i8>, len: ptr<i32>) -> i64
```

[Declaration](sys/macos/arm64/socket.wave#L116)

```wave
pub fun shutdown(fd: i64, how: i32) -> i64
```

[Declaration](sys/macos/arm64/socket.wave#L120)

```wave
pub fun close_socket(fd: i64) -> i64
```

[Declaration](sys/macos/arm64/socket.wave#L122)

```wave
pub fun setsockopt(
    fd: i64,
    level: i32,
    optname: i32,
    optval: ptr<i8>,
    optlen: i32
) -> i64
```

[Declaration](sys/macos/arm64/socket.wave#L139)

```wave
pub fun getsockopt(
    fd: i64,
    level: i32,
    optname: i32,
    optval: ptr<i8>,
    optlen: ptr<i32>
) -> i64
```

[Declaration](sys/macos/arm64/socket.wave#L156)

```wave
pub fun poll(fds: ptr<PollFd>, nfds: i64, timeout_ms: i32) -> i64
```

[Declaration](sys/macos/arm64/socket.wave#L160)

```wave
pub fun send(fd: i64, buf: ptr<u8>, len: i64, flags: i32) -> i64
```

[Declaration](sys/macos/arm64/socket.wave#L164)

```wave
pub fun recv(fd: i64, buf: ptr<u8>, len: i64, flags: i32) -> i64
```

[Declaration](sys/macos/arm64/socket.wave#L168)

```wave
pub fun sendto(
    fd: i64,
    buf: ptr<u8>,
    len: i64,
    flags: i32,
    addr: ptr<i8>,
    addrlen: i32
) -> i64
```

[Declaration](sys/macos/arm64/socket.wave#L194)

```wave
pub fun recvfrom(
    fd: i64,
    buf: ptr<u8>,
    len: i64,
    flags: i32,
    addr: ptr<i8>,
    addrlen: ptr<i32>
) -> i64
```

## `std::sys::macos::arm64::syscall`

[Source](sys/macos/arm64/syscall.wave)

[Declaration](sys/macos/arm64/syscall.wave#L25)

```wave
pub fun syscall0(id: i64) -> i64
```

[Declaration](sys/macos/arm64/syscall.wave#L37)

```wave
pub fun syscall1(id: i64, a1: i64) -> i64
```

[Declaration](sys/macos/arm64/syscall.wave#L50)

```wave
pub fun syscall2(id: i64, a1: i64, a2: i64) -> i64
```

[Declaration](sys/macos/arm64/syscall.wave#L64)

```wave
pub fun syscall3(id: i64, a1: i64, a2: i64, a3: i64) -> i64
```

[Declaration](sys/macos/arm64/syscall.wave#L79)

```wave
pub fun syscall4(id: i64, a1: i64, a2: i64, a3: i64, a4: i64) -> i64
```

[Declaration](sys/macos/arm64/syscall.wave#L95)

```wave
pub fun syscall5(id: i64, a1: i64, a2: i64, a3: i64, a4: i64, a5: i64) -> i64
```

[Declaration](sys/macos/arm64/syscall.wave#L112)

```wave
pub fun syscall6(
    id: i64,
    a1: i64,
    a2: i64,
    a3: i64,
    a4: i64,
    a5: i64,
    a6: i64
) -> i64
```

## `std::sys::macos::arm64::time`

[Source](sys/macos/arm64/time.wave)

[Declaration](sys/macos/arm64/time.wave#L39)

```wave
pub struct TimeSpec {
    sec: i64;
    nsec: i64;
}
```

[Declaration](sys/macos/arm64/time.wave#L100)

```wave
pub fun nanosleep(req: ptr<TimeSpec>, rem: ptr<TimeSpec>) -> i64
```

[Declaration](sys/macos/arm64/time.wave#L169)

```wave
pub fun clock_gettime(clock_id: i32, tp: ptr<TimeSpec>) -> i64
```

## `std::sys::macos::arm64::tty`

[Source](sys/macos/arm64/tty.wave)

[Declaration](sys/macos/arm64/tty.wave#L33)

```wave
pub const TTY_SYS_IOCTL: i64 = 54;
```

[Declaration](sys/macos/arm64/tty.wave#L34)

```wave
pub const TTY_SYS_FCNTL: i64 = 92;
```

[Declaration](sys/macos/arm64/tty.wave#L36)

```wave
pub const TTY_TCGETS: i64 = 0x40487413;
```

[Declaration](sys/macos/arm64/tty.wave#L37)

```wave
pub const TTY_TCSETS: i64 = 0x80487414;
```

[Declaration](sys/macos/arm64/tty.wave#L38)

```wave
pub const TTY_TCSETSW: i64 = 0x80487415;
```

[Declaration](sys/macos/arm64/tty.wave#L39)

```wave
pub const TTY_TCSETSF: i64 = 0x80487416;
```

[Declaration](sys/macos/arm64/tty.wave#L41)

```wave
pub const TTY_TCSANOW: i32 = 0;
```

[Declaration](sys/macos/arm64/tty.wave#L42)

```wave
pub const TTY_TCSADRAIN: i32 = 1;
```

[Declaration](sys/macos/arm64/tty.wave#L43)

```wave
pub const TTY_TCSAFLUSH: i32 = 2;
```

[Declaration](sys/macos/arm64/tty.wave#L45)

```wave
pub const TTY_F_GETFL: i32 = 3;
```

[Declaration](sys/macos/arm64/tty.wave#L46)

```wave
pub const TTY_F_SETFL: i32 = 4;
```

[Declaration](sys/macos/arm64/tty.wave#L47)

```wave
pub const TTY_O_NONBLOCK: i32 = 4;
```

[Declaration](sys/macos/arm64/tty.wave#L49)

```wave
pub const TTY_ICANON: u64 = 0x00000100;
```

[Declaration](sys/macos/arm64/tty.wave#L50)

```wave
pub const TTY_ECHO: u64 = 0x00000008;
```

[Declaration](sys/macos/arm64/tty.wave#L52)

```wave
pub struct Termios {
    c_iflag: u64;
    c_oflag: u64;
    c_cflag: u64;
    c_lflag: u64;
    c_cc: array<u8, 20>;
    c_ispeed: u64;
    c_ospeed: u64;
}
```

[Declaration](sys/macos/arm64/tty.wave#L62)

```wave
pub struct TtyRawState {
    term: Termios;
    flags: i32;
    active: i32;
}
```

[Declaration](sys/macos/arm64/tty.wave#L68)

```wave
pub fun tty_getattr(fd: i32, t: ptr<Termios>) -> i64
```

[Declaration](sys/macos/arm64/tty.wave#L72)

```wave
pub fun tty_setattr(fd: i32, action: i32, t: ptr<Termios>) -> i64
```

[Declaration](sys/macos/arm64/tty.wave#L85)

```wave
pub fun tty_getfl(fd: i32) -> i64
```

[Declaration](sys/macos/arm64/tty.wave#L89)

```wave
pub fun tty_setfl(fd: i32, flags: i32) -> i64
```

[Declaration](sys/macos/arm64/tty.wave#L93)

```wave
pub fun tty_enable_raw_nonblock(fd: i32, st: ptr<TtyRawState>) -> i64
```

[Declaration](sys/macos/arm64/tty.wave#L130)

```wave
pub fun tty_restore(fd: i32, st: ptr<TtyRawState>)
```

## `std::sys::macos::common::env`

[Source](sys/macos/common/env.wave)

[Declaration](sys/macos/common/env.wave#L15)

```wave
pub const ERR_NOT_IMPLEMENTED: i64 = -38;
```

[Declaration](sys/macos/common/env.wave#L77)

```wave
pub fun env_read(buf: ptr<u8>, cap: i64) -> i64
```

## `std::sys::macos::env`

[Source](sys/macos/env.wave)

[Declaration](sys/macos/env.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::macos::amd64::env")::{ERR_NOT_IMPLEMENTED, env_read};
```

[Declaration](sys/macos/env.wave#L11)

```wave
#[target(arch="aarch64")]
pub import("std::sys::macos::arm64::env")::{ERR_NOT_IMPLEMENTED, env_read};
```

## `std::sys::macos::event`

[Source](sys/macos/event.wave)

[Declaration](sys/macos/event.wave#L3)

```wave
pub const EVENT_BACKEND_KIND: i32 = 2;
```

[Declaration](sys/macos/event.wave#L4)

```wave
pub const EVENT_READABLE: i32 = 1;
```

[Declaration](sys/macos/event.wave#L5)

```wave
pub const EVENT_WRITABLE: i32 = 2;
```

[Declaration](sys/macos/event.wave#L6)

```wave
pub const EVENT_ERROR: i32 = 4;
```

[Declaration](sys/macos/event.wave#L7)

```wave
pub const EVENT_HANGUP: i32 = 8;
```

[Declaration](sys/macos/event.wave#L15)

```wave
pub struct NativeEvent { fd: i64; token: u64; flags: i32; }
```

[Declaration](sys/macos/event.wave#L16)

```wave
pub struct NativeKevent {
    ident: u64; filter: i16; flags: u16; filter_flags: u32;
    data: i64; user_data: ptr<i8>;
}
```

[Declaration](sys/macos/event.wave#L20)

```wave
pub struct NativeTimeSpec { sec: i64; nsec: i64; }
```

[Declaration](sys/macos/event.wave#L62)

```wave
pub fun event_create(capacity: i32) -> i64
```

[Declaration](sys/macos/event.wave#L66)

```wave
pub fun event_add(handle: i64, fd: i64, token: u64, flags: i32) -> i64
```

[Declaration](sys/macos/event.wave#L69)

```wave
pub fun event_modify(handle: i64, fd: i64, token: u64, flags: i32) -> i64
```

[Declaration](sys/macos/event.wave#L75)

```wave
pub fun event_remove(handle: i64, fd: i64) -> i64
```

[Declaration](sys/macos/event.wave#L76)

```wave
pub fun event_wait(
    handle: i64, output: ptr<NativeEvent>, capacity: i32, timeout_ms: i32
) -> i64
```

[Declaration](sys/macos/event.wave#L108)

```wave
pub fun event_close(handle: i64) -> i64
```

## `std::sys::macos::fs`

[Source](sys/macos/fs.wave)

[Declaration](sys/macos/fs.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::macos::amd64::fs")::{
    FS_O_RDONLY, FS_O_WRONLY, FS_O_RDWR, FS_O_CREAT, FS_O_EXCL,
    FS_O_TRUNC, FS_O_APPEND, FS_O_NONBLOCK,
    FS_F_OK, FS_X_OK, FS_W_OK, FS_R_OK,
    FS_SEEK_SET, FS_SEEK_CUR, FS_SEEK_END, FS_F_GETFL, FS_F_SETFL,
    open, close, dup, dup2, pipe, fsync, fcntl, read, write, getcwd,
    chdir, access, lseek, unlink, mkdir, rmdir, rename_path, Stat, stat, fstat, ftruncate, same_file,
};
```

[Declaration](sys/macos/fs.wave#L18)

```wave
#[target(arch="aarch64")]
pub import("std::sys::macos::arm64::fs")::{
    FS_O_RDONLY, FS_O_WRONLY, FS_O_RDWR, FS_O_CREAT, FS_O_EXCL,
    FS_O_TRUNC, FS_O_APPEND, FS_O_NONBLOCK,
    FS_F_OK, FS_X_OK, FS_W_OK, FS_R_OK,
    FS_SEEK_SET, FS_SEEK_CUR, FS_SEEK_END, FS_F_GETFL, FS_F_SETFL,
    open, close, dup, dup2, pipe, fsync, fcntl, read, write, getcwd,
    chdir, access, lseek, unlink, mkdir, rmdir, rename_path, Stat, stat, fstat, ftruncate, same_file,
};
```

## `std::sys::macos::interfaces`

[Source](sys/macos/interfaces.wave)

[Declaration](sys/macos/interfaces.wave#L1)

```wave
pub struct NativeIfAddrs {
    next: ptr<NativeIfAddrs>;
    name: ptr<u8>;
    flags: u32;
    address: ptr<i8>;
    netmask: ptr<i8>;
    destination: ptr<i8>;
    data: ptr<i8>;
}
```

[Declaration](sys/macos/interfaces.wave#L11)

```wave
pub struct NativeSockAddrHeader { len: u8; family: u8; }
```

[Declaration](sys/macos/interfaces.wave#L18)

```wave
pub fun interfaces_open(output: ptr<ptr<NativeIfAddrs>>) -> i64
```

[Declaration](sys/macos/interfaces.wave#L25)

```wave
pub fun interfaces_close(first: ptr<NativeIfAddrs>)
```

[Declaration](sys/macos/interfaces.wave#L28)

```wave
pub fun interface_next(value: ptr<NativeIfAddrs>) -> ptr<NativeIfAddrs>
```

[Declaration](sys/macos/interfaces.wave#L29)

```wave
pub fun interface_name(value: ptr<NativeIfAddrs>) -> ptr<u8>
```

[Declaration](sys/macos/interfaces.wave#L30)

```wave
pub fun interface_flags(value: ptr<NativeIfAddrs>) -> u32
```

[Declaration](sys/macos/interfaces.wave#L31)

```wave
pub fun interface_address(value: ptr<NativeIfAddrs>) -> ptr<i8>
```

[Declaration](sys/macos/interfaces.wave#L32)

```wave
pub fun interface_family(value: ptr<NativeIfAddrs>) -> i32
```

[Declaration](sys/macos/interfaces.wave#L38)

```wave
pub fun interface_index(name: ptr<u8>) -> u32
```

## `std::sys::macos::local_addr`

[Source](sys/macos/local_addr.wave)

[Declaration](sys/macos/local_addr.wave#L1)

```wave
pub const AF_LOCAL: i32 = 1;
```

[Declaration](sys/macos/local_addr.wave#L2)

```wave
pub const NATIVE_LOCAL_PATH_CAPACITY: i32 = 104;
```

[Declaration](sys/macos/local_addr.wave#L4)

```wave
pub struct NativeLocalAddr {
    len: u8;
    family: u8;
    path: array<u8, 104>;
}
```

[Declaration](sys/macos/local_addr.wave#L10)

```wave
pub fun native_local_addr(path: str, output: ptr<NativeLocalAddr>) -> i32
```

[Declaration](sys/macos/local_addr.wave#L26)

```wave
pub fun native_local_abstract(name: str, output: ptr<NativeLocalAddr>) -> i32
```

## `std::sys::macos::memory`

[Source](sys/macos/memory.wave)

[Declaration](sys/macos/memory.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::macos::amd64::memory")::{
    PROT_READ, PROT_WRITE, MAP_PRIVATE, MAP_ANON, MAP_ANONYMOUS,
    mmap, munmap, brk, sys_alloc, sys_free, sys_page_size,
};
```

[Declaration](sys/macos/memory.wave#L14)

```wave
#[target(arch="aarch64")]
pub import("std::sys::macos::arm64::memory")::{
    PROT_READ, PROT_WRITE, MAP_PRIVATE, MAP_ANON, MAP_ANONYMOUS,
    mmap, munmap, brk, sys_alloc, sys_free, sys_page_size,
};
```

## `std::sys::macos::process`

[Source](sys/macos/process.wave)

[Declaration](sys/macos/process.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::macos::amd64::process")::{
    exit, getpid, getppid, fork, execve, waitpid, kill,
};
```

[Declaration](sys/macos/process.wave#L13)

```wave
#[target(arch="aarch64")]
pub import("std::sys::macos::arm64::process")::{
    exit, getpid, getppid, fork, execve, waitpid, kill,
};
```

## `std::sys::macos::resolver`

[Source](sys/macos/resolver.wave)

[Declaration](sys/macos/resolver.wave#L3)

```wave
pub struct NativeAddrInfo {
    flags: i32;
    family: i32;
    socket_type: i32;
    protocol: i32;
    address_length: u32;
    canonical_name: ptr<i8>;
    address: ptr<i8>;
    next: ptr<NativeAddrInfo>;
}
```

[Declaration](sys/macos/resolver.wave#L19)

```wave
pub fun resolver_open(
    host: str, service: str, socket_type: i32, protocol: i32,
    result: ptr<ptr<NativeAddrInfo>>
) -> i64
```

[Declaration](sys/macos/resolver.wave#L36)

```wave
pub fun resolver_close(result: ptr<NativeAddrInfo>)
```

[Declaration](sys/macos/resolver.wave#L40)

```wave
pub fun resolver_family(result: ptr<NativeAddrInfo>) -> i32
```

[Declaration](sys/macos/resolver.wave#L41)

```wave
pub fun resolver_address(result: ptr<NativeAddrInfo>) -> ptr<i8>
```

[Declaration](sys/macos/resolver.wave#L42)

```wave
pub fun resolver_address_length(result: ptr<NativeAddrInfo>) -> i64
```

[Declaration](sys/macos/resolver.wave#L45)

```wave
pub fun resolver_next(result: ptr<NativeAddrInfo>) -> ptr<NativeAddrInfo>
```

## `std::sys::macos::socket`

[Source](sys/macos/socket.wave)

[Declaration](sys/macos/socket.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::macos::amd64::socket")::{
    AF_INET, AF_INET6, SOCK_STREAM, SOCK_DGRAM,
    IPPROTO_IP, IPPROTO_IPV6, IPPROTO_TCP, IPPROTO_UDP,
    SHUT_RD, SHUT_WR, SHUT_RDWR, SOL_SOCKET, SO_REUSEADDR, SO_REUSEPORT,
    SO_ERROR, SO_RCVBUF, SO_SNDBUF, SO_BROADCAST, SO_KEEPALIVE,
    TCP_NODELAY, IP_TTL,
    IP_MULTICAST_IF, IP_MULTICAST_TTL, IP_MULTICAST_LOOP,
    IP_ADD_MEMBERSHIP, IP_DROP_MEMBERSHIP,
    IPV6_MULTICAST_IF, IPV6_MULTICAST_HOPS, IPV6_MULTICAST_LOOP,
    IPV6_JOIN_GROUP, IPV6_LEAVE_GROUP, SO_NOSIGPIPE, MSG_NOSIGNAL, MSG_DONTWAIT,
    POLLIN, POLLOUT, POLLERR, POLLHUP, POLLNVAL, PollFd,
    socket, bind, listen, accept, connect, getsockname, getpeername,
    shutdown, close_socket, setsockopt, getsockopt,
    poll, send, recv, sendto, recvfrom,
};
```

[Declaration](sys/macos/socket.wave#L25)

```wave
#[target(arch="aarch64")]
pub import("std::sys::macos::arm64::socket")::{
    AF_INET, AF_INET6, SOCK_STREAM, SOCK_DGRAM,
    IPPROTO_IP, IPPROTO_IPV6, IPPROTO_TCP, IPPROTO_UDP,
    SHUT_RD, SHUT_WR, SHUT_RDWR, SOL_SOCKET, SO_REUSEADDR, SO_REUSEPORT,
    SO_ERROR, SO_RCVBUF, SO_SNDBUF, SO_BROADCAST, SO_KEEPALIVE,
    TCP_NODELAY, IP_TTL,
    IP_MULTICAST_IF, IP_MULTICAST_TTL, IP_MULTICAST_LOOP,
    IP_ADD_MEMBERSHIP, IP_DROP_MEMBERSHIP,
    IPV6_MULTICAST_IF, IPV6_MULTICAST_HOPS, IPV6_MULTICAST_LOOP,
    IPV6_JOIN_GROUP, IPV6_LEAVE_GROUP, SO_NOSIGPIPE, MSG_NOSIGNAL, MSG_DONTWAIT,
    POLLIN, POLLOUT, POLLERR, POLLHUP, POLLNVAL, PollFd,
    socket, bind, listen, accept, connect, getsockname, getpeername,
    shutdown, close_socket, setsockopt, getsockopt,
    poll, send, recv, sendto, recvfrom,
};
```

## `std::sys::macos::socket_addr`

[Source](sys/macos/socket_addr.wave)

[Declaration](sys/macos/socket_addr.wave#L8)

```wave
pub const NATIVE_SOCKADDR_V4_LEN: i32 = 16;
```

[Declaration](sys/macos/socket_addr.wave#L9)

```wave
pub const NATIVE_SOCKADDR_V6_LEN: i32 = 28;
```

[Declaration](sys/macos/socket_addr.wave#L11)

```wave
pub struct NativeSockAddrV4 {
    len: u8;
    family: u8;
    port: u16;
    addr: u32;
    zero: array<u8, 8>;
}
```

[Declaration](sys/macos/socket_addr.wave#L19)

```wave
pub struct NativeSockAddrV6 {
    len: u8;
    family: u8;
    port: u16;
    flowinfo: u32;
    addr: array<u8, 16>;
    scope_id: u32;
}
```

[Declaration](sys/macos/socket_addr.wave#L28)

```wave
pub fun native_sockaddr_v4(ip_network: u32, port_network: u16) -> NativeSockAddrV4
```

[Declaration](sys/macos/socket_addr.wave#L38)

```wave
pub fun native_sockaddr_v4_ip(value: NativeSockAddrV4) -> u32
```

[Declaration](sys/macos/socket_addr.wave#L39)

```wave
pub fun native_sockaddr_v4_port(value: NativeSockAddrV4) -> u16
```

[Declaration](sys/macos/socket_addr.wave#L41)

```wave
pub fun native_sockaddr_v6(
    ip: ptr<u8>, port_network: u16, flowinfo: u32, scope_id: u32
) -> NativeSockAddrV6
```

[Declaration](sys/macos/socket_addr.wave#L55)

```wave
pub fun native_sockaddr_v6_copy_ip(value: NativeSockAddrV6, dst: ptr<u8>)
```

[Declaration](sys/macos/socket_addr.wave#L61)

```wave
pub fun native_sockaddr_v6_port(value: NativeSockAddrV6) -> u16
```

## `std::sys::macos::syscall`

[Source](sys/macos/syscall.wave)

[Declaration](sys/macos/syscall.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::macos::amd64::syscall")::{
    syscall0, syscall1, syscall2, syscall3, syscall4, syscall5, syscall6,
};
```

[Declaration](sys/macos/syscall.wave#L13)

```wave
#[target(arch="aarch64")]
pub import("std::sys::macos::arm64::syscall")::{
    syscall0, syscall1, syscall2, syscall3, syscall4, syscall5, syscall6,
};
```

## `std::sys::macos::time`

[Source](sys/macos/time.wave)

[Declaration](sys/macos/time.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::macos::amd64::time")::{TimeSpec, nanosleep, clock_gettime};
```

[Declaration](sys/macos/time.wave#L11)

```wave
#[target(arch="aarch64")]
pub import("std::sys::macos::arm64::time")::{TimeSpec, nanosleep, clock_gettime};
```

## `std::sys::macos::tty`

[Source](sys/macos/tty.wave)

[Declaration](sys/macos/tty.wave#L8)

```wave
#[target(arch="x86_64")]
pub import("std::sys::macos::amd64::tty")::{
    TTY_SYS_IOCTL, TTY_SYS_FCNTL, TTY_TCGETS, TTY_TCSETS, TTY_TCSETSW,
    TTY_TCSETSF, TTY_TCSANOW, TTY_TCSADRAIN, TTY_TCSAFLUSH,
    TTY_F_GETFL, TTY_F_SETFL, TTY_O_NONBLOCK, TTY_ICANON, TTY_ECHO,
    Termios, TtyRawState,
    tty_getattr, tty_setattr, tty_getfl, tty_setfl,
    tty_enable_raw_nonblock, tty_restore,
};
```

[Declaration](sys/macos/tty.wave#L18)

```wave
#[target(arch="aarch64")]
pub import("std::sys::macos::arm64::tty")::{
    TTY_SYS_IOCTL, TTY_SYS_FCNTL, TTY_TCGETS, TTY_TCSETS, TTY_TCSETSW,
    TTY_TCSETSF, TTY_TCSANOW, TTY_TCSADRAIN, TTY_TCSAFLUSH,
    TTY_F_GETFL, TTY_F_SETFL, TTY_O_NONBLOCK, TTY_ICANON, TTY_ECHO,
    Termios, TtyRawState,
    tty_getattr, tty_setattr, tty_getfl, tty_setfl,
    tty_enable_raw_nonblock, tty_restore,
};
```

## `std::sys::macos::vector_io`

[Source](sys/macos/vector_io.wave)

[Declaration](sys/macos/vector_io.wave#L1)

```wave
pub struct NativeIoSlice { data: ptr<u8>; length: u64; }
```

[Declaration](sys/macos/vector_io.wave#L2)

```wave
pub struct NativeMessage {
    name: ptr<i8>;
    name_length: u32;
    vectors: ptr<NativeIoSlice>;
    vector_count: i32;
    control: ptr<i8>;
    control_length: u32;
    flags: i32;
}
```

[Declaration](sys/macos/vector_io.wave#L16)

```wave
pub fun native_io_slice(data: ptr<u8>, length: i64, output: ptr<NativeIoSlice>) -> i64
```

[Declaration](sys/macos/vector_io.wave#L32)

```wave
pub fun vector_send(fd: i64, vectors: ptr<NativeIoSlice>, count: i32, flags: i32) -> i64
```

[Declaration](sys/macos/vector_io.wave#L36)

```wave
pub fun vector_recv(fd: i64, vectors: ptr<NativeIoSlice>, count: i32, flags: i32) -> i64
```

## `std::sys::memory`

[Source](sys/memory.wave)

[Declaration](sys/memory.wave#L23)

```wave
#[target(os="linux")]
pub import("std::sys::linux::memory")::{
    PROT_READ,
    PROT_WRITE,
    MAP_PRIVATE,
    MAP_ANONYMOUS,
    mmap,
    munmap,
    brk,
    sys_alloc,
    sys_free,
    sys_page_size,
};
```

[Declaration](sys/memory.wave#L37)

```wave
#[target(os="macos")]
pub import("std::sys::macos::memory")::{
    PROT_READ,
    PROT_WRITE,
    MAP_PRIVATE,
    MAP_ANON,
    MAP_ANONYMOUS,
    mmap,
    munmap,
    brk,
    sys_alloc,
    sys_free,
    sys_page_size,
};
```

[Declaration](sys/memory.wave#L52)

```wave
#[target(os="windows")]
pub import("std::sys::windows::memory")::{
    PROT_READ, PROT_WRITE, MAP_PRIVATE, MAP_ANONYMOUS,
    mmap, munmap, brk, sys_alloc, sys_free, sys_page_size,
};
```

[Declaration](sys/memory.wave#L58)

```wave
#[target(os="freebsd")]
pub import("std::sys::freebsd::memory")::{
    PROT_READ, PROT_WRITE, MAP_PRIVATE, MAP_ANONYMOUS,
    mmap, munmap, brk, sys_alloc, sys_free, sys_page_size,
};
```

[Declaration](sys/memory.wave#L64)

```wave
#[target(os="wasi")]
pub import("std::sys::wasm::memory")::{
    PROT_READ, PROT_WRITE, MAP_PRIVATE, MAP_ANONYMOUS,
    mmap, munmap, brk, sys_alloc, sys_free, sys_page_size,
};
```

[Declaration](sys/memory.wave#L70)

```wave
#[target(arch="wasm32", os="unknown")]
pub import("std::sys::wasm::memory")::{
    PROT_READ, PROT_WRITE, MAP_PRIVATE, MAP_ANONYMOUS,
    mmap, munmap, brk, sys_alloc, sys_free, sys_page_size,
};
```

[Declaration](sys/memory.wave#L76)

```wave
#[target(arch="wasm64", os="unknown")]
pub import("std::sys::wasm64::memory")::{
    PROT_READ, PROT_WRITE, MAP_PRIVATE, MAP_ANONYMOUS,
    mmap, munmap, brk, sys_alloc, sys_free, sys_page_size,
};
```

## `std::sys::process`

[Source](sys/process.wave)

[Declaration](sys/process.wave#L23)

```wave
#[target(os="linux")]
pub import("std::sys::linux::process")::{
    exit,
    getpid,
    getppid,
    fork,
    execve,
    waitpid,
    kill,
};
```

[Declaration](sys/process.wave#L34)

```wave
#[target(os="macos")]
pub import("std::sys::macos::process")::{
    exit,
    getpid,
    getppid,
    fork,
    execve,
    waitpid,
    kill,
};
```

[Declaration](sys/process.wave#L45)

```wave
#[target(os="windows")]
pub import("std::sys::windows::process")::{
    exit, getpid, getppid, fork, execve, waitpid, kill,
};
```

[Declaration](sys/process.wave#L50)

```wave
#[target(os="freebsd")]
pub import("std::sys::freebsd::process")::{
    exit, getpid, getppid, fork, execve, waitpid, kill,
};
```

[Declaration](sys/process.wave#L55)

```wave
#[target(os="wasi")]
pub import("std::sys::wasm::process")::{
    exit, getpid, getppid, fork, execve, waitpid, kill,
};
```

## `std::sys::random`

[Source](sys/random.wave)

[Declaration](sys/random.wave#L23)

```wave
#[target(os="linux")]
pub fun sys_random_available() -> bool
```

[Declaration](sys/random.wave#L27)

```wave
#[target(os="linux")]
pub fun sys_random_read(buffer: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/random.wave#L32)

```wave
#[target(os="macos")]
pub fun sys_random_available() -> bool
```

[Declaration](sys/random.wave#L36)

```wave
#[target(os="macos")]
pub fun sys_random_read(buffer: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/random.wave#L47)

```wave
#[target(os="windows")]
pub import("std::sys::windows::random")::{sys_random_available, sys_random_read};
```

[Declaration](sys/random.wave#L49)

```wave
#[target(os="freebsd", arch="x86_64")]
pub import("std::sys::freebsd::amd64::random")::{sys_random_available, sys_random_read};
```

[Declaration](sys/random.wave#L54)

```wave
#[target(os="freebsd", arch="aarch64")]
pub fun sys_random_available() -> bool
```

[Declaration](sys/random.wave#L58)

```wave
#[target(os="wasi")]
pub fun sys_random_available() -> bool
```

[Declaration](sys/random.wave#L62)

```wave
#[target(os="unknown")]
pub fun sys_random_available() -> bool
```

[Declaration](sys/random.wave#L66)

```wave
#[target(os="none")]
pub fun sys_random_available() -> bool
```

[Declaration](sys/random.wave#L71)

```wave
#[target(os="freebsd", arch="aarch64")]
pub fun sys_random_read(buffer: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/random.wave#L75)

```wave
#[target(os="wasi")]
pub fun sys_random_read(buffer: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/random.wave#L80)

```wave
#[target(os="unknown")]
pub fun sys_random_read(buffer: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/random.wave#L84)

```wave
#[target(os="none")]
pub fun sys_random_read(buffer: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/random.wave#L89)

```wave
#[target(os="freebsd", arch="riscv64")]
pub fun sys_random_available() -> bool
```

[Declaration](sys/random.wave#L91)

```wave
#[target(os="freebsd", arch="riscv64")]
pub fun sys_random_read(buffer: ptr<u8>, size: i64) -> i64
```

## `std::sys::resolver`

[Source](sys/resolver.wave)

[Declaration](sys/resolver.wave#L4)

```wave
#[target(os="linux")]
pub import("std::sys::linux::resolver")::{
    NativeAddrInfo, resolver_open, resolver_close, resolver_family,
    resolver_address, resolver_address_length, resolver_next,
};
```

[Declaration](sys/resolver.wave#L9)

```wave
#[target(os="macos")]
pub import("std::sys::macos::resolver")::{
    NativeAddrInfo, resolver_open, resolver_close, resolver_family,
    resolver_address, resolver_address_length, resolver_next,
};
```

[Declaration](sys/resolver.wave#L14)

```wave
#[target(os="windows")]
pub import("std::sys::windows::resolver")::{
    NativeAddrInfo, resolver_open, resolver_close, resolver_family,
    resolver_address, resolver_address_length, resolver_next,
};
```

[Declaration](sys/resolver.wave#L19)

```wave
#[target(os="freebsd")]
pub import("std::sys::freebsd::resolver")::{
    NativeAddrInfo, resolver_open, resolver_close, resolver_family,
    resolver_address, resolver_address_length, resolver_next,
};
```

## `std::sys::socket`

[Source](sys/socket.wave)

[Declaration](sys/socket.wave#L23)

```wave
#[target(os="linux")]
pub import("std::sys::linux::socket")::{
    AF_INET,
    AF_INET6,
    SOCK_STREAM,
    SOCK_DGRAM,
    IPPROTO_IP,
    IPPROTO_IPV6,
    IPPROTO_TCP,
    IPPROTO_UDP,
    SHUT_RD,
    SHUT_WR,
    SHUT_RDWR,
    SOL_SOCKET,
    SO_REUSEADDR,
    SO_REUSEPORT,
    SO_ERROR,
    SO_RCVBUF,
    SO_SNDBUF,
    SO_BROADCAST,
    SO_KEEPALIVE,
    TCP_NODELAY,
    IP_TTL,
    IP_MULTICAST_IF,
    IP_MULTICAST_TTL,
    IP_MULTICAST_LOOP,
    IP_ADD_MEMBERSHIP,
    IP_DROP_MEMBERSHIP,
    IPV6_MULTICAST_IF,
    IPV6_MULTICAST_HOPS,
    IPV6_MULTICAST_LOOP,
    IPV6_JOIN_GROUP,
    IPV6_LEAVE_GROUP,
    MSG_NOSIGNAL, MSG_DONTWAIT,
    POLLIN,
    POLLOUT,
    POLLERR,
    POLLHUP,
    POLLNVAL,
    PollFd,
    socket,
    bind,
    listen,
    accept,
    connect,
    getsockname,
    getpeername,
    shutdown,
    close_socket,
    setsockopt,
    getsockopt,
    poll,
    send,
    recv,
    sendto,
    recvfrom,
};
```

[Declaration](sys/socket.wave#L81)

```wave
#[target(os="macos")]
pub import("std::sys::macos::socket")::{
    AF_INET,
    AF_INET6,
    SOCK_STREAM,
    SOCK_DGRAM,
    IPPROTO_IP,
    IPPROTO_IPV6,
    IPPROTO_TCP,
    IPPROTO_UDP,
    SHUT_RD,
    SHUT_WR,
    SHUT_RDWR,
    SOL_SOCKET,
    SO_REUSEADDR,
    SO_REUSEPORT,
    SO_ERROR,
    SO_RCVBUF,
    SO_SNDBUF,
    SO_BROADCAST,
    SO_KEEPALIVE,
    TCP_NODELAY,
    IP_TTL,
    IP_MULTICAST_IF,
    IP_MULTICAST_TTL,
    IP_MULTICAST_LOOP,
    IP_ADD_MEMBERSHIP,
    IP_DROP_MEMBERSHIP,
    IPV6_MULTICAST_IF,
    IPV6_MULTICAST_HOPS,
    IPV6_MULTICAST_LOOP,
    IPV6_JOIN_GROUP,
    IPV6_LEAVE_GROUP,
    SO_NOSIGPIPE,
    MSG_NOSIGNAL, MSG_DONTWAIT,
    POLLIN,
    POLLOUT,
    POLLERR,
    POLLHUP,
    POLLNVAL,
    PollFd,
    socket,
    bind,
    listen,
    accept,
    connect,
    getsockname,
    getpeername,
    shutdown,
    close_socket,
    setsockopt,
    getsockopt,
    poll,
    send,
    recv,
    sendto,
    recvfrom,
};
```

[Declaration](sys/socket.wave#L140)

```wave
#[target(os="windows")]
pub import("std::sys::windows::socket")::{
    AF_INET, AF_INET6, SOCK_STREAM, SOCK_DGRAM,
    IPPROTO_IP, IPPROTO_IPV6, IPPROTO_TCP, IPPROTO_UDP,
    SHUT_RD, SHUT_WR, SHUT_RDWR, SOL_SOCKET, SO_REUSEADDR, SO_REUSEPORT,
    SO_ERROR, SO_RCVBUF, SO_SNDBUF, SO_BROADCAST, SO_KEEPALIVE,
    TCP_NODELAY, IP_TTL,
    IP_MULTICAST_IF, IP_MULTICAST_TTL, IP_MULTICAST_LOOP,
    IP_ADD_MEMBERSHIP, IP_DROP_MEMBERSHIP,
    IPV6_MULTICAST_IF, IPV6_MULTICAST_HOPS, IPV6_MULTICAST_LOOP,
    IPV6_JOIN_GROUP, IPV6_LEAVE_GROUP, MSG_NOSIGNAL,
    POLLIN, POLLOUT, POLLERR, POLLHUP, POLLNVAL, PollFd,
    socket, bind, listen, accept, connect, getsockname, getpeername,
    shutdown, close_socket, setsockopt, getsockopt,
    poll, send, recv, sendto, recvfrom,
};
```

[Declaration](sys/socket.wave#L157)

```wave
#[target(os="freebsd")]
pub import("std::sys::freebsd::socket")::{
    AF_INET, AF_INET6, SOCK_STREAM, SOCK_DGRAM,
    IPPROTO_IP, IPPROTO_IPV6, IPPROTO_TCP, IPPROTO_UDP,
    SHUT_RD, SHUT_WR, SHUT_RDWR, SOL_SOCKET, SO_REUSEADDR, SO_REUSEPORT,
    SO_ERROR, SO_RCVBUF, SO_SNDBUF, SO_BROADCAST, SO_KEEPALIVE,
    TCP_NODELAY, IP_TTL,
    IP_MULTICAST_IF, IP_MULTICAST_TTL, IP_MULTICAST_LOOP,
    IP_ADD_MEMBERSHIP, IP_DROP_MEMBERSHIP,
    IPV6_MULTICAST_IF, IPV6_MULTICAST_HOPS, IPV6_MULTICAST_LOOP,
    IPV6_JOIN_GROUP, IPV6_LEAVE_GROUP, MSG_NOSIGNAL, MSG_DONTWAIT,
    POLLIN, POLLOUT, POLLERR, POLLHUP, POLLNVAL, PollFd,
    socket, bind, listen, accept, connect, getsockname, getpeername,
    shutdown, close_socket, setsockopt, getsockopt,
    poll, send, recv, sendto, recvfrom,
};
```

## `std::sys::socket_addr`

[Source](sys/socket_addr.wave)

[Declaration](sys/socket_addr.wave#L8)

```wave
#[target(os="linux")]
pub import("std::sys::linux::socket_addr")::{
    NATIVE_SOCKADDR_V4_LEN, NATIVE_SOCKADDR_V6_LEN,
    NativeSockAddrV4, NativeSockAddrV6,
    native_sockaddr_v4, native_sockaddr_v4_ip, native_sockaddr_v4_port,
    native_sockaddr_v6, native_sockaddr_v6_copy_ip, native_sockaddr_v6_port,
};
```

[Declaration](sys/socket_addr.wave#L16)

```wave
#[target(os="macos")]
pub import("std::sys::macos::socket_addr")::{
    NATIVE_SOCKADDR_V4_LEN, NATIVE_SOCKADDR_V6_LEN,
    NativeSockAddrV4, NativeSockAddrV6,
    native_sockaddr_v4, native_sockaddr_v4_ip, native_sockaddr_v4_port,
    native_sockaddr_v6, native_sockaddr_v6_copy_ip, native_sockaddr_v6_port,
};
```

[Declaration](sys/socket_addr.wave#L24)

```wave
#[target(os="windows")]
pub import("std::sys::windows::socket_addr")::{
    NATIVE_SOCKADDR_V4_LEN, NATIVE_SOCKADDR_V6_LEN,
    NativeSockAddrV4, NativeSockAddrV6,
    native_sockaddr_v4, native_sockaddr_v4_ip, native_sockaddr_v4_port,
    native_sockaddr_v6, native_sockaddr_v6_copy_ip, native_sockaddr_v6_port,
};
```

[Declaration](sys/socket_addr.wave#L32)

```wave
#[target(os="freebsd")]
pub import("std::sys::freebsd::socket_addr")::{
    NATIVE_SOCKADDR_V4_LEN, NATIVE_SOCKADDR_V6_LEN,
    NativeSockAddrV4, NativeSockAddrV6,
    native_sockaddr_v4, native_sockaddr_v4_ip, native_sockaddr_v4_port,
    native_sockaddr_v6, native_sockaddr_v6_copy_ip, native_sockaddr_v6_port,
};
```

## `std::sys::time`

[Source](sys/time.wave)

[Declaration](sys/time.wave#L23)

```wave
#[target(os="linux")]
pub import("std::sys::linux::time")::{
    TimeSpec,
    nanosleep,
    clock_gettime,
};
```

[Declaration](sys/time.wave#L30)

```wave
#[target(os="macos")]
pub import("std::sys::macos::time")::{
    TimeSpec,
    nanosleep,
    clock_gettime,
};
```

[Declaration](sys/time.wave#L37)

```wave
#[target(os="windows")]
pub import("std::sys::windows::time")::{TimeSpec, nanosleep, clock_gettime};
```

[Declaration](sys/time.wave#L40)

```wave
#[target(os="freebsd")]
pub import("std::sys::freebsd::time")::{TimeSpec, nanosleep, clock_gettime};
```

[Declaration](sys/time.wave#L43)

```wave
#[target(os="wasi")]
pub import("std::sys::wasm::time")::{TimeSpec, nanosleep, clock_gettime};
```

## `std::sys::tty`

[Source](sys/tty.wave)

[Declaration](sys/tty.wave#L23)

```wave
#[target(os="linux")]
pub import("std::sys::linux::tty")::{
    TTY_SYS_IOCTL,
    TTY_SYS_FCNTL,
    TTY_TCGETS,
    TTY_TCSETS,
    TTY_TCSETSW,
    TTY_TCSETSF,
    TTY_TCSANOW,
    TTY_TCSADRAIN,
    TTY_TCSAFLUSH,
    TTY_F_GETFL,
    TTY_F_SETFL,
    TTY_O_NONBLOCK,
    TTY_ICANON,
    TTY_ECHO,
    TTY_VTIME_IDX,
    TTY_VMIN_IDX,
    Termios,
    TtyRawState,
    tty_getattr,
    tty_setattr,
    tty_getfl,
    tty_setfl,
    tty_enable_raw_nonblock,
    tty_restore,
};
```

[Declaration](sys/tty.wave#L51)

```wave
#[target(os="macos")]
pub import("std::sys::macos::tty")::{
    TTY_SYS_IOCTL,
    TTY_SYS_FCNTL,
    TTY_TCGETS,
    TTY_TCSETS,
    TTY_TCSETSW,
    TTY_TCSETSF,
    TTY_TCSANOW,
    TTY_TCSADRAIN,
    TTY_TCSAFLUSH,
    TTY_F_GETFL,
    TTY_F_SETFL,
    TTY_O_NONBLOCK,
    TTY_ICANON,
    TTY_ECHO,
    Termios,
    TtyRawState,
    tty_getattr,
    tty_setattr,
    tty_getfl,
    tty_setfl,
    tty_enable_raw_nonblock,
    tty_restore,
};
```

[Declaration](sys/tty.wave#L77)

```wave
#[target(os="windows")]
pub import("std::sys::windows::tty")::{
    TTY_SYS_IOCTL, TTY_SYS_FCNTL, TTY_TCGETS, TTY_TCSETS, TTY_TCSETSW,
    TTY_TCSETSF, TTY_TCSANOW, TTY_TCSADRAIN, TTY_TCSAFLUSH,
    TTY_F_GETFL, TTY_F_SETFL, TTY_O_NONBLOCK, TTY_ICANON, TTY_ECHO,
    TTY_VTIME_IDX, TTY_VMIN_IDX, Termios, TtyRawState,
    tty_getattr, tty_setattr, tty_getfl, tty_setfl,
    tty_enable_raw_nonblock, tty_restore,
};
```

[Declaration](sys/tty.wave#L87)

```wave
#[target(os="freebsd")]
pub import("std::sys::freebsd::tty")::{
    TTY_SYS_IOCTL, TTY_SYS_FCNTL, TTY_TCGETS, TTY_TCSETS, TTY_TCSETSW,
    TTY_TCSETSF, TTY_TCSANOW, TTY_TCSADRAIN, TTY_TCSAFLUSH,
    TTY_F_GETFL, TTY_F_SETFL, TTY_O_NONBLOCK, TTY_ICANON, TTY_ECHO,
    TTY_VTIME_IDX, TTY_VMIN_IDX, Termios, TtyRawState,
    tty_getattr, tty_setattr, tty_getfl, tty_setfl,
    tty_enable_raw_nonblock, tty_restore,
};
```

## `std::sys::vector_io`

[Source](sys/vector_io.wave)

[Declaration](sys/vector_io.wave#L2)

```wave
#[target(os="linux")]
pub import("std::sys::linux::vector_io")::{NativeIoSlice, native_io_slice, vector_send, vector_recv};
```

[Declaration](sys/vector_io.wave#L4)

```wave
#[target(os="macos")]
pub import("std::sys::macos::vector_io")::{NativeIoSlice, native_io_slice, vector_send, vector_recv};
```

[Declaration](sys/vector_io.wave#L6)

```wave
#[target(os="freebsd")]
pub import("std::sys::freebsd::vector_io")::{NativeIoSlice, native_io_slice, vector_send, vector_recv};
```

[Declaration](sys/vector_io.wave#L8)

```wave
#[target(os="windows")]
pub import("std::sys::windows::vector_io")::{NativeIoSlice, native_io_slice, vector_send, vector_recv};
```

## `std::sys::wasm::env`

[Source](sys/wasm/env.wave)

[Declaration](sys/wasm/env.wave#L10)

```wave
pub const ENV_READ_NEEDS_SPACE: i64 = -4096;
```

[Declaration](sys/wasm/env.wave#L22)

```wave
pub fun env_read(buf: ptr<u8>, cap: i64) -> i64
```

## `std::sys::wasm::fs`

[Source](sys/wasm/fs.wave)

[Declaration](sys/wasm/fs.wave#L43)

```wave
pub const FS_O_RDONLY: i32 = 0;
```

[Declaration](sys/wasm/fs.wave#L44)

```wave
pub const FS_O_WRONLY: i32 = 1;
```

[Declaration](sys/wasm/fs.wave#L45)

```wave
pub const FS_O_RDWR: i32 = 2;
```

[Declaration](sys/wasm/fs.wave#L46)

```wave
pub const FS_O_CREAT: i32 = 64;
```

[Declaration](sys/wasm/fs.wave#L47)

```wave
pub const FS_O_EXCL: i32 = 128;
```

[Declaration](sys/wasm/fs.wave#L48)

```wave
pub const FS_O_TRUNC: i32 = 512;
```

[Declaration](sys/wasm/fs.wave#L49)

```wave
pub const FS_O_APPEND: i32 = 1024;
```

[Declaration](sys/wasm/fs.wave#L50)

```wave
pub const FS_O_NONBLOCK: i32 = 2048;
```

[Declaration](sys/wasm/fs.wave#L51)

```wave
pub const FS_F_OK: i32 = 0;
```

[Declaration](sys/wasm/fs.wave#L52)

```wave
pub const FS_X_OK: i32 = 1;
```

[Declaration](sys/wasm/fs.wave#L53)

```wave
pub const FS_W_OK: i32 = 2;
```

[Declaration](sys/wasm/fs.wave#L54)

```wave
pub const FS_R_OK: i32 = 4;
```

[Declaration](sys/wasm/fs.wave#L55)

```wave
pub const FS_SEEK_SET: i32 = 0;
```

[Declaration](sys/wasm/fs.wave#L56)

```wave
pub const FS_SEEK_CUR: i32 = 1;
```

[Declaration](sys/wasm/fs.wave#L57)

```wave
pub const FS_SEEK_END: i32 = 2;
```

[Declaration](sys/wasm/fs.wave#L58)

```wave
pub const FS_F_GETFL: i32 = 3;
```

[Declaration](sys/wasm/fs.wave#L59)

```wave
pub const FS_F_SETFL: i32 = 4;
```

[Declaration](sys/wasm/fs.wave#L91)

```wave
pub struct Stat {
    mode: i16;
    size: i64;
    mtime: i64;
}
```

[Declaration](sys/wasm/fs.wave#L124)

```wave
pub fun open(path: str, flags: i32, mode: i32) -> i64
```

[Declaration](sys/wasm/fs.wave#L151)

```wave
pub fun close(fd: i64) -> i64
```

[Declaration](sys/wasm/fs.wave#L158)

```wave
pub fun read(fd: i64, buf: ptr<u8>, len: i64) -> i64
```

[Declaration](sys/wasm/fs.wave#L159)

```wave
pub fun write(fd: i64, buf: ptr<u8>, len: i64) -> i64
```

[Declaration](sys/wasm/fs.wave#L161)

```wave
pub fun fsync(fd: i64) -> i64
```

[Declaration](sys/wasm/fs.wave#L168)

```wave
pub fun lseek(fd: i64, offset: i64, whence: i32) -> i64
```

[Declaration](sys/wasm/fs.wave#L177)

```wave
pub fun access(path: str, mode: i32) -> i64
```

[Declaration](sys/wasm/fs.wave#L204)

```wave
pub fun stat(path: str, result: ptr<Stat>) -> i64
```

[Declaration](sys/wasm/fs.wave#L216)

```wave
pub fun fstat(fd: i64, result: ptr<Stat>) -> i64
```

[Declaration](sys/wasm/fs.wave#L224)

```wave
pub fun mkdir(path: str, mode: i32) -> i64
```

[Declaration](sys/wasm/fs.wave#L234)

```wave
pub fun unlink(path: str) -> i64
```

[Declaration](sys/wasm/fs.wave#L242)

```wave
pub fun rmdir(path: str) -> i64
```

[Declaration](sys/wasm/fs.wave#L250)

```wave
pub fun rename_path(old_path: str, new_path: str) -> i64
```

[Declaration](sys/wasm/fs.wave#L263)

```wave
pub fun getcwd(buf: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/wasm/fs.wave#L270)

```wave
pub fun chdir(path: str) -> i64
```

[Declaration](sys/wasm/fs.wave#L271)

```wave
pub fun dup(fd: i64) -> i64
```

[Declaration](sys/wasm/fs.wave#L272)

```wave
pub fun dup2(oldfd: i64, newfd: i64) -> i64
```

[Declaration](sys/wasm/fs.wave#L273)

```wave
pub fun pipe(fds: ptr<i32>) -> i64
```

[Declaration](sys/wasm/fs.wave#L274)

```wave
pub fun fcntl(fd: i64, cmd: i32, arg: i64) -> i64
```

[Declaration](sys/wasm/fs.wave#L278)

```wave
pub fun ftruncate(fd: i64, length: i64) -> i64
```

[Declaration](sys/wasm/fs.wave#L286)

```wave
pub fun same_file(first_fd: i64, second_fd: i64) -> i64
```

## `std::sys::wasm::io`

[Source](sys/wasm/io.wave)

[Declaration](sys/wasm/io.wave#L9)

```wave
pub struct WasiIoVec {
    data: ptr<u8>;
    len: u32;
}
```

[Declaration](sys/wasm/io.wave#L21)

```wave
pub fun wasi_read(fd: i64, data: ptr<u8>, len: i64) -> i64
```

[Declaration](sys/wasm/io.wave#L31)

```wave
pub fun wasi_write(fd: i64, data: ptr<u8>, len: i64) -> i64
```

## `std::sys::wasm::memory`

[Source](sys/wasm/memory.wave)

[Declaration](sys/wasm/memory.wave#L12)

```wave
pub const PROT_READ: i32 = 1;
```

[Declaration](sys/wasm/memory.wave#L13)

```wave
pub const PROT_WRITE: i32 = 2;
```

[Declaration](sys/wasm/memory.wave#L14)

```wave
pub const MAP_PRIVATE: i32 = 2;
```

[Declaration](sys/wasm/memory.wave#L15)

```wave
pub const MAP_ANONYMOUS: i32 = 32;
```

[Declaration](sys/wasm/memory.wave#L50)

```wave
pub fun sys_alloc(size: i64) -> ptr<u8>
```

[Declaration](sys/wasm/memory.wave#L92)

```wave
pub fun sys_free(p: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/wasm/memory.wave#L96)

```wave
pub fun mmap(
    addr: ptr<i8>, length: i64, prot: i32, flags: i32, fd: i64, offset: i64
) -> ptr<i8>
```

[Declaration](sys/wasm/memory.wave#L106)

```wave
pub fun munmap(addr: ptr<i8>, length: i64) -> i64
```

[Declaration](sys/wasm/memory.wave#L110)

```wave
pub fun brk(addr: ptr<i8>) -> ptr<i8>
```

[Declaration](sys/wasm/memory.wave#L115)

```wave
pub fun sys_page_size() -> i64
```

## `std::sys::wasm::process`

[Source](sys/wasm/process.wave)

[Declaration](sys/wasm/process.wave#L13)

```wave
pub fun exit(code: i32) -> !
```

[Declaration](sys/wasm/process.wave#L18)

```wave
pub fun getpid() -> i64
```

[Declaration](sys/wasm/process.wave#L19)

```wave
pub fun getppid() -> i64
```

[Declaration](sys/wasm/process.wave#L20)

```wave
pub fun fork() -> i64
```

[Declaration](sys/wasm/process.wave#L21)

```wave
pub fun execve(path: str, argv: ptr<ptr<i8>>, envp: ptr<ptr<i8>>) -> i64
```

[Declaration](sys/wasm/process.wave#L24)

```wave
pub fun waitpid(pid: i64, status: ptr<i32>, options: i32) -> i64
```

[Declaration](sys/wasm/process.wave#L27)

```wave
pub fun kill(pid: i64, sig: i32) -> i64
```

## `std::sys::wasm::time`

[Source](sys/wasm/time.wave)

[Declaration](sys/wasm/time.wave#L43)

```wave
pub struct TimeSpec {
    sec: i64;
    nsec: i64;
}
```

[Declaration](sys/wasm/time.wave#L48)

```wave
pub fun clock_gettime(clock_id: i32, tp: ptr<TimeSpec>) -> i64
```

[Declaration](sys/wasm/time.wave#L61)

```wave
pub fun nanosleep(req: ptr<TimeSpec>, rem: ptr<TimeSpec>) -> i64
```

## `std::sys::wasm64::memory`

[Source](sys/wasm64/memory.wave)

[Declaration](sys/wasm64/memory.wave#L12)

```wave
pub const PROT_READ: i32 = 1;
```

[Declaration](sys/wasm64/memory.wave#L13)

```wave
pub const PROT_WRITE: i32 = 2;
```

[Declaration](sys/wasm64/memory.wave#L14)

```wave
pub const MAP_PRIVATE: i32 = 2;
```

[Declaration](sys/wasm64/memory.wave#L15)

```wave
pub const MAP_ANONYMOUS: i32 = 32;
```

[Declaration](sys/wasm64/memory.wave#L50)

```wave
pub fun sys_alloc(size: i64) -> ptr<u8>
```

[Declaration](sys/wasm64/memory.wave#L92)

```wave
pub fun sys_free(p: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/wasm64/memory.wave#L96)

```wave
pub fun mmap(
    addr: ptr<i8>, length: i64, prot: i32, flags: i32, fd: i64, offset: i64
) -> ptr<i8>
```

[Declaration](sys/wasm64/memory.wave#L105)

```wave
pub fun munmap(addr: ptr<i8>, length: i64) -> i64
```

[Declaration](sys/wasm64/memory.wave#L109)

```wave
pub fun brk(addr: ptr<i8>) -> ptr<i8>
```

[Declaration](sys/wasm64/memory.wave#L114)

```wave
pub fun sys_page_size() -> i64
```

## `std::sys::wasm_blocks`

[Source](sys/wasm_blocks.wave)

[Declaration](sys/wasm_blocks.wave#L12)

```wave
pub struct MemoryBlock {
    capacity: u64;
    requested: i64;
    next: ptr<MemoryBlock>;
    free_next: ptr<MemoryBlock>;
}
```

[Declaration](sys/wasm_blocks.wave#L51)

```wave
pub fun blocks_reuse(
    free_head: ptr<ptr<MemoryBlock>>, tail: ptr<ptr<MemoryBlock>>, size: i64, capacity: u64
) -> ptr<u8>
```

[Declaration](sys/wasm_blocks.wave#L83)

```wave
pub fun blocks_append(
    head: ptr<ptr<MemoryBlock>>, tail: ptr<ptr<MemoryBlock>>, storage: ptr<u8>, size: i64, capacity: u64
) -> ptr<u8>
```

[Declaration](sys/wasm_blocks.wave#L97)

```wave
pub fun blocks_free(
    head: ptr<MemoryBlock>, tail: ptr<ptr<MemoryBlock>>, free_head: ptr<ptr<MemoryBlock>>,
    address: ptr<u8>, size: i64
) -> i64
```

## `std::sys::windows::completion_port`

[Source](sys/windows/completion_port.wave)

[Declaration](sys/windows/completion_port.wave#L25)

```wave
pub fun completion_port_handle() -> ptr<u8>
```

[Declaration](sys/windows/completion_port.wave#L29)

```wave
pub fun completion_port_acquire() -> i64
```

[Declaration](sys/windows/completion_port.wave#L49)

```wave
pub fun completion_port_associate(socket: i64) -> i64
```

[Declaration](sys/windows/completion_port.wave#L79)

```wave
pub fun completion_port_socket_closed(socket: i64)
```

[Declaration](sys/windows/completion_port.wave#L99)

```wave
pub fun completion_port_sockets_closed()
```

[Declaration](sys/windows/completion_port.wave#L111)

```wave
pub fun completion_port_release_executor()
```

## `std::sys::windows::env`

[Source](sys/windows/env.wave)

[Declaration](sys/windows/env.wave#L23)

```wave
pub fun env_read(buf: ptr<u8>, cap: i64) -> i64
```

## `std::sys::windows::event`

[Source](sys/windows/event.wave)

[Declaration](sys/windows/event.wave#L11)

```wave
pub const EVENT_BACKEND_KIND: i32 = 3;
```

[Declaration](sys/windows/event.wave#L12)

```wave
pub const EVENT_READABLE: i32 = 1;
```

[Declaration](sys/windows/event.wave#L13)

```wave
pub const EVENT_WRITABLE: i32 = 2;
```

[Declaration](sys/windows/event.wave#L14)

```wave
pub const EVENT_ERROR: i32 = 4;
```

[Declaration](sys/windows/event.wave#L15)

```wave
pub const EVENT_HANGUP: i32 = 8;
```

[Declaration](sys/windows/event.wave#L19)

```wave
pub struct NativeEvent { fd: i64; token: u64; flags: i32; }
```

[Declaration](sys/windows/event.wave#L45)

```wave
pub fun event_create(capacity: i32) -> i64
```

[Declaration](sys/windows/event.wave#L55)

```wave
pub fun event_add(handle: i64, fd: i64, token: u64, flags: i32) -> i64
```

[Declaration](sys/windows/event.wave#L65)

```wave
pub fun event_modify(handle: i64, fd: i64, token: u64, flags: i32) -> i64
```

[Declaration](sys/windows/event.wave#L73)

```wave
pub fun event_remove(handle: i64, fd: i64) -> i64
```

[Declaration](sys/windows/event.wave#L89)

```wave
pub fun event_wait(
    handle: i64, output: ptr<NativeEvent>, capacity: i32, timeout_ms: i32
) -> i64
```

[Declaration](sys/windows/event.wave#L140)

```wave
pub fun event_close(handle: i64) -> i64
```

## `std::sys::windows::fs`

[Source](sys/windows/fs.wave)

[Declaration](sys/windows/fs.wave#L50)

```wave
pub const FS_O_RDONLY: i32 = 0;
```

[Declaration](sys/windows/fs.wave#L51)

```wave
pub const FS_O_WRONLY: i32 = 1;
```

[Declaration](sys/windows/fs.wave#L52)

```wave
pub const FS_O_RDWR: i32 = 2;
```

[Declaration](sys/windows/fs.wave#L53)

```wave
pub const FS_O_CREAT: i32 = 64;
```

[Declaration](sys/windows/fs.wave#L54)

```wave
pub const FS_O_EXCL: i32 = 128;
```

[Declaration](sys/windows/fs.wave#L55)

```wave
pub const FS_O_TRUNC: i32 = 512;
```

[Declaration](sys/windows/fs.wave#L56)

```wave
pub const FS_O_APPEND: i32 = 1024;
```

[Declaration](sys/windows/fs.wave#L57)

```wave
pub const FS_O_NONBLOCK: i32 = 2048;
```

[Declaration](sys/windows/fs.wave#L58)

```wave
pub const FS_F_OK: i32 = 0;
```

[Declaration](sys/windows/fs.wave#L59)

```wave
pub const FS_X_OK: i32 = 1;
```

[Declaration](sys/windows/fs.wave#L60)

```wave
pub const FS_W_OK: i32 = 2;
```

[Declaration](sys/windows/fs.wave#L61)

```wave
pub const FS_R_OK: i32 = 4;
```

[Declaration](sys/windows/fs.wave#L62)

```wave
pub const FS_SEEK_SET: i32 = 0;
```

[Declaration](sys/windows/fs.wave#L63)

```wave
pub const FS_SEEK_CUR: i32 = 1;
```

[Declaration](sys/windows/fs.wave#L64)

```wave
pub const FS_SEEK_END: i32 = 2;
```

[Declaration](sys/windows/fs.wave#L65)

```wave
pub const FS_F_GETFL: i32 = 3;
```

[Declaration](sys/windows/fs.wave#L66)

```wave
pub const FS_F_SETFL: i32 = 4;
```

[Declaration](sys/windows/fs.wave#L89)

```wave
pub struct Stat {
    mode: i16;
    size: i64;
    mtime: i64;
}
```

[Declaration](sys/windows/fs.wave#L132)

```wave
pub fun open(path: str, flags: i32, mode: i32) -> i64
```

[Declaration](sys/windows/fs.wave#L198)

```wave
pub fun close(fd: i64) -> i64
```

[Declaration](sys/windows/fs.wave#L205)

```wave
pub fun dup(fd: i64) -> i64
```

[Declaration](sys/windows/fs.wave#L214)

```wave
pub fun dup2(oldfd: i64, newfd: i64) -> i64
```

[Declaration](sys/windows/fs.wave#L215)

```wave
pub fun pipe(fds: ptr<i32>) -> i64
```

[Declaration](sys/windows/fs.wave#L217)

```wave
pub fun fsync(fd: i64) -> i64
```

[Declaration](sys/windows/fs.wave#L222)

```wave
pub fun fcntl(fd: i64, cmd: i32, arg: i64) -> i64
```

[Declaration](sys/windows/fs.wave#L224)

```wave
pub fun read(fd: i64, buf: ptr<u8>, len: i64) -> i64
```

[Declaration](sys/windows/fs.wave#L237)

```wave
pub fun write(fd: i64, buf: ptr<u8>, len: i64) -> i64
```

[Declaration](sys/windows/fs.wave#L244)

```wave
pub fun getcwd(buf: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/windows/fs.wave#L273)

```wave
pub fun chdir(path: str) -> i64
```

[Declaration](sys/windows/fs.wave#L284)

```wave
pub fun access(path: str, mode: i32) -> i64
```

[Declaration](sys/windows/fs.wave#L314)

```wave
pub fun lseek(fd: i64, offset: i64, whence: i32) -> i64
```

[Declaration](sys/windows/fs.wave#L320)

```wave
pub fun unlink(path: str) -> i64
```

[Declaration](sys/windows/fs.wave#L331)

```wave
pub fun mkdir(path: str, mode: i32) -> i64
```

[Declaration](sys/windows/fs.wave#L342)

```wave
pub fun rmdir(path: str) -> i64
```

[Declaration](sys/windows/fs.wave#L353)

```wave
pub fun rename_path(old_path: str, new_path: str) -> i64
```

[Declaration](sys/windows/fs.wave#L370)

```wave
pub fun fstat(fd: i64, st: ptr<Stat>) -> i64
```

[Declaration](sys/windows/fs.wave#L392)

```wave
pub fun stat(path: str, st: ptr<Stat>) -> i64
```

[Declaration](sys/windows/fs.wave#L425)

```wave
pub fun same_file(first_fd: i64, second_fd: i64) -> i64
```

[Declaration](sys/windows/fs.wave#L448)

```wave
pub fun ftruncate(fd: i64, length: i64) -> i64
```

## `std::sys::windows::fs_errors`

[Source](sys/windows/fs_errors.wave)

[Declaration](sys/windows/fs_errors.wave#L8)

```wave
pub fun windows_fs_error(code: u32) -> i64
```

## `std::sys::windows::interfaces`

[Source](sys/windows/interfaces.wave)

[Declaration](sys/windows/interfaces.wave#L1)

```wave
pub struct NativeIfAddrs { unavailable: u8; }
```

[Declaration](sys/windows/interfaces.wave#L2)

```wave
pub fun interfaces_open(output: ptr<ptr<NativeIfAddrs>>) -> i64
```

[Declaration](sys/windows/interfaces.wave#L3)

```wave
pub fun interfaces_close(first: ptr<NativeIfAddrs>)
```

[Declaration](sys/windows/interfaces.wave#L4)

```wave
pub fun interface_next(value: ptr<NativeIfAddrs>) -> ptr<NativeIfAddrs>
```

[Declaration](sys/windows/interfaces.wave#L5)

```wave
pub fun interface_name(value: ptr<NativeIfAddrs>) -> ptr<u8>
```

[Declaration](sys/windows/interfaces.wave#L6)

```wave
pub fun interface_flags(value: ptr<NativeIfAddrs>) -> u32
```

[Declaration](sys/windows/interfaces.wave#L7)

```wave
pub fun interface_address(value: ptr<NativeIfAddrs>) -> ptr<i8>
```

[Declaration](sys/windows/interfaces.wave#L8)

```wave
pub fun interface_family(value: ptr<NativeIfAddrs>) -> i32
```

[Declaration](sys/windows/interfaces.wave#L9)

```wave
pub fun interface_index(name: ptr<u8>) -> u32
```

## `std::sys::windows::iocp`

[Source](sys/windows/iocp.wave)

[Declaration](sys/windows/iocp.wave#L22)

```wave
pub struct IocpCompletion { token: i64; value: i64; kind: i32; }
```

[Declaration](sys/windows/iocp.wave#L65)

```wave
pub fun iocp_start_io(token:i64,socket:i64,destination:ptr<u8>,length:i64,writing:bool)->i64
```

[Declaration](sys/windows/iocp.wave#L88)

```wave
pub fun iocp_watch(token:i64,socket:i64,interests:i32)->i64
```

[Declaration](sys/windows/iocp.wave#L103)

```wave
pub fun iocp_cancel(token:i64)->i32
```

[Declaration](sys/windows/iocp.wave#L118)

```wave
pub fun iocp_wait(output:ptr<IocpCompletion>,timeout_ms:i32)->i64
```

[Declaration](sys/windows/iocp.wave#L151)

```wave
pub fun iocp_shutdown()
```

## `std::sys::windows::local_addr`

[Source](sys/windows/local_addr.wave)

[Declaration](sys/windows/local_addr.wave#L9)

```wave
pub const AF_LOCAL: i32 = 1;
```

[Declaration](sys/windows/local_addr.wave#L10)

```wave
pub const NATIVE_LOCAL_PATH_CAPACITY: i32 = 108;
```

[Declaration](sys/windows/local_addr.wave#L12)

```wave
pub struct NativeLocalAddr {
    family: u16;
    path: array<u8, 108>;
}
```

[Declaration](sys/windows/local_addr.wave#L17)

```wave
pub fun native_local_addr(path: str, output: ptr<NativeLocalAddr>) -> i32
```

[Declaration](sys/windows/local_addr.wave#L41)

```wave
pub fun native_local_abstract(name: str, output: ptr<NativeLocalAddr>) -> i32
```

## `std::sys::windows::memory`

[Source](sys/windows/memory.wave)

[Declaration](sys/windows/memory.wave#L29)

```wave
pub const PROT_READ: i32 = 1;
```

[Declaration](sys/windows/memory.wave#L30)

```wave
pub const PROT_WRITE: i32 = 2;
```

[Declaration](sys/windows/memory.wave#L31)

```wave
pub const MAP_PRIVATE: i32 = 2;
```

[Declaration](sys/windows/memory.wave#L32)

```wave
pub const MAP_ANONYMOUS: i32 = 32;
```

[Declaration](sys/windows/memory.wave#L41)

```wave
pub fun mmap(
    addr: ptr<i8>,
    length: i64,
    prot: i32,
    flags: i32,
    fd: i64,
    offset: i64
) -> ptr<i8>
```

[Declaration](sys/windows/memory.wave#L65)

```wave
pub fun munmap(addr: ptr<i8>, length: i64) -> i64
```

[Declaration](sys/windows/memory.wave#L76)

```wave
pub fun brk(addr: ptr<i8>) -> ptr<i8>
```

[Declaration](sys/windows/memory.wave#L80)

```wave
pub fun sys_alloc(size: i64) -> ptr<u8>
```

[Declaration](sys/windows/memory.wave#L87)

```wave
pub fun sys_free(p: ptr<u8>, size: i64) -> i64
```

[Declaration](sys/windows/memory.wave#L94)

```wave
pub fun sys_page_size() -> i64
```

## `std::sys::windows::path_encoding`

[Source](sys/windows/path_encoding.wave)

[Declaration](sys/windows/path_encoding.wave#L17)

```wave
pub struct WidePath {
    data: ptr<u16>;
    bytes: i64;
    error: i64;
}
```

[Declaration](sys/windows/path_encoding.wave#L24)

```wave
pub fun wide_path(path: str) -> WidePath
```

[Declaration](sys/windows/path_encoding.wave#L49)

```wave
pub fun release_path(path: WidePath, status: i64) -> i64
```

[Declaration](sys/windows/path_encoding.wave#L58)

```wave
pub fun wide_path_to_utf8(source: ptr<u16>, units: i32, destination: ptr<u8>, capacity: i64) -> i64
```

## `std::sys::windows::process`

[Source](sys/windows/process.wave)

[Declaration](sys/windows/process.wave#L14)

```wave
pub fun exit(code: i32) -> !
```

[Declaration](sys/windows/process.wave#L19)

```wave
pub fun getpid() -> i64
```

[Declaration](sys/windows/process.wave#L23)

```wave
pub fun getppid() -> i64
```

[Declaration](sys/windows/process.wave#L27)

```wave
pub fun fork() -> i64
```

[Declaration](sys/windows/process.wave#L31)

```wave
pub fun execve(path: str, argv: ptr<ptr<i8>>, envp: ptr<ptr<i8>>) -> i64
```

[Declaration](sys/windows/process.wave#L35)

```wave
pub fun waitpid(pid: i64, status: ptr<i32>, options: i32) -> i64
```

[Declaration](sys/windows/process.wave#L39)

```wave
pub fun kill(pid: i64, sig: i32) -> i64
```

## `std::sys::windows::random`

[Source](sys/windows/random.wave)

[Declaration](sys/windows/random.wave#L8)

```wave
pub fun sys_random_available() -> bool
```

[Declaration](sys/windows/random.wave#L10)

```wave
pub fun sys_random_read(buffer: ptr<u8>, size: i64) -> i64
```

## `std::sys::windows::resolver`

[Source](sys/windows/resolver.wave)

[Declaration](sys/windows/resolver.wave#L5)

```wave
pub struct NativeAddrInfo {
    flags: i32;
    family: i32;
    socket_type: i32;
    protocol: i32;
    address_length: u64;
    canonical_name: ptr<i8>;
    address: ptr<i8>;
    next: ptr<NativeAddrInfo>;
}
```

[Declaration](sys/windows/resolver.wave#L21)

```wave
pub fun resolver_open(
    host: str, service: str, socket_type: i32, protocol: i32,
    result: ptr<ptr<NativeAddrInfo>>
) -> i64
```

[Declaration](sys/windows/resolver.wave#L40)

```wave
pub fun resolver_close(result: ptr<NativeAddrInfo>)
```

[Declaration](sys/windows/resolver.wave#L44)

```wave
pub fun resolver_family(result: ptr<NativeAddrInfo>) -> i32
```

[Declaration](sys/windows/resolver.wave#L45)

```wave
pub fun resolver_address(result: ptr<NativeAddrInfo>) -> ptr<i8>
```

[Declaration](sys/windows/resolver.wave#L46)

```wave
pub fun resolver_address_length(result: ptr<NativeAddrInfo>) -> i64
```

[Declaration](sys/windows/resolver.wave#L49)

```wave
pub fun resolver_next(result: ptr<NativeAddrInfo>) -> ptr<NativeAddrInfo>
```

## `std::sys::windows::socket`

[Source](sys/windows/socket.wave)

[Declaration](sys/windows/socket.wave#L40)

```wave
pub const AF_INET: i32 = 2;
```

[Declaration](sys/windows/socket.wave#L41)

```wave
pub const AF_INET6: i32 = 23;
```

[Declaration](sys/windows/socket.wave#L42)

```wave
pub const SOCK_STREAM: i32 = 1;
```

[Declaration](sys/windows/socket.wave#L43)

```wave
pub const SOCK_DGRAM: i32 = 2;
```

[Declaration](sys/windows/socket.wave#L44)

```wave
pub const IPPROTO_IP: i32 = 0;
```

[Declaration](sys/windows/socket.wave#L45)

```wave
pub const IPPROTO_IPV6: i32 = 41;
```

[Declaration](sys/windows/socket.wave#L46)

```wave
pub const IPPROTO_TCP: i32 = 6;
```

[Declaration](sys/windows/socket.wave#L47)

```wave
pub const IPPROTO_UDP: i32 = 17;
```

[Declaration](sys/windows/socket.wave#L48)

```wave
pub const SHUT_RD: i32 = 0;
```

[Declaration](sys/windows/socket.wave#L49)

```wave
pub const SHUT_WR: i32 = 1;
```

[Declaration](sys/windows/socket.wave#L50)

```wave
pub const SHUT_RDWR: i32 = 2;
```

[Declaration](sys/windows/socket.wave#L51)

```wave
pub const SOL_SOCKET: i32 = 65535;
```

[Declaration](sys/windows/socket.wave#L52)

```wave
pub const SO_REUSEADDR: i32 = 4;
```

[Declaration](sys/windows/socket.wave#L54)

```wave
pub const SO_REUSEPORT: i32 = 0;
```

[Declaration](sys/windows/socket.wave#L55)

```wave
pub const SO_ERROR: i32 = 4103;
```

[Declaration](sys/windows/socket.wave#L56)

```wave
pub const SO_RCVBUF: i32 = 4098;
```

[Declaration](sys/windows/socket.wave#L57)

```wave
pub const SO_SNDBUF: i32 = 4097;
```

[Declaration](sys/windows/socket.wave#L58)

```wave
pub const SO_BROADCAST: i32 = 32;
```

[Declaration](sys/windows/socket.wave#L59)

```wave
pub const SO_KEEPALIVE: i32 = 8;
```

[Declaration](sys/windows/socket.wave#L60)

```wave
pub const TCP_NODELAY: i32 = 1;
```

[Declaration](sys/windows/socket.wave#L61)

```wave
pub const IP_TTL: i32 = 4;
```

[Declaration](sys/windows/socket.wave#L62)

```wave
pub const IP_MULTICAST_IF: i32 = 9;
```

[Declaration](sys/windows/socket.wave#L63)

```wave
pub const IP_MULTICAST_TTL: i32 = 10;
```

[Declaration](sys/windows/socket.wave#L64)

```wave
pub const IP_MULTICAST_LOOP: i32 = 11;
```

[Declaration](sys/windows/socket.wave#L65)

```wave
pub const IP_ADD_MEMBERSHIP: i32 = 12;
```

[Declaration](sys/windows/socket.wave#L66)

```wave
pub const IP_DROP_MEMBERSHIP: i32 = 13;
```

[Declaration](sys/windows/socket.wave#L67)

```wave
pub const IPV6_MULTICAST_IF: i32 = 9;
```

[Declaration](sys/windows/socket.wave#L68)

```wave
pub const IPV6_MULTICAST_HOPS: i32 = 10;
```

[Declaration](sys/windows/socket.wave#L69)

```wave
pub const IPV6_MULTICAST_LOOP: i32 = 11;
```

[Declaration](sys/windows/socket.wave#L70)

```wave
pub const IPV6_JOIN_GROUP: i32 = 12;
```

[Declaration](sys/windows/socket.wave#L71)

```wave
pub const IPV6_LEAVE_GROUP: i32 = 13;
```

[Declaration](sys/windows/socket.wave#L72)

```wave
pub const MSG_NOSIGNAL: i32 = 0;
```

[Declaration](sys/windows/socket.wave#L73)

```wave
pub const FIONBIO: u32 = 2147772030 as u32;
```

[Declaration](sys/windows/socket.wave#L74)

```wave
pub const POLLIN: i16 = 256;
```

[Declaration](sys/windows/socket.wave#L75)

```wave
pub const POLLOUT: i16 = 16;
```

[Declaration](sys/windows/socket.wave#L76)

```wave
pub const POLLERR: i16 = 1;
```

[Declaration](sys/windows/socket.wave#L77)

```wave
pub const POLLHUP: i16 = 2;
```

[Declaration](sys/windows/socket.wave#L78)

```wave
pub const POLLNVAL: i16 = 4;
```

[Declaration](sys/windows/socket.wave#L80)

```wave
pub struct PollFd {
    fd: i64;
    events: i16;
    revents: i16;
}
```

[Declaration](sys/windows/socket.wave#L88)

```wave
pub fun socket_runtime_init() -> i64
```

[Declaration](sys/windows/socket.wave#L97)

```wave
pub fun socket_runtime_cleanup() -> i64
```

[Declaration](sys/windows/socket.wave#L106)

```wave
pub fun socket_last_error() -> i64
```

[Declaration](sys/windows/socket.wave#L115)

```wave
pub fun socket(domain: i32, ty: i32, protocol: i32) -> i64
```

[Declaration](sys/windows/socket.wave#L120)

```wave
pub fun bind(fd: i64, addr: ptr<i8>, len: i32) -> i64
```

[Declaration](sys/windows/socket.wave#L123)

```wave
pub fun listen(fd: i64, backlog: i32) -> i64
```

[Declaration](sys/windows/socket.wave#L126)

```wave
pub fun accept(fd: i64, addr: ptr<i8>, len: ptr<i32>) -> i64
```

[Declaration](sys/windows/socket.wave#L129)

```wave
pub fun connect(fd: i64, addr: ptr<i8>, len: i32) -> i64
```

[Declaration](sys/windows/socket.wave#L132)

```wave
pub fun getsockname(fd: i64, addr: ptr<i8>, len: ptr<i32>) -> i64
```

[Declaration](sys/windows/socket.wave#L135)

```wave
pub fun getpeername(fd: i64, addr: ptr<i8>, len: ptr<i32>) -> i64
```

[Declaration](sys/windows/socket.wave#L138)

```wave
pub fun shutdown(fd: i64, how: i32) -> i64
```

[Declaration](sys/windows/socket.wave#L141)

```wave
pub fun close_socket(fd: i64) -> i64
```

[Declaration](sys/windows/socket.wave#L148)

```wave
pub fun socket_set_nonblock(fd: i64, enabled: i32) -> i64
```

[Declaration](sys/windows/socket.wave#L153)

```wave
pub fun setsockopt(fd: i64, level: i32, optname: i32, optval: ptr<i8>, optlen: i32) -> i64
```

[Declaration](sys/windows/socket.wave#L156)

```wave
pub fun getsockopt(fd: i64, level: i32, optname: i32, optval: ptr<i8>, optlen: ptr<i32>) -> i64
```

[Declaration](sys/windows/socket.wave#L159)

```wave
pub fun poll(fds: ptr<PollFd>, nfds: i64, timeout_ms: i32) -> i64
```

[Declaration](sys/windows/socket.wave#L163)

```wave
pub fun send(fd: i64, buf: ptr<u8>, len: i64, flags: i32) -> i64
```

[Declaration](sys/windows/socket.wave#L167)

```wave
pub fun recv(fd: i64, buf: ptr<u8>, len: i64, flags: i32) -> i64
```

[Declaration](sys/windows/socket.wave#L171)

```wave
pub fun sendto(fd: i64, buf: ptr<u8>, len: i64, flags: i32, addr: ptr<i8>, addrlen: i32) -> i64
```

[Declaration](sys/windows/socket.wave#L175)

```wave
pub fun recvfrom(fd: i64, buf: ptr<u8>, len: i64, flags: i32, addr: ptr<i8>, addrlen: ptr<i32>) -> i64
```

## `std::sys::windows::socket_addr`

[Source](sys/windows/socket_addr.wave)

[Declaration](sys/windows/socket_addr.wave#L8)

```wave
pub const NATIVE_SOCKADDR_V4_LEN: i32 = 16;
```

[Declaration](sys/windows/socket_addr.wave#L9)

```wave
pub const NATIVE_SOCKADDR_V6_LEN: i32 = 28;
```

[Declaration](sys/windows/socket_addr.wave#L11)

```wave
pub struct NativeSockAddrV4 {
    family: u16;
    port: u16;
    addr: u32;
    zero: array<u8, 8>;
}
```

[Declaration](sys/windows/socket_addr.wave#L18)

```wave
pub struct NativeSockAddrV6 {
    family: u16;
    port: u16;
    flowinfo: u32;
    addr: array<u8, 16>;
    scope_id: u32;
}
```

[Declaration](sys/windows/socket_addr.wave#L26)

```wave
pub fun native_sockaddr_v4(ip_network: u32, port_network: u16) -> NativeSockAddrV4
```

[Declaration](sys/windows/socket_addr.wave#L35)

```wave
pub fun native_sockaddr_v4_ip(value: NativeSockAddrV4) -> u32
```

[Declaration](sys/windows/socket_addr.wave#L36)

```wave
pub fun native_sockaddr_v4_port(value: NativeSockAddrV4) -> u16
```

[Declaration](sys/windows/socket_addr.wave#L38)

```wave
pub fun native_sockaddr_v6(
    ip: ptr<u8>, port_network: u16, flowinfo: u32, scope_id: u32
) -> NativeSockAddrV6
```

[Declaration](sys/windows/socket_addr.wave#L51)

```wave
pub fun native_sockaddr_v6_copy_ip(value: NativeSockAddrV6, dst: ptr<u8>)
```

[Declaration](sys/windows/socket_addr.wave#L57)

```wave
pub fun native_sockaddr_v6_port(value: NativeSockAddrV6) -> u16
```

## `std::sys::windows::time`

[Source](sys/windows/time.wave)

[Declaration](sys/windows/time.wave#L18)

```wave
pub struct TimeSpec {
    sec: i64;
    nsec: i64;
}
```

[Declaration](sys/windows/time.wave#L23)

```wave
pub fun nanosleep(req: ptr<TimeSpec>, rem: ptr<TimeSpec>) -> i64
```

[Declaration](sys/windows/time.wave#L52)

```wave
pub fun clock_gettime(clock_id: i32, tp: ptr<TimeSpec>) -> i64
```

## `std::sys::windows::timed_socket`

[Source](sys/windows/timed_socket.wave)

[Declaration](sys/windows/timed_socket.wave#L15)

```wave
pub struct TimedSocketResult {
    count: i64;
    error: i64;
}
```

[Declaration](sys/windows/timed_socket.wave#L40)

```wave
pub fun socket_io_timeout(fd: i64, data: ptr<u8>, length: i64, timeout_ms: i32, writing: bool) -> TimedSocketResult
```

## `std::sys::windows::tty`

[Source](sys/windows/tty.wave)

[Declaration](sys/windows/tty.wave#L9)

```wave
pub const TTY_SYS_IOCTL: i64 = 0;
```

[Declaration](sys/windows/tty.wave#L10)

```wave
pub const TTY_SYS_FCNTL: i64 = 0;
```

[Declaration](sys/windows/tty.wave#L11)

```wave
pub const TTY_TCGETS: i64 = 0;
```

[Declaration](sys/windows/tty.wave#L12)

```wave
pub const TTY_TCSETS: i64 = 0;
```

[Declaration](sys/windows/tty.wave#L13)

```wave
pub const TTY_TCSETSW: i64 = 0;
```

[Declaration](sys/windows/tty.wave#L14)

```wave
pub const TTY_TCSETSF: i64 = 0;
```

[Declaration](sys/windows/tty.wave#L15)

```wave
pub const TTY_TCSANOW: i32 = 0;
```

[Declaration](sys/windows/tty.wave#L16)

```wave
pub const TTY_TCSADRAIN: i32 = 1;
```

[Declaration](sys/windows/tty.wave#L17)

```wave
pub const TTY_TCSAFLUSH: i32 = 2;
```

[Declaration](sys/windows/tty.wave#L18)

```wave
pub const TTY_F_GETFL: i32 = 3;
```

[Declaration](sys/windows/tty.wave#L19)

```wave
pub const TTY_F_SETFL: i32 = 4;
```

[Declaration](sys/windows/tty.wave#L20)

```wave
pub const TTY_O_NONBLOCK: i32 = 0;
```

[Declaration](sys/windows/tty.wave#L21)

```wave
pub const TTY_ICANON: u32 = 2;
```

[Declaration](sys/windows/tty.wave#L22)

```wave
pub const TTY_ECHO: u32 = 4;
```

[Declaration](sys/windows/tty.wave#L23)

```wave
pub const TTY_VTIME_IDX: i32 = 5;
```

[Declaration](sys/windows/tty.wave#L24)

```wave
pub const TTY_VMIN_IDX: i32 = 6;
```

[Declaration](sys/windows/tty.wave#L26)

```wave
pub struct Termios {
    c_iflag: u32;
    c_oflag: u32;
    c_cflag: u32;
    c_lflag: u32;
    c_line: u8;
    c_cc: array<u8, 32>;
    c_ispeed: u32;
    c_ospeed: u32;
}
```

[Declaration](sys/windows/tty.wave#L37)

```wave
pub struct TtyRawState {
    term: Termios;
    flags: i32;
    active: i32;
}
```

[Declaration](sys/windows/tty.wave#L43)

```wave
pub fun tty_getattr(fd: i32, t: ptr<Termios>) -> i64
```

[Declaration](sys/windows/tty.wave#L44)

```wave
pub fun tty_setattr(fd: i32, action: i32, t: ptr<Termios>) -> i64
```

[Declaration](sys/windows/tty.wave#L45)

```wave
pub fun tty_getfl(fd: i32) -> i64
```

[Declaration](sys/windows/tty.wave#L46)

```wave
pub fun tty_setfl(fd: i32, flags: i32) -> i64
```

[Declaration](sys/windows/tty.wave#L47)

```wave
pub fun tty_enable_raw_nonblock(fd: i32, st: ptr<TtyRawState>) -> i64
```

[Declaration](sys/windows/tty.wave#L51)

```wave
pub fun tty_restore(fd: i32, st: ptr<TtyRawState>)
```

## `std::sys::windows::vector_io`

[Source](sys/windows/vector_io.wave)

[Declaration](sys/windows/vector_io.wave#L3)

```wave
pub struct NativeIoSlice { length: u32; data: ptr<u8>; }
```

[Declaration](sys/windows/vector_io.wave#L13)

```wave
pub fun native_io_slice(data: ptr<u8>, length: i64, output: ptr<NativeIoSlice>) -> i64
```

[Declaration](sys/windows/vector_io.wave#L20)

```wave
pub fun vector_send(fd: i64, vectors: ptr<NativeIoSlice>, count: i32, flags: i32) -> i64
```

[Declaration](sys/windows/vector_io.wave#L29)

```wave
pub fun vector_recv(fd: i64, vectors: ptr<NativeIoSlice>, count: i32, flags: i32) -> i64
```

## `std::task::net`

[Source](task/net.wave)

[Declaration](task/net.wave#L94)

```wave
pub async fun accept(fd: i64, timeout_ms: i64) -> i64
```

[Declaration](task/net.wave#L118)

```wave
pub async fun connect(fd: i64, address: SocketAddrV4, timeout_ms: i64) -> i64
```

[Declaration](task/net.wave#L138)

```wave
pub async fun accept_v6(fd: i64, timeout_ms: i64) -> i64
```

[Declaration](task/net.wave#L162)

```wave
pub async fun connect_v6(fd: i64, address: SocketAddrV6, timeout_ms: i64) -> i64
```

[Declaration](task/net.wave#L174)

```wave
pub async fun read_exact(fd: i64, buffer: ptr<u8>, size: i64, timeout_ms: i64) -> NetIoResult
```

[Declaration](task/net.wave#L191)

```wave
pub fun close(fd: i64) -> i64
```

[Declaration](task/net.wave#L194)

```wave
#[target(os="linux")]
pub async fun read(fd:i64,buffer:ptr<u8>,size:i64,timeout_ms:i64)->NetIoResult
```

[Declaration](task/net.wave#L196)

```wave
#[target(os="linux")]
pub async fun write_all(fd:i64,buffer:ptr<u8>,size:i64,timeout_ms:i64)->NetIoResult
```

[Declaration](task/net.wave#L199)

```wave
#[target(os="macos")]
pub async fun read(fd:i64,buffer:ptr<u8>,size:i64,timeout_ms:i64)->NetIoResult
```

[Declaration](task/net.wave#L201)

```wave
#[target(os="macos")]
pub async fun write_all(fd:i64,buffer:ptr<u8>,size:i64,timeout_ms:i64)->NetIoResult
```

[Declaration](task/net.wave#L204)

```wave
#[target(os="freebsd")]
pub async fun read(fd:i64,buffer:ptr<u8>,size:i64,timeout_ms:i64)->NetIoResult
```

[Declaration](task/net.wave#L206)

```wave
#[target(os="freebsd")]
pub async fun write_all(fd:i64,buffer:ptr<u8>,size:i64,timeout_ms:i64)->NetIoResult
```

[Declaration](task/net.wave#L211)

```wave
#[target(os="windows")]
pub async fun read(fd:i64,buffer:ptr<u8>,size:i64,timeout_ms:i64)->NetIoResult
```

[Declaration](task/net.wave#L221)

```wave
#[target(os="windows")]
pub async fun write_all(fd:i64,buffer:ptr<u8>,size:i64,timeout_ms:i64)->NetIoResult
```

## `std::task`

[Source](task.wave)

[Declaration](task.wave#L668)

```wave
pub fun block_on<T>(future: Future<T>) -> T
```

[Declaration](task.wave#L669)

```wave
pub fun spawn<T>(future: Future<T>) -> Future<T>
```

[Declaration](task.wave#L670)

```wave
pub async fun join<T>(future: Future<T>) -> T
```

[Declaration](task.wave#L671)

```wave
pub fun cancel<T>(future: Future<T>) -> bool
```

[Declaration](task.wave#L672)

```wave
pub fun yield_now() -> Future<void>
```

[Declaration](task.wave#L673)

```wave
pub fun shutdown()
```

[Declaration](task.wave#L701)

```wave
pub fun wait_fd(fd: i64, flags: i32, timeout_ms: i64) -> Future<i32>
```

[Declaration](task.wave#L703)

```wave
pub fun sleep_ms(milliseconds: i64) -> Future<i32>
```

[Declaration](task.wave#L704)

```wave
pub fun closing_fd(fd: i64)
```

[Declaration](task.wave#L872)

```wave
pub fun cancel_and_join<T>(future: Future<T>) -> Future<void>
```

[Declaration](task.wave#L874)

```wave
#[target(os="windows")]
pub fun overlapped_read(fd: i64, buffer: ptr<u8>, length: i64, timeout_ms: i64) -> Future<i64>
```

[Declaration](task.wave#L876)

```wave
#[target(os="windows")]
pub fun overlapped_write(fd: i64, buffer: ptr<u8>, length: i64, timeout_ms: i64) -> Future<i64>
```

## `std::time::clock`

[Source](time/clock.wave)

[Declaration](time/clock.wave#L19)

```wave
pub import("std::sys::time")::{
    TimeSpec,
    nanosleep,
    clock_gettime,
};
```

[Declaration](time/clock.wave#L30)

```wave
pub fun time_now_realtime(tp: ptr<TimeSpec>) -> i64
```

[Declaration](time/clock.wave#L34)

```wave
pub fun time_now_monotonic(tp: ptr<TimeSpec>) -> i64
```

[Declaration](time/clock.wave#L38)

```wave
pub fun time_now_realtime_duration(output: ptr<Duration>) -> i64
```

[Declaration](time/clock.wave#L47)

```wave
pub fun time_now_monotonic_duration(output: ptr<Duration>) -> i64
```

[Declaration](time/clock.wave#L56)

```wave
pub fun time_now_realtime_ns() -> i64
```

[Declaration](time/clock.wave#L71)

```wave
pub fun time_now_monotonic_ns() -> i64
```

## `std::time::consts`

[Source](time/consts.wave)

[Declaration](time/consts.wave#L19)

```wave
pub const TIME_CLOCK_REALTIME: i32 = 0;
```

[Declaration](time/consts.wave#L20)

```wave
pub const TIME_CLOCK_MONOTONIC: i32 = 1;
```

[Declaration](time/consts.wave#L22)

```wave
pub const TIME_NS_PER_US: i64 = 1000;
```

[Declaration](time/consts.wave#L23)

```wave
pub const TIME_NS_PER_MS: i64 = 1000000;
```

[Declaration](time/consts.wave#L24)

```wave
pub const TIME_NS_PER_SEC: i64 = 1000000000;
```

[Declaration](time/consts.wave#L25)

```wave
pub const TIME_US_PER_MS: i64 = 1000;
```

[Declaration](time/consts.wave#L26)

```wave
pub const TIME_MS_PER_SEC: i64 = 1000;
```

[Declaration](time/consts.wave#L27)

```wave
pub const TIME_SEC_PER_MINUTE: i64 = 60;
```

[Declaration](time/consts.wave#L28)

```wave
pub const TIME_SEC_PER_HOUR: i64 = 3600;
```

[Declaration](time/consts.wave#L29)

```wave
pub const TIME_SEC_PER_DAY: i64 = 86400;
```

## `std::time::datetime`

[Source](time/datetime.wave)

[Declaration](time/datetime.wave#L29)

```wave
pub const TIME_WEEKDAY_SUNDAY: i32 = 0;
```

[Declaration](time/datetime.wave#L30)

```wave
pub const TIME_WEEKDAY_MONDAY: i32 = 1;
```

[Declaration](time/datetime.wave#L31)

```wave
pub const TIME_WEEKDAY_TUESDAY: i32 = 2;
```

[Declaration](time/datetime.wave#L32)

```wave
pub const TIME_WEEKDAY_WEDNESDAY: i32 = 3;
```

[Declaration](time/datetime.wave#L33)

```wave
pub const TIME_WEEKDAY_THURSDAY: i32 = 4;
```

[Declaration](time/datetime.wave#L34)

```wave
pub const TIME_WEEKDAY_FRIDAY: i32 = 5;
```

[Declaration](time/datetime.wave#L35)

```wave
pub const TIME_WEEKDAY_SATURDAY: i32 = 6;
```

[Declaration](time/datetime.wave#L38)

```wave
pub struct DateTime {
    year: i64;
    month: i32;
    day: i32;
    hour: i32;
    minute: i32;
    second: i32;
    nanosecond: i64;
    weekday: i32;
    year_day: i32;
}
```

[Declaration](time/datetime.wave#L51)

```wave
pub struct UnixTimeResult {
    ok: bool;
    seconds: i64;
    nanosecond: i64;
}
```

[Declaration](time/datetime.wave#L57)

```wave
pub struct DateTimeResult {
    ok: bool;
    value: DateTime;
}
```

[Declaration](time/datetime.wave#L62)

```wave
pub fun time_datetime_zero() -> DateTime
```

[Declaration](time/datetime.wave#L79)

```wave
pub fun time_is_leap_year(year: i64) -> bool
```

[Declaration](time/datetime.wave#L84)

```wave
pub fun time_days_in_month(year: i64, month: i32) -> i32
```

[Declaration](time/datetime.wave#L110)

```wave
pub fun time_datetime_is_valid(value: DateTime) -> bool
```

[Declaration](time/datetime.wave#L130)

```wave
pub fun time_datetime_from_parts(
    year: i64, month: i32, day: i32,
    hour: i32, minute: i32, second: i32, nanosecond: i64
) -> DateTimeResult
```

[Declaration](time/datetime.wave#L153)

```wave
pub fun time_datetime_from_unix(seconds: i64, nanosecond: i64) -> DateTime
```

[Declaration](time/datetime.wave#L223)

```wave
pub fun time_datetime_to_unix(value: DateTime) -> UnixTimeResult
```

[Declaration](time/datetime.wave#L277)

```wave
pub fun time_datetime_compare(left: DateTime, right: DateTime) -> i32
```

[Declaration](time/datetime.wave#L296)

```wave
pub fun time_datetime_add_seconds(value: DateTime, seconds: i64) -> DateTimeResult
```

[Declaration](time/datetime.wave#L312)

```wave
pub fun time_now_datetime(output: ptr<DateTime>) -> i64
```

## `std::time::diff`

[Source](time/diff.wave)

[Declaration](time/diff.wave#L26)

```wave
pub fun time_diff(start_ts: TimeSpec, end_ts: TimeSpec) -> Duration
```

[Declaration](time/diff.wave#L33)

```wave
pub fun time_diff_ns(start_ts: TimeSpec, end_ts: TimeSpec) -> i64
```

[Declaration](time/diff.wave#L39)

```wave
pub fun time_diff_ms(start_ts: TimeSpec, end_ts: TimeSpec) -> i64
```

[Declaration](time/diff.wave#L43)

```wave
pub fun time_diff_us(start_ts: TimeSpec, end_ts: TimeSpec) -> i64
```

## `std::time::duration`

[Source](time/duration.wave)

[Declaration](time/duration.wave#L7)

```wave
pub struct Duration {
    seconds: i64;
    nanoseconds: i64;
}
```

[Declaration](time/duration.wave#L12)

```wave
pub struct DurationValueResult {
    ok: bool;
    value: i64;
}
```

[Declaration](time/duration.wave#L18)

```wave
pub struct DurationResult {
    ok: bool;
    value: Duration;
}
```

[Declaration](time/duration.wave#L34)

```wave
pub fun time_duration_zero() -> Duration
```

[Declaration](time/duration.wave#L46)

```wave
pub fun time_duration_is_valid(value: Duration) -> bool
```

[Declaration](time/duration.wave#L51)

```wave
pub fun time_duration_checked_new(seconds: i64, nanoseconds: i64) -> DurationResult
```

[Declaration](time/duration.wave#L71)

```wave
pub fun time_duration_new(seconds: i64, nanoseconds: i64) -> Duration
```

[Declaration](time/duration.wave#L78)

```wave
pub fun time_duration_from_ns(nanoseconds: i64) -> Duration
```

[Declaration](time/duration.wave#L82)

```wave
pub fun time_duration_from_us(microseconds: i64) -> Duration
```

[Declaration](time/duration.wave#L89)

```wave
pub fun time_duration_from_ms(milliseconds: i64) -> Duration
```

[Declaration](time/duration.wave#L96)

```wave
pub fun time_duration_from_seconds(seconds: i64) -> Duration
```

[Declaration](time/duration.wave#L100)

```wave
pub fun time_duration_checked_add(left: Duration, right: Duration) -> DurationResult
```

[Declaration](time/duration.wave#L132)

```wave
pub fun time_duration_add(left: Duration, right: Duration) -> Duration
```

[Declaration](time/duration.wave#L139)

```wave
pub fun time_duration_checked_sub(left: Duration, right: Duration) -> DurationResult
```

[Declaration](time/duration.wave#L171)

```wave
pub fun time_duration_sub(left: Duration, right: Duration) -> Duration
```

[Declaration](time/duration.wave#L178)

```wave
pub fun time_duration_compare(left: Duration, right: Duration) -> i32
```

[Declaration](time/duration.wave#L187)

```wave
pub fun time_duration_is_zero(value: Duration) -> bool
```

[Declaration](time/duration.wave#L191)

```wave
pub fun time_duration_is_negative(value: Duration) -> bool
```

[Declaration](time/duration.wave#L196)

```wave
pub fun time_duration_to_ns(value: Duration) -> DurationValueResult
```

[Declaration](time/duration.wave#L219)

```wave
pub fun time_duration_to_ms(value: Duration) -> DurationValueResult
```

[Declaration](time/duration.wave#L239)

```wave
pub fun time_duration_to_us(value: Duration) -> DurationValueResult
```

[Declaration](time/duration.wave#L258)

```wave
pub fun time_duration_whole_ms(value: Duration) -> i64
```

[Declaration](time/duration.wave#L264)

```wave
pub fun time_duration_whole_us(value: Duration) -> i64
```

## `std::time::format`

[Source](time/format.wave)

[Declaration](time/format.wave#L12)

```wave
pub const TIME_FORMAT_INVALID: i64 = -22;
```

[Declaration](time/format.wave#L13)

```wave
pub const TIME_FORMAT_NO_SPACE: i64 = -28;
```

[Declaration](time/format.wave#L14)

```wave
pub const TIME_FORMAT_ISO8601_LENGTH: i64 = 30;
```

[Declaration](time/format.wave#L34)

```wave
pub fun time_datetime_format_iso8601(value: DateTime, output: ptr<u8>, capacity: i64) -> i64
```

[Declaration](time/format.wave#L74)

```wave
pub fun time_datetime_parse_iso8601(text: str) -> DateTimeResult
```

## `std::time::sleep`

[Source](time/sleep.wave)

[Declaration](time/sleep.wave#L32)

```wave
pub fun time_sleep(duration: Duration) -> i64
```

[Declaration](time/sleep.wave#L67)

```wave
pub fun time_sleep_ns(ns: i64) -> i64
```

[Declaration](time/sleep.wave#L72)

```wave
pub fun time_sleep_us(us: i64) -> i64
```

[Declaration](time/sleep.wave#L77)

```wave
pub fun time_sleep_ms(ms: i64) -> i64
```

[Declaration](time/sleep.wave#L82)

```wave
pub fun time_sleep_seconds(seconds: i64) -> i64
```

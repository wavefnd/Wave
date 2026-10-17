// SPDX-License-Identifier: MPL-2.0
import test from "node:test";
import assert from "node:assert/strict";
import { createOutputHost, fixedDouble, missingHostImports } from "../src/runtime/wasm_host.mjs";

for (const memory64 of [false, true]) {
  test(`typed output and vararg alignment (${memory64 ? 64 : 32}-bit pointers)`, () => {
    const memory = new WebAssembly.Memory({ initial: 1 });
    const bytes = new Uint8Array(memory.buffer);
    const view = new DataView(memory.buffer);
    const pointer = value => memory64 ? BigInt(value) : value;
    const put = (offset, value) => bytes.set(Buffer.from(value + "\0"), offset);
    put(16, "%s %c %f %p %% %s");
    put(128, "한글");
    put(160, "340282366920938463463374607431768211455");
    let cursor = 256;
    const ptr = value => {
      cursor = Math.ceil(cursor / (memory64 ? 8 : 4)) * (memory64 ? 8 : 4);
      if (memory64) view.setBigUint64(cursor, BigInt(value), true);
      else view.setUint32(cursor, value, true);
      cursor += memory64 ? 8 : 4;
    };
    ptr(128);
    view.setInt32(cursor, 65, true); cursor += 4;
    cursor = Math.ceil(cursor / 8) * 8;
    view.setFloat64(cursor, -0, true); cursor += 8;
    ptr(0x1234); ptr(160);
    const chunks = [];
    const host = createOutputHost(() => memory, memory64, data => chunks.push(data));
    const expected = "한글 A -0.000000 0x1234 % 340282366920938463463374607431768211455";
    assert.equal(host.printf(pointer(16), pointer(256)), Buffer.byteLength(expected));
    assert.equal(Buffer.concat(chunks).toString(), expected);
    chunks.length = 0;
    assert.equal(host.puts(pointer(128)), Buffer.byteLength("한글\n"));
    assert.equal(Buffer.concat(chunks).toString(), "한글\n");
    memory.grow(1);
    new Uint8Array(memory.buffer).set([0xff, 0], 70000);
    chunks.length = 0;
    host.puts(pointer(70000));
    assert.deepEqual(Buffer.concat(chunks), Buffer.from([0xff, 10]));
  });
}

test("fixed floating output retains precision, ties, signs and fixed notation", () => {
  assert.equal(fixedDouble(0.0078125), "0.007812");
  assert.equal(fixedDouble(0.0234375), "0.023438");
  assert.equal(fixedDouble(-0), "-0.000000");
  assert.equal(fixedDouble(-Number.MIN_VALUE), "-0.000000");
  assert.equal(fixedDouble(1e21), "1000000000000000000000.000000");
  assert.equal(fixedDouble(Number.MAX_VALUE).length, 316);
  assert.equal(fixedDouble(Infinity), "inf");
  assert.equal(fixedDouble(-Infinity), "-inf");
  assert.equal(fixedDouble(NaN), "nan");
  for (const [value, precision, expected] of [
    [2.5, 0, "2"], [3.5, 0, "4"], [-2.5, 0, "-2"], [-3.5, 0, "-4"],
    [1.125, 2, "1.12"], [1.375, 2, "1.38"], [-1.125, 2, "-1.12"],
    [1.005, 2, "1.00"], [2.675, 2, "2.67"],
    [-0, 0, "-0"], [-0, 2, "-0.00"], [-Number.MIN_VALUE, 2, "-0.00"],
    [1e21, 2, "1000000000000000000000.00"], [1.5, 12, "1.500000000000"],
  ]) {
    assert.equal(fixedDouble(value, precision), expected, `${value} at precision ${precision}`);
  }
  for (const precision of [-1, 1.5, NaN, Infinity, 2147483648]) {
    assert.throws(() => fixedDouble(1, precision), /precision/);
  }
});

for (const memory64 of [false, true]) {
  test(`fixed precision formats keep argument order (${memory64 ? 64 : 32}-bit pointers)`, () => {
    const memory = new WebAssembly.Memory({ initial: 1 });
    const bytes = new Uint8Array(memory.buffer);
    const view = new DataView(memory.buffer);
    bytes.set(Buffer.from("%.0f %.2f %s %.9f %% %f\0"), 16);
    bytes.set(Buffer.from("next\0"), 128);
    view.setFloat64(256, 2.5, true);
    view.setFloat64(264, 1.375, true);
    if (memory64) view.setBigUint64(272, 128n, true);
    else view.setUint32(272, 128, true);
    view.setFloat64(280, -0, true);
    view.setFloat64(288, 0.0078125, true);
    const chunks = [];
    const host = createOutputHost(() => memory, memory64, data => chunks.push(data));
    const pointer = value => memory64 ? BigInt(value) : value;
    const expected = "2 1.38 next -0.000000000 % 0.007812";
    assert.equal(host.printf(pointer(16), pointer(256)), Buffer.byteLength(expected));
    assert.equal(Buffer.concat(chunks).toString(), expected);
    for (const format of ["%.f", "%.-1f", "%.+1f", "%.2s", "%.2e", "%.2", "%.2147483648f"]) {
      bytes.set(Buffer.from(format + "\0"), 16);
      assert.throws(() => host.printf(pointer(16), pointer(256)), /unsupported.*format|precision/);
    }
  });
}

test("invalid memory, unterminated strings and unsupported formats fail explicitly", () => {
  const memory = new WebAssembly.Memory({ initial: 1 });
  const host = createOutputHost(() => memory, false, () => assert.fail("must not write"));
  assert.throws(() => host.puts(65536), /outside linear memory/);
  new Uint8Array(memory.buffer).fill(65);
  assert.throws(() => host.puts(0), /no NUL terminator/);
  new Uint8Array(memory.buffer).set(Buffer.from("%d\0"));
  assert.throws(() => host.printf(0, 256), /unsupported.*format/);
  new Uint8Array(memory.buffer).set(Buffer.from("%f\0"));
  assert.throws(() => host.printf(0, 65536), /argument is outside/);
  const wide = createOutputHost(() => memory, true);
  assert.throws(() => wide.puts(1n << 60n), /host address range/);
});

test("output errors propagate", () => {
  const memory = new WebAssembly.Memory({ initial: 1 });
  const host = createOutputHost(() => memory, false, () => { throw new Error("broken pipe"); });
  assert.throws(() => host.puts(0), /broken pipe/);
});


test("missing host imports identify every unresolved namespace and function", () => {
  const string = value => [Buffer.byteLength(value), ...Buffer.from(value)];
  const functions = [["env", "host_add"], ["other", "read"], ["env", "puts"]];
  const section = [functions.length, ...functions.flatMap(([module, name]) =>
    [...string(module), ...string(name), 0, 0])];
  const module = new WebAssembly.Module(Uint8Array.from([
    0, 97, 115, 109, 1, 0, 0, 0,
    1, 4, 1, 96, 0, 0, 2, section.length, ...section,
  ]));
  assert.deepEqual(missingHostImports(module, { env: { puts() {} } }), ["env.host_add", "other.read"]);
  assert.deepEqual(missingHostImports(module, { env: { puts() {}, host_add: 42 }, other: { read() {} } }), ["env.host_add"]);
  assert.deepEqual(missingHostImports(module, { env: { puts() {}, host_add() {} }, other: { read() {} } }), []);
  assert.deepEqual(missingHostImports(module, Object.create({ env: { puts() {}, host_add() {} }, other: { read() {} } })),
    ["env.host_add", "env.puts", "other.read"]);
});

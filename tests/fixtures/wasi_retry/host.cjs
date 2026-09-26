// SPDX-License-Identifier: MPL-2.0
// Deterministic host: interrupt and permanent failures are separately scripted.
const assert = require('node:assert/strict');
const fs = require('node:fs');
(async () => {
    const module = await WebAssembly.compile(fs.readFileSync(process.argv[2]));
    let instance, io = [], lengths = [], times = [], polls = [], timeouts = [], clockError = 0, closes = 0;
    const mem = () => new DataView(instance.exports.memory.buffer);
    const transfer = (_fd, vec, count, out) => {
        assert.equal(count, 1);
        lengths.push(mem().getUint32(vec + 4, true));
        assert.ok(io.length, 'unexpected I/O retry');
        const [error, bytes] = io.shift();
        if (!error) mem().setUint32(out, bytes, true);
        return error;
    };
    const host = {
        fd_read: transfer, fd_write: transfer,
        fd_close: () => { closes++; return 27; },
        clock_time_get: (id, precision, out) => {
            assert.equal(id, 1); assert.equal(precision, 1n);
            if (clockError) return clockError;
            assert.ok(times.length, 'unexpected clock read');
            const next = times.shift();
            if (typeof next === "object") return next.error;
            mem().setBigUint64(out, next, true); return 0;
        },
        poll_oneoff: (sub, event, count, out) => {
            assert.equal(count, 1); assert.equal(mem().getUint32(sub + 16, true), 1);
            assert.equal(mem().getUint16(sub + 40, true), 0);
            timeouts.push(mem().getBigUint64(sub + 24, true));
            assert.ok(polls.length, 'unexpected sleep retry');
            const [error, eventError = 0, eventCount = 1] = polls.shift();
            mem().setUint32(out, eventCount, true); mem().setUint16(event + 8, eventError, true);
            return error;
        },
    };
    const imports = {};
    for (const item of WebAssembly.Module.imports(module)) {
        assert.equal(item.module, 'wasi_snapshot_preview1');
        (imports[item.module] ??= {})[item.name] = host[item.name] ?? (() => { throw Error(`unexpected host call ${item.name}`); });
    }
    instance = await WebAssembly.instantiate(module, imports);
    for (const name of ['read_bytes', 'write_bytes']) {
        io = [[27, 0], [0, 3], [27, 0], [0, 5]]; lengths = [];
        assert.equal(instance.exports[name](), 8n); assert.deepEqual(lengths, [8, 8, 5, 5]);
        for (const error of [4, 8, 28, 29]) {
            io = [[error, 0]]; lengths = [];
            assert.equal(instance.exports[name](), -BigInt(error)); assert.equal(lengths.length, 1);
        }
    }
    assert.equal(instance.exports.close_fd(), -27n); assert.equal(closes, 1);
    // The deadline stays 1100; each relative wait uses only the remaining time.
    times = [1000n, 1000n, 1030n, 1030n, 1070n, 1070n]; polls = [[27], [0, 27], [0]]; timeouts = [];
    assert.equal(instance.exports.sleep_ns(100n), 0n); assert.deepEqual(timeouts, [100n, 70n, 30n]);
    times = [1000n, 1000n, 1200n]; polls = [[27]]; timeouts = [];
    assert.equal(instance.exports.sleep_ns(100n), 0n); assert.deepEqual(timeouts, [100n]);
    for (const error of [4, 29]) {
        times = [1000n, 1000n]; polls = [[error]];
        assert.equal(instance.exports.sleep_ns(100n), -BigInt(error));
    }
    times = [1000n, 1030n]; polls = [[27]];
    assert.equal(instance.exports.raw_sleep_ns(100n), 70n);
    times = [1000n, 1200n]; polls = [[0, 27]];
    assert.equal(instance.exports.raw_sleep_ns(100n), 0n);
    times = [1000n, 1000n, {error: 29}]; polls = [[27]];
    assert.equal(instance.exports.sleep_ns(100n), -29n);
    times = [1000n, 1000n]; polls = [[0, 0, 0]];
    assert.equal(instance.exports.sleep_ns(100n), -29n);
    times = [(1n << 64n) - 1n]; polls = [];
    assert.equal(instance.exports.raw_sleep_ns(100n), -61n);
    times = []; polls = [];
    assert.equal(instance.exports.sleep_ns(0n), 0n);
    assert.equal(instance.exports.sleep_ns(-1n), -28n);
    assert.equal(instance.exports.raw_sleep_ns(-1n), -28n);
    clockError = 29;
    assert.equal(instance.exports.sleep_ns(100n), -29n);
    assert.equal(instance.exports.clock_read(1, 0, 0), -29n);
    clockError = 0;
    for (const timestamp of [0n, (1n << 63n) - 1n, 1n << 63n, (1n << 64n) - 1n]) {
        times = [timestamp, timestamp];
        assert.equal(instance.exports.clock_read(1, 0, 0), timestamp / 1000000000n);
        assert.equal(instance.exports.clock_read(1, 1, 0), timestamp % 1000000000n);
        assert.equal(times.length, 0);
    }
    times = [];
    assert.equal(instance.exports.clock_read(1, 0, 1), -22n);
    assert.equal(instance.exports.clock_read(-1, 0, 0), -22n);
    assert.equal(instance.exports.clock_read(4, 0, 0), -22n);
    console.log('WASI retry, deadline and full timestamp range cases passed');
})().catch(error => { console.error(error); process.exitCode = 1; });

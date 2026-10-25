// SPDX-License-Identifier: MPL-2.0
import { readFileSync } from "node:fs";

/**
 * Reads WebAssembly module binary bytes synchronously.
 */
export function readModuleBytes(modulePath) {
  return readFileSync(modulePath);
}

/**
 * Validates that private Wave symbols do not leak into module exports.
 */
export function validateNoPrivateExports(instance, extraDisallowed = []) {
  const exports = instance.exports ?? instance;
  const leaked = Object.keys(exports).filter((name) => name.startsWith("__wave_"));
  const extra = extraDisallowed.filter((name) => name in exports);
  const allLeaked = [...leaked, ...extra];
  if (allLeaked.length !== 0) {
    throw new Error(`private Wave functions leaked into exports: ${allLeaked.join(", ")}`);
  }
}

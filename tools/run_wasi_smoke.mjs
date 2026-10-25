// Runs a WASI Preview 1 command with the supplied directory preopened as fd 3.
import { WASI } from "node:wasi";
import { readModuleBytes, validateNoPrivateExports } from "./wasm_smoke_common.mjs";

const modulePath = process.argv[2];
const preopenPath = process.argv[3] ?? process.cwd();
if (!modulePath) {
  console.error("usage: node tools/run_wasi_smoke.mjs <module.wasm> [preopen-dir]");
  process.exit(2);
}

const wasi = new WASI({
  version: "preview1",
  returnOnExit: true,
  args: [],
  env: process.env,
  preopens: { ".": preopenPath },
});
// This is a synchronous command runner. Avoid asynchronous module compilation
// and top-level await so startup and termination do not depend on worker/event
// loop progress while a preloaded host handle is keeping the process alive.
const module = new WebAssembly.Module(readModuleBytes(modulePath));
const instance = new WebAssembly.Instance(module, wasi.getImportObject());
validateNoPrivateExports(instance);
// WASI commands perform synchronous fd writes and finish when _start returns
// or proc_exit unwinds it. Host handles must not keep a completed command alive.
process.exit(wasi.start(instance));

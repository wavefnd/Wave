// Instantiates the compiler's minimal browser-hosted WebAssembly fixture.
import { readModuleBytes, validateNoPrivateExports } from "./wasm_smoke_common.mjs";

const modulePath = process.argv[2];
if (!modulePath) {
  console.error("usage: node tools/run_wasm_smoke.mjs <module.wasm>");
  process.exit(2);
}

const bytes = readModuleBytes(modulePath);
const imports = {
  env: {
    host_add(a, b) {
      return a + b;
    },
  },
};
const { instance } = await WebAssembly.instantiate(bytes, imports);
const {
  wave_add: add,
  wave_features: exerciseFeatures,
  main,
  memory,
} = instance.exports;
validateNoPrivateExports(instance, ["add"]);

if (typeof add !== "function" || add(7, 5) !== 12) {
  throw new Error("exported add function returned an unexpected value");
}
if (typeof exerciseFeatures !== "function" || exerciseFeatures(5) !== 20) {
  throw new Error("WebAssembly language feature smoke test failed");
}
if (typeof main !== "function" || main() !== 42) {
  throw new Error("exported main function returned an unexpected value");
}
if (!(memory instanceof WebAssembly.Memory)) {
  throw new Error("module did not export linear memory");
}

console.log("WebAssembly smoke test passed");

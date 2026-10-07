use std::fs;
use std::path::Path;
use std::process::Command;

// Alias a copy of this checkout's std so installed compiler/std versions and
// the user's home directory cannot affect these regression tests.
fn copy_std(source: &Path, destination: &Path) {
    fs::create_dir_all(destination).unwrap();
    for entry in fs::read_dir(source).unwrap() {
        let entry = entry.unwrap();
        let to = destination.join(entry.file_name());
        if entry.file_type().unwrap().is_dir() {
            copy_std(&entry.path(), &to);
        } else if entry.path().extension().is_some_and(|e| e == "wave") {
            fs::write(
                to,
                fs::read_to_string(entry.path()).unwrap().replace("\"std::", "\"checkout_std::"),
            )
            .unwrap();
        } else {
            fs::copy(entry.path(), to).unwrap();
        }
    }
}
#[test]
fn checked_typed_memory_and_sleb128_preserve_data_and_failure_state() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let dir = std::env::temp_dir().join(format!("wave-checked-std-{}", std::process::id()));
    let std = dir.join("std");
    copy_std(&root.join("std"), &std);
    let dep = format!("checkout_std={}", std.display());
    for fixture in [
        "tests/fixtures/checked_std/typed_memory.wave",
        "tests/fixtures/checked_std/sleb128.wave",
        "tests/cases/shared/test125.wave",
        "tests/fixtures/typed_conversions/main.wave",
    ] {
        let name = Path::new(fixture).file_stem().unwrap().to_str().unwrap();
        let source = dir.join(format!("{name}.wave"));
        fs::write(
            &source,
            fs::read_to_string(root.join(fixture)).unwrap().replace("\"std::", "\"checkout_std::"),
        )
        .unwrap();
        for opt in ["-O0", "-O2"] {
            let output = Command::new(env!("CARGO_BIN_EXE_wavec"))
                .arg("build")
                .arg(&source)
                .args(["--dep", &dep, opt, "--run", "--out-dir"])
                .arg(dir.join(format!("{name}{opt}")))
                .output()
                .unwrap();
            assert!(output.status.success(), "{fixture} {opt}: {output:?}");
            if std::env::var_os("WAVE_RUN_CHECKED_CROSS").is_some() {
                run_cross_fixture(root, &source, &dep, &dir, name, opt);
            }
        }
    }
    // Exercise the unsigned layout boundary without constructing an LLVM type
    // whose bit-size itself exceeds LLVM's representable layout range.
    fs::write(
        std.join("mem/layout.wave"),
        "pub fun size_of<T>() -> u64 { return 18446744073709551615; }",
    )
    .unwrap();
    let source = dir.join("layout_boundary.wave");
    fs::write(&source, r#"import("checkout_std::mem::ops")::{mem_copy_items_checked, mem_move_items_checked, mem_zero_items_checked};
        fun main() -> i32 {
            if (mem_copy_items_checked<u8>(null, null, 1) != -4098 || mem_move_items_checked<u8>(null, null, 1) != -4098 || mem_zero_items_checked<u8>(null, 1) != -4098) { return 1; }
            if (mem_zero_items_checked<u8>(null, 0) != 0) { return 2; }
            return 0;
        }"#).unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_wavec"))
        .arg("build")
        .arg(source)
        .args(["--dep", &dep, "--run", "--out-dir"])
        .arg(dir.join("layout_boundary"))
        .output()
        .unwrap();
    assert!(output.status.success(), "layout boundary: {output:?}");
    fs::remove_dir_all(dir).unwrap();
}

// Opt-in, like the repository's existing QEMU tests. Uses no installed std or
// target libc; requires clang/LLD, Node's WebAssembly engine, and qemu-aarch64.
fn run_cross_fixture(
    root: &Path,
    source: &Path,
    dep: &str,
    directory: &Path,
    name: &str,
    opt: &str,
) {
    fn checked(command: &mut Command) {
        let output = command.output().unwrap();
        assert!(output.status.success(), "{command:?}: {output:?}");
    }
    for target in ["wasm32-unknown-unknown", "aarch64-unknown-linux-gnu"] {
        let out = directory.join(format!("{name}-{target}-{opt}"));
        checked(
            Command::new(env!("CARGO_BIN_EXE_wavec"))
                .arg("build")
                .arg(source)
                .args(["--dep", dep, opt, "--target", target, "--emit=obj", "--out-dir"])
                .arg(&out),
        );
        let object = out.join(format!("{name}.o"));
        if target.starts_with("wasm32") {
            let wasm = out.join("case.wasm");
            checked(
                Command::new("wasm-ld")
                    .args(["--no-entry", "--export=main"])
                    .arg(object)
                    .arg("-o")
                    .arg(&wasm),
            );
            checked(Command::new("node").arg("-e").arg(
                "WebAssembly.instantiate(require('fs').readFileSync(process.argv[1]), {}).then(({instance}) => process.exit(instance.exports.main())).catch(e => { console.error(e); process.exit(100); })"
            ).arg(wasm));
        } else {
            let runtime = out.join("start.o");
            checked(
                Command::new("clang")
                    .args([
                        "-target",
                        target,
                        "-ffreestanding",
                        "-fno-builtin",
                        "-fno-stack-protector",
                        "-c",
                    ])
                    .arg(root.join("tests/fixtures/linux_case_runtime/start.c"))
                    .arg("-o")
                    .arg(&runtime),
            );
            let executable = out.join("case");
            checked(
                Command::new("clang")
                    .args([
                        "-target",
                        target,
                        "-fuse-ld=lld",
                        "-nostdlib",
                        "-static",
                        "-Wl,-e,_start",
                    ])
                    .arg(runtime)
                    .arg(object)
                    .arg("-o")
                    .arg(&executable),
            );
            checked(Command::new("qemu-aarch64").arg(executable));
        }
    }
}

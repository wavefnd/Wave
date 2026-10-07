// SPDX-License-Identifier: MPL-2.0
use llvm::backend::msvc_link_args;
#[test]
fn msvc_machine_crt_and_output_modes_are_independent() {
    for (target, machine) in
        [("x86_64-pc-windows-msvc", "X64"), ("aarch64-pc-windows-msvc", "ARM64")]
    {
        for static_crt in [false, true] {
            for shared in [false, true] {
                for no_default in [false, true] {
                    let args = msvc_link_args(
                        target,
                        &["input.obj".into()],
                        "output.exe",
                        &[],
                        &[],
                        no_default,
                        static_crt,
                        shared,
                        None,
                        &[],
                    );
                    assert_eq!(
                        &args[..4],
                        &[
                            "/NOLOGO",
                            &format!("/MACHINE:{machine}"),
                            "/OUT:output.exe",
                            if shared { "/DLL" } else { "/SUBSYSTEM:CONSOLE" }
                        ]
                    );
                    let defaults = args
                        .iter()
                        .filter(|s| s.starts_with("/DEFAULTLIB:"))
                        .cloned()
                        .collect::<Vec<_>>();
                    let crt = if static_crt {
                        ["libcmt", "libvcruntime", "libucrt"]
                    } else {
                        ["msvcrt", "vcruntime", "ucrt"]
                    };
                    let expected = if no_default {
                        vec![]
                    } else {
                        crt.into_iter()
                            .chain([
                                "legacy_stdio_definitions",
                                "kernel32",
                                "user32",
                                "advapi32",
                                "shell32",
                                "ws2_32",
                                "bcrypt",
                            ])
                            .map(|s| format!("/DEFAULTLIB:{s}.lib"))
                            .collect()
                    };
                    assert_eq!(defaults, expected);
                    assert_eq!(args.iter().any(|s| s == "/NODEFAULTLIB"), no_default);
                }
            }
        }
    }
}
#[test]
fn preserves_library_suffixes_paths_and_explicit_argument_order() {
    let args = msvc_link_args(
        "x86_64-pc-windows-msvc",
        &["a b.obj".into()],
        "out file.exe",
        &["kernel32".into(), "custom.LIB".into()],
        &["C:\\SDK files\\lib".into()],
        true,
        false,
        false,
        Some("start"),
        &["/OPT:NOREF".into(), "/OPT:REF".into()],
    );
    assert_eq!(
        args,
        vec![
            "/NOLOGO",
            "/MACHINE:X64",
            "/OUT:out file.exe",
            "/SUBSYSTEM:CONSOLE",
            "/ENTRY:start",
            "a b.obj",
            "/LIBPATH:C:\\SDK files\\lib",
            "kernel32.lib",
            "custom.LIB",
            "/NODEFAULTLIB",
            "/OPT:NOREF",
            "/OPT:REF"
        ]
    );
}

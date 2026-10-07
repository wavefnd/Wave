// SPDX-License-Identifier: MPL-2.0
use llvm::msvc::{coff, sdk};
use std::fs;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
const X64: &str = "x86_64-pc-windows-msvc";
const ARM: &str = "aarch64-pc-windows-msvc";
static NEXT: AtomicU64 = AtomicU64::new(0);
struct Temp(PathBuf);
impl Temp {
    fn new() -> Self {
        let p = std::env::temp_dir().join(format!(
            "wave-msvc-{}-{}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir_all(&p).unwrap();
        Self(p)
    }

    fn lib(&self, name: &str, machine: u16) -> PathBuf {
        let p = self.0.join(name);
        fs::create_dir_all(p.parent().unwrap()).unwrap();
        fs::write(&p, obj(machine)).unwrap();
        p
    }
}
impl Drop for Temp {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}
fn obj(machine: u16) -> Vec<u8> {
    let mut b = vec![0; 20];
    b[..2].copy_from_slice(&machine.to_le_bytes());
    b
}

fn member(name: &str, bytes: &[u8]) -> Vec<u8> {
    let mut b =
        format!("{name:<16}{:<12}{:<6}{:<6}{:<8}{:<10}`\n", 0, 0, 0, 0, bytes.len()).into_bytes();
    assert_eq!(b.len(), 60);
    b.extend(bytes);
    if b.len() % 2 != 0 {
        b.push(b'\n');
    }
    b
}
#[test]
fn rejects_wrong_machine_images_bitcode_and_truncation() {
    assert!(coff::inspect(&obj(0x8664), X64).is_ok());
    assert!(coff::inspect(&obj(0xaa64), ARM).is_ok());
    for b in
        [obj(0xaa64), vec![], vec![0x64, 0x86], b"MZfake image".to_vec(), b"BC\xc0\xde".to_vec()]
    {
        assert!(coff::inspect(&b, X64).is_err(), "{b:?}");
    }
    let mut b = obj(0x8664);
    b[2] = 1;
    assert!(coff::inspect(&b, X64).is_err());
    b.resize(60, 0);
    b[36] = 1;
    b[40] = 255;
    assert!(coff::inspect(&b, X64).is_err());
}
#[test]
fn handles_bigobj_and_short_import_objects() {
    let mut b = vec![0; 56];
    b[2..4].copy_from_slice(&[255, 255]);
    b[4] = 2;
    b[6..8].copy_from_slice(&0xaa64u16.to_le_bytes());
    b[12..28].copy_from_slice(&[
        0xc7, 0xa1, 0xba, 0xd1, 0xee, 0xba, 0xa9, 0x4b, 0xaf, 0x20, 0xfa, 0xf6, 0x6a, 0xa4, 0xdc,
        0xb8,
    ]);
    assert!(coff::inspect(&b, ARM).is_ok());
    assert!(coff::inspect(&b, X64).is_err());
    b[12] = 0;
    assert!(coff::inspect(&b, ARM).is_err());
    b = vec![0; 20];
    b[2..4].copy_from_slice(&[255, 255]);
    b[6..8].copy_from_slice(&0x8664u16.to_le_bytes());
    let names = b"symbol\0native.dll\0";
    b[12..16].copy_from_slice(&(names.len() as u32).to_le_bytes());
    b.extend(names);
    assert!(coff::inspect(&b, X64).is_ok());
    b.pop();
    assert!(coff::inspect(&b, X64).is_err());
}
#[test]
fn archives_check_every_member_and_report_long_names() {
    let mut a = b"!<arch>\n".to_vec();
    assert!(coff::inspect(&a, X64).is_ok());
    a.extend(member("//", b"foreign object.obj\0"));
    a.extend(member("valid.obj/", &obj(0x8664)));
    assert!(coff::inspect(&a, X64).is_ok());
    a.extend(member("/0", &obj(0xaa64)));
    assert!(coff::inspect(&a, X64).unwrap_err().contains("foreign object.obj"));
    for cut in [9, 20, 67, a.len() - 1] {
        assert!(coff::inspect(&a[..cut], X64).is_err());
    }
    let mut a = b"!<arch>\n".to_vec();
    a.extend(member("/999", &obj(0x8664)));
    assert!(coff::inspect(&a, X64).is_err());
    assert!(coff::inspect(b"!<thin>\n", X64).is_err());
}
#[test]
fn sdk_selects_complete_numeric_versions_and_target_architecture() {
    let t = Temp::new();
    for version in ["10.0.9.0", "10.0.10.0"] {
        for (kind, lib) in [("um", "kernel32"), ("ucrt", "ucrt")] {
            t.lib(&format!("SDK/Lib/{version}/{kind}/arm64/{lib}.lib"), 0xaa64);
        }
    }
    t.lib("SDK/Lib/10.0.11.0/um/arm64/kernel32.lib", 0xaa64);
    t.lib("VC/lib/arm64/vcruntime.lib", 0xaa64);
    let env = sdk::Environment::from([
        ("WindowsSdkDir".into(), t.0.join("SDK").display().to_string()),
        ("VCToolsInstallDir".into(), t.0.join("VC").display().to_string()),
    ]);
    let p = sdk::discover(ARM, &[], &env);
    assert_eq!(p.len(), 3);
    assert!(p[0].to_string_lossy().contains("10.0.10.0"));
    assert!(sdk::discover(X64, &[], &env).is_empty());
    let mut env = env;
    env.insert("WindowsSDKVersion".into(), "10.0.9.0\\".into());
    assert!(sdk::discover(ARM, &[], &env)[0].to_string_lossy().contains("10.0.9.0"));
}
#[test]
fn explicit_paths_precede_environment_and_wrong_architecture_fails_closed() {
    let t = Temp::new();
    t.lib("explicit/thing.lib", 0x8664);
    t.lib("ambient/thing.lib", 0xaa64);
    let env = sdk::Environment::from([("LIB".into(), t.0.join("ambient").display().to_string())]);
    let paths = sdk::discover(X64, &[t.0.join("explicit").display().to_string()], &env);
    let mut args = paths.iter().map(|p| format!("/LIBPATH:{}", p.display())).collect::<Vec<_>>();
    args.push("thing.lib".into());
    assert!(sdk::validate_arguments(X64, &args).is_ok());
    args.remove(0);
    assert!(sdk::validate_arguments(X64, &args).unwrap_err().contains("machine"));
    args.push("missing.lib".into());
    assert!(sdk::validate_arguments(X64, &args).unwrap_err().contains("missing.lib"));
}
#[test]
fn missing_default_libraries_are_actionable_and_nodefaultlib_is_respected() {
    let args = vec!["/DEFAULTLIB:wave_nonexistent_sdk.lib".into()];
    let error = sdk::validate_arguments(X64, &args).unwrap_err();
    assert!(error.contains("UM/UCRT"));
    let mut args = args;
    args.push("/NODEFAULTLIB".into());
    assert!(sdk::validate_arguments(X64, &args).is_ok());
}

#[test]
fn rejected_inputs_never_launch_backend_linker_or_replace_output() {
    let t = Temp::new();
    let bad = t.lib("foreign-without-extension", 0xaa64);
    let output = t.0.join("existing.exe");
    fs::write(&output, b"preserve previous output").unwrap();
    let backend = llvm::backend::BackendOptions {
        target: Some(X64.into()),
        linker: Some(t.0.join("must-not-be-launched").display().to_string()),
        no_default_libs: true,
        ..Default::default()
    };
    let error = llvm::backend::link_objects(
        &[bad.display().to_string()],
        output.to_str().unwrap(),
        &[],
        &[],
        &backend,
    )
    .unwrap_err();
    assert!(error.to_string().contains("machine"), "{error}");
    assert_eq!(fs::read(output).unwrap(), b"preserve previous output");
}

#[test]
fn rejects_archive_index_offsets_and_truncated_symbol_names() {
    let mut a = b"!<arch>\n".to_vec();
    let mut index = 1u32.to_be_bytes().to_vec();
    index.extend(123456u32.to_be_bytes());
    index.extend(b"symbol\0");
    a.extend(member("/", &index));
    a.extend(member("object.obj/", &obj(0x8664)));
    assert!(coff::inspect(&a, X64).unwrap_err().contains("nonexistent member"));
}

#[test]
fn ordinary_install_roots_and_explicit_sdk_versions_do_not_mix() {
    let t = Temp::new();
    for (part, name) in [("um", "kernel32"), ("ucrt", "ucrt")] {
        t.lib(&format!("Program Files/Windows Kits/10/Lib/10.0.1/{part}/x64/{name}.lib"), 0x8664);
    }
    t.lib("Visual Studio/VC/Tools/MSVC/14.9/lib/x64/vcruntime.lib", 0x8664);
    t.lib("Visual Studio/VC/Tools/MSVC/14.10/lib/x64/vcruntime.lib", 0x8664);
    let mut env = sdk::Environment::from([
        ("ProgramFiles(x86)".into(), t.0.join("Program Files").display().to_string()),
        ("VSINSTALLDIR".into(), t.0.join("Visual Studio").display().to_string()),
    ]);
    let paths = sdk::discover(X64, &[], &env);
    assert_eq!(paths.len(), 3);
    assert!(paths[2].to_string_lossy().contains("14.10"));
    env.insert("WindowsSdkDir".into(), t.0.join("missing SDK").display().to_string());
    assert_eq!(
        sdk::discover(X64, &[], &env).len(),
        1,
        "an explicit missing SDK must not fall back silently"
    );
}

// A Microsoft archive's second linker member lists every object, including EC
// objects. Its symbol indices (and the first member) select native objects;
// ECSYMBOLS selects EC objects from the same offset table.
fn hybrid_archive(native_machine: u16, ec_machine: u16, native_selects_ec: bool) -> Vec<u8> {
    let native_offset = 8
        + member("/", &[0; 10]).len()
        + member("/", &[0; 20]).len()
        + member("/<ECSYMBOLS>/", &[0; 8]).len();
    let ec_offset = native_offset + member("native.obj/", &obj(native_machine)).len();
    let selected = if native_selects_ec { ec_offset } else { native_offset };
    let mut first = 1u32.to_be_bytes().to_vec();
    first.extend((selected as u32).to_be_bytes());
    first.extend(b"n\0");
    let mut second = 2u32.to_le_bytes().to_vec();
    second.extend((native_offset as u32).to_le_bytes());
    second.extend((ec_offset as u32).to_le_bytes());
    second.extend(1u32.to_le_bytes());
    second.extend((if native_selects_ec { 2u16 } else { 1u16 }).to_le_bytes());
    second.extend(b"n\0");
    let mut ec = 1u32.to_le_bytes().to_vec();
    ec.extend(2u16.to_le_bytes());
    ec.extend(b"e\0");
    let mut bytes = b"!<arch>\n".to_vec();
    bytes.extend(member("/", &first));
    bytes.extend(member("/", &second));
    bytes.extend(member("/<ECSYMBOLS>/", &ec));
    bytes.extend(member("native.obj/", &obj(native_machine)));
    bytes.extend(member("ec.obj/", &obj(ec_machine)));
    bytes
}

#[test]
fn arm64_hybrid_archives_only_exempt_valid_ec_index_members() {
    let good = hybrid_archive(0xaa64, 0xa641, false);
    assert!(coff::inspect(&good, ARM).is_ok());
    assert!(coff::inspect(&hybrid_archive(0xaa64, 0x8664, false), ARM).is_ok());
    for bad in [
        hybrid_archive(0xaa64, 0xa641, true),
        hybrid_archive(0x8664, 0xa641, false),
        hybrid_archive(0xaa64, 0x14c, false),
        obj(0xa641),
    ] {
        assert!(coff::inspect(&bad, ARM).is_err());
    }
    assert!(coff::inspect(&good, X64).is_err());
    let mut unindexed = good.clone();
    unindexed.extend(member("foreign.obj/", &obj(0xa641)));
    // VC runtime archives also retain unreferenced duplicate EC members.
    assert!(coff::inspect(&unindexed, ARM).is_ok());
    unindexed.extend(member("x64.obj/", &obj(0x8664)));
    assert!(coff::inspect(&unindexed, ARM).is_ok());
    let ec_start = 8 + member("/", &[0; 10]).len() + member("/", &[0; 20]).len() + 60;
    let mut invalid_index = good.clone();
    invalid_index[ec_start + 4..ec_start + 6].copy_from_slice(&3u16.to_le_bytes());
    assert!(coff::inspect(&invalid_index, ARM).is_err());
    let mut missing_name = good.clone();
    missing_name[ec_start + 7] = b'x';
    assert!(coff::inspect(&missing_name, ARM).is_err());
    let t = Temp::new();
    let file = t.0.join("hybrid.lib");
    fs::write(&file, &good).unwrap();
    assert!(coff::validate_file(&file, ARM).is_ok());
    assert!(coff::validate_file_all_members(&file, ARM).is_err());
    for args in [
        vec![format!("/WHOLEARCHIVE:{}", file.display())],
        vec!["/WHOLEARCHIVE".into(), file.display().to_string()],
    ] {
        assert!(sdk::validate_arguments(ARM, &args).is_err());
    }
}

#[test]
fn microsoft_archives_allow_nul_alignment_but_not_arbitrary_padding() {
    let mut archive = b"!<arch>\n".to_vec();
    let mut payload = obj(0xaa64);
    payload.push(0);
    archive.extend(member("padded.obj/", &payload));
    *archive.last_mut().unwrap() = 0;
    assert!(coff::inspect(&archive, ARM).is_ok());
    *archive.last_mut().unwrap() = b'x';
    assert!(coff::inspect(&archive, ARM).is_err());
}

#[test]
fn machine_neutral_sdk_alias_objects_cannot_hide_instructions_or_relocations() {
    let mut bytes = obj(0);
    bytes[2] = 1;
    bytes.resize(60, 0);
    bytes[20..28].copy_from_slice(b".debug$S");
    for target in [X64, ARM] {
        assert!(coff::inspect(&bytes, target).is_ok());
        let mut code = bytes.clone();
        code[56] = 0x20;
        assert!(coff::inspect(&code, target).is_err());
        let mut reloc = bytes.clone();
        reloc[52] = 1;
        assert!(coff::inspect(&reloc, target).is_err());
        let mut text = bytes.clone();
        text[20..28].copy_from_slice(b".text\0\0\0");
        assert!(coff::inspect(&text, target).is_err());
    }
}

#[test]
fn arithmetic_builtins_discovery_keeps_target_and_sdk_order() {
    let t = Temp::new();
    let x64 = t.lib("first/lib/clang/21/lib/windows/clang_rt.builtins-x86_64.lib", 0x8664);
    let arm = t.lib("second/lib/clang/21/lib/windows/clang_rt.builtins-aarch64.lib", 0xaa64);
    let roots = vec![t.0.join("first"), t.0.join("second")];
    assert_eq!(llvm::msvc::runtime::find_builtins(X64, &roots), Some(x64));
    assert_eq!(llvm::msvc::runtime::find_builtins(ARM, &roots), Some(arm));
    assert!(llvm::msvc::runtime::find_builtins("x86_64-unknown-linux-gnu", &roots).is_none());
    assert!(llvm::msvc::runtime::find_builtins(ARM, &roots[..1]).is_none());
    let mut args = vec!["/NODEFAULTLIB".to_string()];
    llvm::msvc::runtime::add_builtins(X64, &mut args);
    assert_eq!(args, ["/NODEFAULTLIB"]);
}

// SPDX-License-Identifier: MPL-2.0
use std::fs::{self, File};
use std::path::Path;
use wave_tools::sdk;

fn archive(path: &Path, links: &[(&str, &str)]) {
    let mut builder = tar::Builder::new(File::create(path).unwrap());
    let mut header = tar::Header::new_gnu();
    header.set_size(7);
    header.set_mode(0o755);
    header.set_cksum();
    builder.append_data(&mut header, "llvm/bin/tool", b"PAYLOAD".as_slice()).unwrap();
    for (name, target) in links {
        let mut header = tar::Header::new_gnu();
        header.set_entry_type(tar::EntryType::Symlink);
        header.set_size(0);
        header.set_mode(0o777);
        header.set_cksum();
        builder.append_link(&mut header, name, target).unwrap();
    }
    builder.finish().unwrap();
}

#[test]
fn extraction_retains_regular_bytes_permissions_and_rejects_links_by_default() {
    let dir = tempfile::tempdir().unwrap();
    let input = dir.path().join("bundle.tar");
    archive(&input, &[("llvm/bin/alias", "tool")]);
    let refused = dir.path().join("refused");
    assert!(sdk::extract(&input, &refused, false).is_err());
    assert!(!refused.exists());
    let output = dir.path().join("allowed");
    sdk::extract(&input, &output, true).unwrap();
    assert_eq!(fs::read(output.join("llvm/bin/alias")).unwrap(), b"PAYLOAD");
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        assert_eq!(
            fs::metadata(output.join("llvm/bin/tool")).unwrap().permissions().mode() & 0o777,
            0o755
        );
    }
}

#[test]
fn malformed_archives_never_activate_or_write_through_an_external_link() {
    let dir = tempfile::tempdir().unwrap();
    let outside = dir.path().join("outside");
    fs::create_dir(&outside).unwrap();
    for (index, links) in [
        vec![("llvm/bin/alias", "missing")],
        vec![("llvm/bin/a", "b"), ("llvm/bin/b", "a")],
        vec![("llvm/bin/alias", "/etc/passwd")],
        vec![("llvm/out", "../../outside"), ("llvm/out/written", "../bin/tool")],
        vec![("llvm/bin/alias", "../bin")],
    ]
    .iter()
    .enumerate()
    {
        let input = dir.path().join(format!("{index}.tar"));
        archive(&input, links);
        let output = dir.path().join(format!("output-{index}"));
        assert!(sdk::extract(&input, &output, true).is_err(), "{links:?}");
        assert!(!output.exists());
        assert_eq!(fs::read_dir(&outside).unwrap().count(), 0);
    }
}

#[test]
#[cfg(unix)]
fn link_policy_checks_intermediate_hops_even_when_the_final_file_is_inside() {
    use std::os::unix::fs::symlink;
    let dir = tempfile::tempdir().unwrap();
    let root = dir.path().join("bundle");
    fs::create_dir(&root).unwrap();
    fs::write(root.join("file"), "bytes").unwrap();
    symlink("file", root.join("one")).unwrap();
    symlink("one", root.join("two")).unwrap();
    sdk::check_tree(&root).unwrap();
    symlink("../bundle/file", root.join("escape-and-return")).unwrap();
    assert!(sdk::check_tree(&root).is_err());
    fs::remove_file(root.join("escape-and-return")).unwrap();
    symlink(".", root.join("directory-link")).unwrap();
    assert!(sdk::check_tree(&root).is_err());
}

fn generation(directory: &Path) {
    use utils::wson::Value;
    use wave_tools::{config, storage};
    for target in ["linux-riscv64", "linux-loong64"] {
        let mut record = sdk::identity(target).unwrap();
        let version = config::pin("LLVM_SOURCE_VERSION").unwrap();
        let name = format!("wave-llvm-{version}-{target}-r1.tar.xz");
        let archive = directory.join(&name);
        fs::write(&archive, format!("{target} immutable SDK")).unwrap();
        let digest = storage::digest(&archive).unwrap();
        fs::write(directory.join(format!("{name}.sha256")), format!("{digest}  {name}\n")).unwrap();
        for (key, value) in [
            ("filename", Value::string(&name)),
            ("revision", Value::string("r1")),
            (
                "url",
                Value::string(format!(
                    "https://wave-lang.dev/downloads/toolchains/llvm/{version}/r1/{name}"
                )),
            ),
            ("size_bytes", Value::integer(fs::metadata(&archive).unwrap().len())),
            ("sha256", Value::string(digest)),
        ] {
            config::field(&mut record, key, value).unwrap();
        }
        fs::write(directory.join(name.replace(".tar.xz", ".json")), config::json(&record).unwrap())
            .unwrap();
    }
}

#[test]
fn sdk_publication_verifies_both_architectures_before_activation_and_is_immutable() {
    let temporary = tempfile::tempdir().unwrap();
    let input = temporary.path().join("input");
    let output = temporary.path().join("website");
    fs::create_dir(&input).unwrap();
    generation(&input);
    let first = sdk::prepare_publication(&input, &output).unwrap();
    let catalog = fs::read(output.join("index.json")).unwrap();
    assert!(wave_tools::config::equivalent(
        &first,
        &sdk::prepare_publication(&input, &output).unwrap()
    ));
    assert_eq!(fs::read(output.join("index.json")).unwrap(), catalog);
    let file = wave_tools::storage::files(&input)
        .unwrap()
        .into_iter()
        .find(|p| p.to_string_lossy().ends_with(".tar.xz"))
        .unwrap();
    fs::write(&file, "changed source archive").unwrap();
    assert!(sdk::prepare_publication(&input, &output).is_err());
    assert_eq!(fs::read(output.join("index.json")).unwrap(), catalog);
    let fresh = temporary.path().join("fresh");
    fs::remove_file(input.join(format!(
        "wave-llvm-{}-linux-riscv64-r1.json",
        wave_tools::config::pin("LLVM_SOURCE_VERSION").unwrap()
    )))
    .unwrap();
    assert!(sdk::prepare_publication(&input, &fresh).is_err());
    assert!(!fresh.exists());
}

#[test]
fn archive_root_directory_and_dot_prefixes_do_not_discard_real_entries() {
    let work = tempfile::tempdir().unwrap();
    let input = work.path().join("root.tar");
    let mut builder = tar::Builder::new(File::create(&input).unwrap());
    let mut header = tar::Header::new_gnu();
    header.set_entry_type(tar::EntryType::Directory);
    header.set_mode(0o755);
    header.set_size(0);
    header.set_cksum();
    builder.append_data(&mut header, ".", std::io::empty()).unwrap();
    let mut header = tar::Header::new_gnu();
    header.set_mode(0o644);
    header.set_size(4);
    header.set_cksum();
    builder.append_data(&mut header, "./tree/file", b"DATA".as_slice()).unwrap();
    builder.finish().unwrap();
    drop(builder);
    let output = work.path().join("output");
    sdk::extract(&input, &output, false).unwrap();
    assert_eq!(fs::read(output.join("tree/file")).unwrap(), b"DATA");
}

#[test]
fn verified_cached_downloads_do_not_require_network_or_replace_the_file() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("reviewed-sdk.tar.xz");
    fs::write(&path, b"reviewed input").unwrap();
    let before = fs::metadata(&path).unwrap().modified().unwrap();
    let digest = wave_tools::storage::digest(&path).unwrap();
    wave_tools::sdk::download("https://invalid.invalid/not-requested", &path, &digest).unwrap();
    assert_eq!(fs::read(&path).unwrap(), b"reviewed input");
    assert_eq!(fs::metadata(&path).unwrap().modified().unwrap(), before);
}

// SPDX-License-Identifier: MPL-2.0
use std::collections::VecDeque;
use std::fs::File;
use std::io::{Read, Write};
use std::time::Duration;
use wave_tools::freebsd::Console;

struct Stream {
    fragments: VecDeque<Vec<u8>>,
    writes: Vec<u8>,
    echo: bool,
    fail: bool,
}
impl Read for Stream {
    fn read(&mut self, out: &mut [u8]) -> std::io::Result<usize> {
        if self.fail {
            return Err(std::io::Error::other("serial read failed"));
        }
        let Some(mut bytes) = self.fragments.pop_front() else {
            return Ok(0);
        };
        let count = bytes.len().min(out.len());
        out[..count].copy_from_slice(&bytes[..count]);
        bytes.drain(..count);
        if !bytes.is_empty() {
            self.fragments.push_front(bytes);
        }
        Ok(count)
    }
}
impl Write for Stream {
    fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        self.writes.extend_from_slice(bytes);
        if self.echo {
            self.fragments.push_back(bytes.to_vec());
        }
        Ok(bytes.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

#[test]
fn console_rejects_timeouts_that_cannot_form_a_deadline() {
    let directory = tempfile::tempdir().unwrap();
    let stream = Stream { fragments: VecDeque::new(), writes: vec![], echo: false, fail: false };

    assert!(Console::new(
        stream,
        File::create(directory.path().join("log")).unwrap(),
        Duration::MAX,
    )
    .is_err());
}

#[test]
fn console_requires_the_entire_newline_terminated_guest_key_and_retains_following_output() {
    let directory = tempfile::tempdir().unwrap();
    let stream = Stream {
        fragments: [
            b"boot\r\nssh-ed25519 AAAA".to_vec(),
            b"bbbb== guest\r".to_vec(),
            b"\nNEXT\n".to_vec(),
        ]
        .into(),
        writes: vec![],
        echo: false,
        fail: false,
    };
    let mut console = Console::new(
        stream,
        File::create(directory.path().join("log")).unwrap(),
        Duration::from_secs(1),
    )
    .unwrap();
    let result = console
        .expect(r"(?:^|\r?\n)(ssh-ed25519 [A-Za-z0-9+/=]+)(?:[ \t]+[^\r\n]*)?\r?\n", None)
        .unwrap();
    assert_eq!(result[1], "ssh-ed25519 AAAAbbbb==");
    assert_eq!(console.expect("NEXT", None).unwrap()[0], "NEXT");
}

#[test]
fn loader_waits_for_each_echo_and_handles_read_errors_and_eof() {
    let directory = tempfile::tempdir().unwrap();
    let stream = Stream { fragments: VecDeque::new(), writes: vec![], echo: true, fail: false };
    let mut console = Console::new(
        stream,
        File::create(directory.path().join("echo")).unwrap(),
        Duration::from_secs(1),
    )
    .unwrap();
    console.loader("set console=comconsole").unwrap();
    assert_eq!(console.expect("\r", None).unwrap()[0], "\r");
    for failure in [true, false] {
        let stream =
            Stream { fragments: VecDeque::new(), writes: vec![], echo: false, fail: failure };
        let mut console = Console::new(
            stream,
            File::create(directory.path().join("failure")).unwrap(),
            Duration::from_secs(1),
        )
        .unwrap();
        let error = console.expect("never", None).unwrap_err().to_string();
        assert!(error.contains(if failure { "serial read failed" } else { "console closed" }));
    }
}

#[test]
fn cache_outputs_identify_only_the_immutable_freebsd_inputs() {
    let directory = tempfile::tempdir().unwrap();
    let output = directory.path().join("outputs");
    let cache = directory.path().join("cache with spaces");
    let result = std::process::Command::new(env!("CARGO_BIN_EXE_wave-tools"))
        .args(["freebsd", "cache-key", "--github-output"])
        .arg(&output)
        .env("WAVE_FREEBSD_CACHE_DIR", &cache)
        .output()
        .unwrap();
    assert!(result.status.success(), "{}", String::from_utf8_lossy(&result.stderr));
    let contents = std::fs::read_to_string(output).unwrap();
    let fields: std::collections::BTreeMap<_, _> =
        contents.lines().map(|line| line.split_once('=').unwrap()).collect();
    assert_eq!(fields.len(), 3);
    assert_eq!(fields["path"], cache.to_str().unwrap());
    let prefix = fields["restore-prefix"];
    assert!(prefix.starts_with("wave-freebsd-inputs-v1-"));
    assert!(fields["key"].starts_with(prefix));
    assert_eq!(fields["key"], prefix);
}

#[test]
#[cfg(target_os = "linux")]
fn base_sysroot_expands_sparse_members_and_keeps_link_validation() {
    use std::io::{Seek, SeekFrom};
    use std::process::Command;
    let directory = tempfile::tempdir().unwrap();
    let fixture = directory.path().join("base");
    std::fs::create_dir_all(fixture.join("usr/lib/libxo/encoder")).unwrap();
    let mut file = File::create(fixture.join("usr/lib/sparse.so.1")).unwrap();
    file.set_len(1024 * 1024).unwrap();
    file.seek(SeekFrom::End(-4)).unwrap();
    file.write_all(b"TAIL").unwrap();
    drop(file);
    std::os::unix::fs::symlink("sparse.so.1", fixture.join("usr/lib/sparse.so")).unwrap();
    std::os::unix::fs::symlink(
        "/usr/tests/lib/plugin.so",
        fixture.join("usr/lib/libxo/encoder/test.enc"),
    )
    .unwrap();
    for unsafe_link in [false, true] {
        if unsafe_link {
            std::os::unix::fs::symlink("../../../outside", fixture.join("usr/lib/escape.so"))
                .unwrap();
        }
        let archive = directory.path().join(format!("base-{unsafe_link}.tar"));
        let status = Command::new("tar")
            .args(["--format=pax", "--sparse", "-cf"])
            .arg(&archive)
            .arg("-C")
            .arg(&fixture)
            .arg(".")
            .status()
            .unwrap();
        assert!(status.success());
        assert!(Command::new("xz").arg(&archive).status().unwrap().success());
        let output = directory.path().join(format!("sysroot-{unsafe_link}"));
        let result = Command::new(env!("CARGO_BIN_EXE_wave-tools"))
            .args(["freebsd", "build-sysroot", "--archive"])
            .arg(archive.with_extension("tar.xz"))
            .arg("--output")
            .arg(&output)
            .output()
            .unwrap();
        if unsafe_link {
            assert!(!result.status.success());
            assert!(!output.exists());
        } else {
            assert!(result.status.success(), "{}", String::from_utf8_lossy(&result.stderr));
            let bytes = std::fs::read(output.join("usr/lib/sparse.so")).unwrap();
            assert_eq!(bytes.len(), 1024 * 1024);
            assert_eq!(&bytes[bytes.len() - 4..], b"TAIL");
            assert!(bytes[..bytes.len() - 4].iter().all(|byte| *byte == 0));
            assert!(!output.join("usr/lib/libxo").exists());
        }
    }
}

#[test]
#[cfg(unix)]
fn vm_validation_rejects_stale_sources_and_changed_archives_before_starting_qemu() {
    let directory = tempfile::tempdir().unwrap();
    let work = directory.path();
    std::fs::create_dir(work.join("artifacts")).unwrap();
    std::fs::create_dir(work.join("inputs")).unwrap();
    let name = format!(
        "wave-v{}-x86_64-unknown-freebsd.tar.gz",
        wave_tools::config::compiler_version().unwrap()
    );
    let archive = work.join("inputs").join(&name);
    std::fs::write(&archive, b"archive bytes").unwrap();
    let source = std::process::Command::new("git")
        .args(["rev-parse", "HEAD"])
        .current_dir(wave_tools::root())
        .output()
        .unwrap();
    assert!(source.status.success());
    let source = String::from_utf8(source.stdout).unwrap();
    let digest = wave_tools::storage::digest(&archive).unwrap();
    for stale_source in [false, true] {
        let metadata = utils::wson::Value::object([
            (
                "source_sha",
                utils::wson::Value::string(if stale_source { "wrong" } else { source.trim() }),
            ),
            ("sha256", utils::wson::Value::string(if stale_source { &digest } else { "wrong" })),
            ("target", utils::wson::Value::string("x86_64-unknown-freebsd")),
        ]);
        std::fs::write(
            work.join("artifacts").join(format!("{name}.metadata.json")),
            wave_tools::config::json(&metadata).unwrap(),
        )
        .unwrap();
        let result = std::process::Command::new(env!("CARGO_BIN_EXE_wave-tools"))
            .args(["freebsd", "validate-package", "--work-dir"])
            .arg(work)
            .arg("--cache")
            .arg(work.join("cache"))
            .output()
            .unwrap();
        assert!(!result.status.success());
        assert!(String::from_utf8_lossy(&result.stderr).contains("source/target/checksum mismatch"));
        assert!(!work.join("inputs.iso").exists());
        assert!(!work.join("guest.qcow2").exists());
    }
}

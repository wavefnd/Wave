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
    );
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
    );
    console.loader("set console=comconsole").unwrap();
    assert_eq!(console.expect("\r", None).unwrap()[0], "\r");
    for failure in [true, false] {
        let stream =
            Stream { fragments: VecDeque::new(), writes: vec![], echo: false, fail: failure };
        let mut console = Console::new(
            stream,
            File::create(directory.path().join("failure")).unwrap(),
            Duration::from_secs(1),
        );
        let error = console.expect("never", None).unwrap_err().to_string();
        assert!(error.contains(if failure { "serial read failed" } else { "console closed" }));
    }
}

#[test]
fn cache_outputs_preserve_the_restore_prefix_key_and_full_identity() {
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
    assert!(prefix.starts_with("wave-freebsd-v2-"));
    assert!(fields["key"].starts_with(prefix));
    assert_eq!(fields["key"].len(), prefix.len() + 40);
}

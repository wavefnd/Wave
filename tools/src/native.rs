// SPDX-License-Identifier: MPL-2.0
//! Native fixture ownership and host-boundary assertions shared by Cargo and CI.
use crate::{
    cli::Args,
    error,
    process::{Output, Spec},
    Result,
};
#[cfg(unix)]
use crate::{package, storage};
use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;

struct Harness {
    work: tempfile::TempDir,
    compiler: PathBuf,
    target: String,
    binaries: std::collections::BTreeMap<String, PathBuf>,
}

impl Harness {
    fn new(compiler: PathBuf, target: String) -> Result<Self> {
        Ok(Self {
            work: tempfile::Builder::new().prefix("wave-native-").tempdir()?,
            compiler,
            target,
            binaries: Default::default(),
        })
    }

    fn compile(&self, source: &Path, output: &Path, opt: &str, object: bool) -> Result<()> {
        let mut spec = Spec::new([
            self.compiler.as_os_str(),
            "--std-root".as_ref(),
            crate::root().join("std").as_os_str(),
            "build".as_ref(),
            source.as_os_str(),
            "--target".as_ref(),
            self.target.as_ref(),
            opt.as_ref(),
            "-o".as_ref(),
            output.as_os_str(),
        ]);
        if object {
            spec.args.push("--emit=obj".into());
        }
        spec.timeout = Duration::from_secs(60);
        self.checked(spec, 0)?;
        Ok(())
    }

    fn checked(&self, spec: Spec, expected: i32) -> Result<Output> {
        let output = spec.run()?;
        if output.status != expected || output.failure.is_some() {
            return Err(error(format!(
                "{} target={}: {} exit {}, expected {expected}\n{}\n{}",
                Path::new(&spec.args[0]).display(),
                self.target,
                output.failure.as_deref().unwrap_or("failed"),
                output.status,
                output.text_stdout(),
                output.text_stderr()
            )));
        }
        Ok(output)
    }

    fn build(&mut self, name: &str) -> Result<PathBuf> {
        if let Some(path) = self.binaries.get(name) {
            return Ok(path.clone());
        }
        let output = self.work.path().join(format!("{name}{}", std::env::consts::EXE_SUFFIX));
        self.compile(
            &crate::root().join(format!("tests/fixtures/io/{name}.wave")),
            &output,
            "-O0",
            false,
        )?;
        self.binaries.insert(name.into(), output.clone());
        Ok(output)
    }

    fn run(&mut self, name: &str, directory: &Path, low_fd: bool) -> Result<String> {
        let binary = self.build(name)?;
        let mut spec = Spec::new([binary]);
        spec.cwd = directory.into();
        spec.timeout = Duration::from_secs(10);
        if cfg!(target_os = "linux") && low_fd {
            spec.nofile = Some(64);
        }
        Ok(self.checked(spec, 0)?.strict_stdout()?.trim().into())
    }

    fn source(&self, source: &str, descriptors: &[i32]) -> Result<()> {
        let directory = tempfile::tempdir_in(self.work.path())?;
        let path = directory.path().join("main.wave");
        fs::write(&path, source)?;
        self.both(&path, descriptors)
    }

    fn both(&self, source: &Path, descriptors: &[i32]) -> Result<()> {
        let directory = tempfile::tempdir_in(self.work.path())?;
        for opt in ["-O0", "-O2"] {
            let binary = directory.path().join(format!("case{}", std::env::consts::EXE_SUFFIX));
            self.compile(source, &binary, opt, false)?;
            let mut spec = Spec::new([binary]);
            spec.cwd = directory.path().into();
            spec.pass_fds = descriptors.to_vec();
            spec.timeout = Duration::from_secs(10);
            self.checked(spec, 0)?;
        }
        Ok(())
    }

    fn io(&mut self) -> Result<()> {
        let case = tempfile::tempdir_in(self.work.path())?;
        let directory = case.path();
        let input = directory.join("source.bin");
        let output = directory.join("destination.bin");
        let data: Vec<u8> = (0..97).collect();
        fs::write(&output, b"untouched")?;
        fs::write(directory.join("existing.bin"), b"preserve me")?;
        self.run("invalid_buffers", directory, false)?;
        require(
            fs::read(&output)? == b"untouched"
                && fs::read(directory.join("existing.bin"))? == b"preserve me",
            "invalid buffer modified existing data",
        )?;
        require(
            !directory.join("missing-write.bin").exists()
                && !directory.join("missing-append.bin").exists()
                && fs::read(directory.join("zero-truncate.bin"))?.is_empty(),
            "invalid buffer created output or zero-write contract changed",
        )?;
        fs::write(&input, &data)?;
        require(
            self.run("copy_self", directory, true)? == "-4096" && fs::read(&input)? == data,
            "self-copy changed input",
        )?;
        fs::remove_file(&output)?;
        fs::hard_link(&input, &output)?;
        require(
            self.run("copy", directory, true)? == "-4096" && fs::read(&input)? == data,
            "hard-link copy changed input",
        )?;
        fs::remove_file(&output)?;
        #[cfg(unix)]
        {
            std::os::unix::fs::symlink(&input, &output)?;
            require(
                self.run("copy", directory, false)? == "-4096" && fs::read(&input)? == data,
                "symlink copy changed input",
            )?;
            fs::remove_file(&output)?;
        }
        fs::write(&output, vec![b'x'; 300])?;
        require(
            self.run("copy", directory, true)? == "97" && fs::read(&output)? == data,
            "copy did not replace destination",
        )?;
        fs::remove_file(&output)?;
        fs::write(&input, b"new content")?;
        require(
            self.run("copy", directory, false)? == "11" && fs::read(&output)? == b"new content",
            "copy did not create destination",
        )?;
        fs::remove_file(&input)?;
        fs::write(&output, b"untouched")?;
        require(
            self.run("copy", directory, false)?.parse::<i64>()? < 0
                && fs::read(&output)? == b"untouched",
            "missing source modified destination",
        )?;
        for length in [0, 1, 127, 128, 129] {
            let data: Vec<u8> = (0..length).map(|v| v as u8).collect();
            fs::write(&input, &data)?;
            let result = self.run("read_all", directory, false)?;
            let mut fields = result.split_whitespace();
            let count = fields.next().ok_or_else(|| error("missing read count"))?.parse::<i64>()?;
            if length > 128 {
                require(count == -4098, "oversized read did not reject")?;
            } else {
                let sum =
                    data.iter().enumerate().map(|(i, v)| (i + 1) * (*v as usize)).sum::<usize>();
                require(
                    count == length as i64 && fields.next().unwrap_or("").parse::<usize>()? == sum,
                    "read count/checksum mismatch",
                )?;
            }
        }
        fs::write(&input, [])?;
        require(self.run("read_zero", directory, false)? == "0", "empty zero-capacity read")?;
        fs::write(&input, b"x")?;
        require(
            self.run("read_zero", directory, false)? == "-4098",
            "nonempty zero-capacity read",
        )?;
        #[cfg(target_os = "linux")]
        {
            fs::remove_file(&input)?;
            std::os::unix::fs::symlink("/proc/self/status", &input)?;
            require(
                fs::metadata("/proc/self/status")?.len() == 0
                    && self.run("read_all", directory, false)?.split_whitespace().next()
                        == Some("-4098"),
                "zero-stat virtual file not read",
            )?;
            fs::remove_file(&input)?;
            std::os::unix::fs::symlink("/proc/self/comm", &input)?;
            let expected = b"read_all\n";
            let sum =
                expected.iter().enumerate().map(|(i, v)| (i + 1) * (*v as usize)).sum::<usize>();
            require(
                self.run("read_all", directory, false)? == format!("{} {sum}", expected.len()),
                "virtual file contents mismatch",
            )?;
            fs::remove_file(&input)?;
        }
        #[cfg(unix)]
        self.fifo(directory)?;
        for name in ["tcp_timeout", "tcp_read"] {
            self.run(name, directory, false)?;
        }
        #[cfg(unix)]
        {
            self.run("fork_status", directory, false)?;
            self.capture(directory)?;
        }
        #[cfg(target_os = "linux")]
        self.linux_c(directory)?;
        for source in ["std_boundaries/paths.wave", "stabilization_17/tcp_zero.wave"] {
            self.both(&crate::root().join("tests/fixtures").join(source), &[])?;
        }
        #[cfg(unix)]
        self.both(&crate::root().join("tests/fixtures/std_boundaries/poll.wave"), &[])?;
        #[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd"))]
        self.both(&crate::root().join("tests/fixtures/io/tcp_fd_bounds.wave"), &[])?;
        self.sockets()?;
        println!("PASS native I/O and socket boundary fixtures ({})", self.target);
        Ok(())
    }

    #[cfg(unix)]
    fn fifo(&mut self, directory: &Path) -> Result<()> {
        use nix::fcntl::{open, OFlag};
        use nix::sys::stat::Mode;
        use std::io::Write;
        use std::sync::{
            atomic::{AtomicBool, Ordering},
            Arc,
        };
        let path = directory.join("source.bin");
        if storage::exists(&path) {
            fs::remove_file(&path)?;
        }
        nix::unistd::mkfifo(&path, Mode::S_IRUSR | Mode::S_IWUSR)?;
        let stop = Arc::new(AtomicBool::new(false));
        let stopped = stop.clone();
        let task = std::thread::spawn(move || -> Result<()> {
            let started = std::time::Instant::now();
            let fd = loop {
                if stopped.load(Ordering::SeqCst) {
                    return Ok(());
                }
                match open(&path, OFlag::O_WRONLY | OFlag::O_NONBLOCK, Mode::empty()) {
                    Ok(fd) => break fd,
                    Err(nix::errno::Errno::ENXIO) if started.elapsed() < Duration::from_secs(5) => {
                        std::thread::sleep(Duration::from_millis(5))
                    },
                    Err(e) => return Err(e.into()),
                }
            };
            let mut file = fs::File::from(fd);
            for piece in [b"abc".as_slice(), b"defgh", b"ijk"] {
                file.write_all(piece)?;
                std::thread::sleep(Duration::from_millis(30));
            }
            Ok(())
        });
        let result = self.run("read_all", directory, false);
        stop.store(true, Ordering::SeqCst);
        task.join().map_err(|_| error("FIFO producer panicked"))??;
        require(result? == "11 6842", "stream read did not accumulate all chunks")?;
        Ok(())
    }

    #[cfg(unix)]
    fn capture(&mut self, directory: &Path) -> Result<()> {
        use std::os::unix::fs::PermissionsExt;
        let child = directory.join("capture-child");
        let tool = std::env::current_exe()?;
        let tool = tool
            .to_str()
            .ok_or_else(|| error("native helper needs UTF-8 executable path"))?
            .replace('\'', "'\\''");
        for (mode, fixture) in [("reader-check", "capture"), ("writer", "capture_close")] {
            fs::write(&child, format!("#!/bin/sh\nexec '{tool}' __fixture-child {mode}\n"))?;
            fs::set_permissions(&child, fs::Permissions::from_mode(0o700))?;
            self.run(fixture, directory, true)?;
            if mode == "reader-check" {
                let binary = self.build(fixture)?;
                let mut spec = Spec::new([binary]);
                spec.cwd = directory.into();
                spec.close_fds = vec![0, 1];
                spec.timeout = Duration::from_secs(10);
                self.checked(spec, 0)?;
            }
        }
        #[cfg(target_os = "linux")]
        {
            self.run("capture_failures", directory, true)?;
            if !nix::unistd::Uid::effective().is_root() {
                let binary = self.build("capture_fork_failure")?;
                let mut spec = Spec::new([binary]);
                spec.cwd = directory.into();
                spec.nproc = Some(0);
                spec.timeout = Duration::from_secs(10);
                self.checked(spec, 0)?;
            } else {
                println!("SKIP fork-failure fixture: root is exempt from RLIMIT_NPROC");
            }
        }
        Ok(())
    }

    #[cfg(target_os = "linux")]
    fn linux_c(&self, directory: &Path) -> Result<()> {
        let cc = package::which("clang-21")
            .or_else(|| package::which("clang"))
            .or_else(|| package::which("cc"))
            .ok_or_else(|| error("C compiler required for native thread fixtures"))?;
        for name in ["exit_group", "tcp_interrupt"] {
            let object = directory.join(format!("{name}.o"));
            let executable = directory.join(format!("{name}-host"));
            self.compile(
                &crate::root().join(format!("tests/fixtures/io/{name}.wave")),
                &object,
                "-O0",
                true,
            )?;
            self.checked(
                Spec::new([
                    cc.as_os_str(),
                    "-pthread".as_ref(),
                    crate::root().join(format!("tests/fixtures/io/{name}.c")).as_os_str(),
                    object.as_os_str(),
                    "-o".as_ref(),
                    executable.as_os_str(),
                ]),
                0,
            )?;
            for args in if name == "exit_group" {
                vec![vec![], vec![OsString::from("worker")]]
            } else {
                vec![vec![]]
            } {
                let mut command = vec![executable.clone().into_os_string()];
                command.extend(args);
                let mut spec = Spec::new(command);
                spec.timeout = Duration::from_secs(10);
                self.checked(spec, if name == "exit_group" { 37 } else { 0 })?;
            }
        }
        Ok(())
    }

    fn sockets(&self) -> Result<()> {
        self.source(UDP_DATAGRAM_COMPLETENESS, &[])?;
        self.source(ADDRESS_FAMILY_FAILURE_PRESERVES_ADDRESS_AND_NATIVE_ERRORS, &[])?;
        let bsd = cfg!(any(target_os = "macos", target_os = "freebsd"));
        let timeout = if cfg!(windows) {
            10060
        } else if bsd {
            60
        } else {
            110
        };
        let overflow = if bsd { 84 } else { 75 };
        let pending = if cfg!(windows) {
            -10036
        } else if bsd {
            -36
        } else {
            -115
        };
        let not_pending = if cfg!(windows) || bsd { -115 } else { -36 };
        let source = SELECTED_ERROR_DOMAINS_MATCH_TIMEOUT_AND_CONNECT_CLASSIFICATION
            .replace("@TIMEOUT@", &timeout.to_string())
            .replace("@OVERFLOW@", &overflow.to_string())
            .replace("@PENDING@", &pending.to_string())
            .replace("@NOT_PENDING@", &not_pending.to_string());
        self.source(&source, &[])?;
        #[cfg(unix)]
        self.unix_sockets()?;
        Ok(())
    }

    #[cfg(unix)]
    fn unix_sockets(&self) -> Result<()> {
        use std::io::{Read, Write};
        use std::os::fd::AsRawFd;
        use std::os::unix::net::UnixStream;
        let (mut left, mut right) = UnixStream::pair()?;
        let fd = left.as_raw_fd();
        let source = |s: &str| s.replace("@FD@", &fd.to_string());
        self.source(
            &source(EMPTY_READS_EXACT_PEEK_REJECTION_AND_ORDINARY_PEEK_PRESERVE_DATA),
            &[fd],
        )?;
        right.write_all(b"ab")?;
        self.source(
            &source(EMPTY_READS_EXACT_PEEK_REJECTION_AND_ORDINARY_PEEK_PRESERVE_DATA_1),
            &[fd],
        )?;
        let mut bytes = [0; 2];
        left.read_exact(&mut bytes)?;
        require(bytes == *b"ab", "peek consumed queued bytes")?;
        self.source(
            &source(OVERSIZED_DESCRIPTORS_CANNOT_MUTATE_WRITE_OR_CLOSE_THE_REAL_SOCKET),
            &[fd],
        )?;
        require(
            !nix::fcntl::OFlag::from_bits_truncate(nix::fcntl::fcntl(
                &left,
                nix::fcntl::FcntlArg::F_GETFL,
            )?)
            .contains(nix::fcntl::OFlag::O_NONBLOCK),
            "oversized socket descriptor changed mode",
        )?;
        right.set_read_timeout(Some(Duration::from_millis(50)))?;
        match right.read(&mut bytes) {
            Err(e)
                if matches!(
                    e.kind(),
                    std::io::ErrorKind::WouldBlock | std::io::ErrorKind::TimedOut
                ) => {},
            other => return Err(error(format!("invalid socket operation wrote data: {other:?}"))),
        }
        self.source(INVALID_PID_VALUES_DO_NOT_ALIAS_PROCESSES_OR_CHANGE_WAIT_STATUS, &[])?;
        self.source(&source(INVALID_ASYNC_TIMEOUT_DOES_NOT_CHANGE_SOCKET_MODE), &[fd])?;
        require(
            !nix::fcntl::OFlag::from_bits_truncate(nix::fcntl::fcntl(
                &left,
                nix::fcntl::FcntlArg::F_GETFL,
            )?)
            .contains(nix::fcntl::OFlag::O_NONBLOCK),
            "invalid timeout changed socket mode",
        )?;
        let pty = nix::pty::openpty(None, None)?;
        let initial = nix::sys::termios::tcgetattr(&pty.slave)?;
        self.source(
            &INVALID_TERMINAL_ACTIONS_PRESERVE_PTY_SETTINGS
                .replace("@SLAVE@", &pty.slave.as_raw_fd().to_string()),
            &[pty.slave.as_raw_fd()],
        )?;
        require(
            nix::sys::termios::tcgetattr(&pty.slave)? == initial,
            "invalid terminal action changed settings",
        )?;
        Ok(())
    }

    #[cfg(unix)]
    fn processes(&self) -> Result<()> {
        use std::io::{Read, Write};
        use std::os::fd::AsRawFd;
        let (read, write) = nix::unistd::pipe()?;
        let mut read = fs::File::from(read);
        let mut write = fs::File::from(write);
        write.write_all(b"ZZ")?;
        let read_fd = read.as_raw_fd();
        let write_fd = write.as_raw_fd();
        let mut source = WIDE_DESCRIPTORS.to_string();
        for (key, value) in [
            ("BAD_READ", 4294967296 + read_fd as i64),
            ("BAD_WRITE", 4294967296 + write_fd as i64),
            ("NEGATIVE_ALIAS", -4294967296 + read_fd as i64),
            ("READ_FD", read_fd as i64),
            ("WRITE_FD", write_fd as i64),
        ] {
            source = source.replace(key, &value.to_string());
        }
        self.source(&source, &[read_fd, write_fd])?;
        drop(write);
        let mut bytes = Vec::new();
        read.read_to_end(&mut bytes)?;
        require(bytes == b"AA", "wide descriptor test changed pipe bytes")?;
        let code = r#"#include <sys/wait.h>
#include <unistd.h>
#include <signal.h>
#include <stdio.h>
int main(void) {
    pid_t child = fork();
    if (child < 0) { return 1; }
    if (child == 0) { for (;;) { pause(); } }
    int stopped = 0, continued = 0;
    if (kill(child, SIGSTOP) || waitpid(child, &stopped, WUNTRACED) != child || !WIFSTOPPED(stopped)) { kill(child, SIGKILL); waitpid(child, 0, 0); return 2; }
    if (kill(child, SIGCONT) || waitpid(child, &continued, WCONTINUED) != child || !WIFCONTINUED(continued)) { kill(child, SIGKILL); waitpid(child, 0, 0); return 3; }
    kill(child, SIGTERM);
    waitpid(child, 0, 0);
    printf("%d %d\n", stopped, continued);
    return 0;
}
"#;
        let cc = package::which("clang-21")
            .or_else(|| package::which("clang"))
            .or_else(|| package::which("cc"))
            .ok_or_else(|| error("C compiler required for raw wait-status fixture"))?;
        let file = self.work.path().join("wait.c");
        let binary = self.work.path().join("wait");
        fs::write(&file, code)?;
        self.checked(
            Spec::new([cc.as_os_str(), file.as_os_str(), "-o".as_ref(), binary.as_os_str()]),
            0,
        )?;
        let mut spec = Spec::new([binary]);
        spec.timeout = Duration::from_secs(5);
        let output = self.checked(spec, 0)?.strict_stdout()?;
        let mut words = output.split_whitespace();
        let stopped = words.next().ok_or_else(|| error("missing stopped word"))?;
        let continued = words.next().ok_or_else(|| error("missing continued word"))?;
        stopped.parse::<i32>()?;
        continued.parse::<i32>()?;
        self.source(&WAIT_WORDS.replace("CONTINUED", continued).replace("STOPPED", stopped), &[])?;
        println!("PASS native process boundary fixtures ({})", self.target);
        Ok(())
    }
}

fn require(condition: bool, message: &str) -> Result<()> {
    if condition {
        Ok(())
    } else {
        Err(error(message))
    }
}

pub fn main(arguments: &[OsString]) -> Result<i32> {
    let mut args = Args::new(arguments);
    let compiler = crate::runner::compiler(args.path("--wavec")?)?;
    let target = args
        .text("--target")?
        .unwrap_or_else(|| std::env::var("WAVE_TEST_TARGET").unwrap_or_default());
    let process = args.flag("--process-only");
    args.finish()?;
    let target = if target.is_empty() {
        Spec::new([compiler.as_os_str(), "print".as_ref(), "default-target".as_ref()])
            .checked()?
            .strict_stdout()?
            .trim()
            .to_string()
    } else {
        target
    };
    let mut harness = Harness::new(compiler, target)?;
    if process {
        #[cfg(unix)]
        harness.processes()?;
        #[cfg(not(unix))]
        return Err(error("POSIX process fixture requires a Unix host"));
    } else {
        harness.io()?;
    }
    Ok(0)
}

/// A native capture child replaces the former generated Python programs.
pub fn child(arguments: &[OsString]) -> Result<i32> {
    #[cfg(unix)]
    {
        use std::io::Write;
        use std::os::unix::fs::{FileTypeExt, MetadataExt};
        if arguments == [OsString::from("writer")] {
            let data = vec![b'x'; 65536];
            loop {
                if let Err(e) = std::io::stdout().write_all(&data) {
                    return if e.kind() == std::io::ErrorKind::BrokenPipe {
                        Ok(0)
                    } else {
                        Err(e.into())
                    };
                }
            }
        }
        if arguments != [OsString::from("reader-check")] {
            return Err(error("unknown fixture child"));
        }
        let root = if cfg!(target_os = "linux") {
            Path::new("/proc/self/fd")
        } else {
            Path::new("/dev/fd")
        };
        let output = fs::metadata(root.join("1"))?;
        for entry in fs::read_dir(root)? {
            let entry = entry?;
            let Some(fd) = entry.file_name().to_str().and_then(|v| v.parse::<i32>().ok()) else {
                continue;
            };
            let Ok(info) = fs::metadata(entry.path()) else {
                continue;
            };
            if info.file_type().is_fifo()
                && info.dev() == output.dev()
                && info.ino() == output.ino()
            {
                // This descriptor was just observed open and this helper has one thread.
                let descriptor = unsafe { std::os::fd::BorrowedFd::borrow_raw(fd) };
                let flags = nix::fcntl::fcntl(descriptor, nix::fcntl::FcntlArg::F_GETFL)?;
                if flags & nix::fcntl::OFlag::O_ACCMODE.bits() == nix::fcntl::OFlag::O_RDONLY.bits()
                {
                    return Ok(42);
                }
            }
        }
        std::io::stdout().write_all(b"OK")?;
        Ok(0)
    }
    #[cfg(not(unix))]
    {
        let _ = arguments;
        Err(error("capture child requires Unix"))
    }
}

pub const EMPTY_READS_EXACT_PEEK_REJECTION_AND_ORDINARY_PEEK_PRESERVE_DATA: &str = r###"import("std::net::unix")::{unix_read, UnixStream};
import("std::net::error")::{NetIoResult};
fun main() -> i32 {
    var result: NetIoResult = unix_read(UnixStream { fd: @FD@ }, null, 0);
    if (result.count != 0 || result.eof || result.error.kind != 0) { return 1; }
    return 0;
}"###;

pub const EMPTY_READS_EXACT_PEEK_REJECTION_AND_ORDINARY_PEEK_PRESERVE_DATA_1: &str = r###"import("std::net::socket_base")::{net_recv_exact, net_recv_exact_io, net_recv_once};
import("std::net::error")::{NetIoResult};
fun main() -> i32 {
    var bytes: array<u8, 4> = [11, 22, 33, 44];
    if (net_recv_exact(@FD@, &bytes[0], 4, 2) != -22) { return 1; }
    var result: NetIoResult = net_recv_exact_io(@FD@, &bytes[0], 4, 2);
    if (result.count != 0 || result.error.native_code != -22) { return 2; }
    if (bytes[0] != 11 || bytes[1] != 22 || bytes[2] != 33 || bytes[3] != 44) { return 3; }
    if (net_recv_once(@FD@, &bytes[0], 2, 2) != 2 || bytes[0] != 97 || bytes[1] != 98) { return 4; }
    return 0;
}"###;

pub const OVERSIZED_DESCRIPTORS_CANNOT_MUTATE_WRITE_OR_CLOSE_THE_REAL_SOCKET: &str = r###"import("std::net::socket_base")::{net_fd_valid, net_send_once, net_close, net_shutdown};
import("std::net::socketopt")::{net_set_nonblock};
fun main() -> i32 {
    var bad: i64 = @FD@ + 4294967296;
    var payload: u8 = 7;
    if (net_fd_valid(bad) || net_set_nonblock(bad, 1) != -22) { return 1; }
    if (net_send_once(bad, &payload, 1, 0) != -22 || net_close(bad) != -22 || net_shutdown(bad, 2) != -22) { return 2; }
    return 0;
}"###;

pub const ADDRESS_FAMILY_FAILURE_PRESERVES_ADDRESS_AND_NATIVE_ERRORS: &str = r###"import("std::net::socket_base")::{net_socket_udp_v4, net_bind_socket_v4, net_local_addr_v6, net_local_addr_v4, net_close};
import("std::net::address")::{SocketAddrV6, SocketAddrV4, socket_addr_v6_any, socket_addr_v4_any, socket_addr_v4_loopback};
fun main() -> i32 {
    var fd: i64 = net_socket_udp_v4();
    if (fd < 0) { return 1; }
    var local: SocketAddrV4 = socket_addr_v4_loopback(0);
    if (net_bind_socket_v4(fd, local) != 0) { net_close(fd); return 4; }
    var wrong: SocketAddrV6 = socket_addr_v6_any(12345);
    if (net_local_addr_v6(fd, &wrong) != -22 || wrong.port != 12345) { net_close(fd); return 2; }
    var valid: SocketAddrV4 = socket_addr_v4_any(0);
    if (net_local_addr_v4(fd, &valid) != 0 || valid.port == 0) { net_close(fd); return 3; }
    return net_close(fd) as i32;
}"###;

pub const INVALID_PID_VALUES_DO_NOT_ALIAS_PROCESSES_OR_CHANGE_WAIT_STATUS: &str = r###"import("std::process::core")::{proc_getpid, proc_kill, proc_waitpid_raw};
fun main() -> i32 {
    var pid: i64 = proc_getpid();
    if (proc_kill(pid, 0) != 0 || proc_kill(pid + 4294967296, 0) != -22 || proc_kill(pid - 4294967296, 0) != -22) { return 1; }
    var status: i32 = 42;
    if (proc_waitpid_raw(4294967296, &status, 1) != -22 || status != 42) { return 2; }
    return 0;
}"###;

pub const INVALID_ASYNC_TIMEOUT_DOES_NOT_CHANGE_SOCKET_MODE: &str = r###"import("std::task" as task);
import("std::task::net" as net);
import("std::net::error")::{NetIoResult};
fun main() -> i32 {
    var payload: u8 = 7;
    var result: NetIoResult = task::block_on(net::write_all(@FD@, &payload, 1, -2));
    task::shutdown();
    if (result.error.native_code != -22) { return 1; }
    return 0;
}"###;

pub const SELECTED_ERROR_DOMAINS_MATCH_TIMEOUT_AND_CONNECT_CLASSIFICATION: &str = r###"import("std::net::error")::{NET_NATIVE_TIMEOUT, NET_ERROR_TIMED_OUT, NetError, net_error_from_native, net_connect_pending};
import("std::sys::errors")::{SYS_ERR_OVERFLOW};
fun main() -> i32 {
    if (NET_NATIVE_TIMEOUT != -@TIMEOUT@ || SYS_ERR_OVERFLOW != -@OVERFLOW@) { return 1; }
    var error: NetError = net_error_from_native(NET_NATIVE_TIMEOUT);
    if (error.kind != NET_ERROR_TIMED_OUT || !net_connect_pending(@PENDING@) || net_connect_pending(@NOT_PENDING@)) { return 2; }
    return 0;
}"###;

pub const WIDE_DESCRIPTORS: &str = r###"import("std::io::fd")::{io_read, io_write, io_close, io_seek, io_dup, io_dup2, io_write_all};
fun main() -> i32 {
    var octet: u8 = 65;
    var descriptors: array<i64, 6> = [BAD_READ, BAD_WRITE, NEGATIVE_ALIAS, 2147483647, 2147483648, -2147483649];
    var i: i32 = 0;
    while (i < 6) {
        var fd: i64 = descriptors[i];
        if (io_read(fd, &octet, 1) != -9 || octet != 65) { return 1; }
        if (io_write(fd, &octet, 1) != -9) { return 2; }
        if (io_seek(fd, 0, 0) != -9 || io_close(fd) != -9 || io_dup(fd) != -9) { return 3; }
        if (io_dup2(fd, WRITE_FD) != -9 || io_dup2(READ_FD, fd) != -9) { return 4; }
        if (io_write_all(fd, &octet, 0) != 0) { return 5; }
        i += 1;
    }
    if (io_read(READ_FD, &octet, 1) != 1 || octet != 90) { return 6; }
    octet = 65;
    if (io_write(WRITE_FD, &octet, 1) != 1) { return 7; }
    return 0;
}"###;

pub const WAIT_WORDS: &str = r###"import("std::process::wait")::{proc_status_continued, proc_status_stopped, proc_status_signaled, proc_status_exited, proc_status_exit_code, proc_status_term_signal};
fun main() -> i32 {
    if (!proc_status_continued(CONTINUED) || proc_status_stopped(CONTINUED) || proc_status_signaled(CONTINUED)) { return 1; }
    if (!proc_status_stopped(STOPPED) || proc_status_continued(STOPPED) || proc_status_signaled(STOPPED)) { return 2; }
    if (!proc_status_exited(1792) || proc_status_exit_code(1792) != 7) { return 3; }
    if (!proc_status_signaled(9) || proc_status_term_signal(9) != 9) { return 4; }
    return 0;
}"###;

pub const INVALID_TERMINAL_ACTIONS_PRESERVE_PTY_SETTINGS: &str = r###"import("std::sys::tty")::{Termios, tty_getattr, tty_setattr, TTY_ECHO, TTY_TCSANOW, TTY_TCSADRAIN, TTY_TCSAFLUSH};
fun main() -> i32 {
    var term: Termios;
    if (tty_getattr(@SLAVE@, &term) != 0) { return 1; }
    term.c_lflag = term.c_lflag ^ TTY_ECHO;
    if (tty_setattr(@SLAVE@, -1, &term) != -22 || tty_setattr(@SLAVE@, 99, &term) != -22) { return 2; }
    var status: i64 = tty_getattr(@SLAVE@, &term);
    if (status != 0) { println("second tty_getattr: {}", status); return 3; }
    if (tty_setattr(@SLAVE@, TTY_TCSANOW, &term) != 0 || tty_setattr(@SLAVE@, TTY_TCSADRAIN, &term) != 0 || tty_setattr(@SLAVE@, TTY_TCSAFLUSH, &term) != 0) { return 4; }
    return 0;
}"###;

/// Deterministic process-tree peers run on every tooling host without Python.
pub fn process_child(arguments: &[OsString]) -> Result<i32> {
    use std::io::Write;
    let (mode, rest) =
        arguments.split_first().ok_or_else(|| error("missing process fixture mode"))?;
    if mode == "bytes" {
        std::io::stdout().write_all(&[255])?;
        std::io::stderr().write_all(b"error")?;
        return Ok(7);
    }
    if mode == "flood" {
        let bytes = vec![b'x'; 65536];
        loop {
            std::io::stdout().write_all(&bytes)?;
            std::io::stderr().write_all(&bytes)?;
        }
    }
    if mode == "mark-later" {
        let path = rest.first().ok_or_else(|| error("missing marker path"))?;
        std::thread::sleep(Duration::from_secs(2));
        fs::write(Path::new(path), b"escaped")?;
        return Ok(0);
    }
    if mode == "spawn-wait" || mode == "spawn-exit" {
        let path = rest.first().ok_or_else(|| error("missing marker path"))?;
        let mut child = std::process::Command::new(std::env::current_exe()?)
            .args([OsString::from("__fixture-process"), "mark-later".into(), path.clone()])
            .spawn()?;
        std::io::stdout().write_all(b"READY\n")?;
        std::io::stdout().flush()?;
        if mode == "spawn-exit" {
            return Ok(0);
        }
        return Ok(child.wait()?.code().unwrap_or(1));
    }
    Err(error("unknown process fixture mode"))
}

pub const UDP_DATAGRAM_COMPLETENESS: &str = r###"
import("std::net::udp")::{UdpSocket, udp_bind_loopback, udp_local_addr_v4, udp_send_to, udp_recv_datagram, udp_recv_vectored_datagram, udp_close};
import("std::net::vectored")::{NetIoSlice};
import("std::net::error")::{NetDatagramResult, NetResult, NetIoResult};
import("std::net::address")::{SocketAddrV4};

fun main() -> i32 {
    var bound: NetResult<UdpSocket> = udp_bind_loopback(0);
    var sender: NetResult<UdpSocket> = udp_bind_loopback(0);
    if (!bound.ok || !sender.ok) { return 1; }
    var local: NetResult<SocketAddrV4> = udp_local_addr_v4(bound.value);
    if (!local.ok) { return 2; }
    var data: array<u8, 8> = [1, 2, 3, 4, 5, 6, 7, 99];
    var lengths: array<i64, 4> = [0, 1, 4, 7];
    var mode: i32 = 0;
    while (mode < 2) {
        var index: i32 = 0;
        while (index < 4) {
            var length: i64 = lengths[index];
            var sent: NetIoResult = udp_send_to(sender.value, local.value, &data[0], length);
            if (sent.error.native_code != 0 || sent.count != length) { return 3; }
            var buffer: array<u8, 5> = [91, 91, 91, 91, 92];
            // Argument rejection must leave the queued datagram untouched.
            var rejected: NetDatagramResult = udp_recv_datagram(bound.value, null, 1);
            if (rejected.error.native_code == 0 || buffer[0] != 91) { return 4; }
            var result: NetDatagramResult;
            if (mode == 0) {
                result = udp_recv_datagram(bound.value, &buffer[0], 4);
            } else {
                var slices: array<NetIoSlice, 2>;
                slices[0] = NetIoSlice { data: &buffer[0], length: 2 };
                slices[1] = NetIoSlice { data: &buffer[2], length: 2 };
                result = udp_recv_vectored_datagram(bound.value, &slices[0], 2);
            }
            var expected: i64 = length;
            if (expected > 4) { expected = 4; }
            if (result.error.native_code != 0 || result.count != expected || result.truncated != (length > 4)
                || (result.original_size_known && result.original_size != length) || buffer[4] != 92) { return 5; }
            var byte_index: i64 = 0;
            while (byte_index < expected) {
                if (buffer[byte_index] != data[byte_index]) { return 6; }
                byte_index += 1;
            }
            if (expected < 4 && buffer[expected] != 91) { return 7; }
            sent = udp_send_to(sender.value, local.value, &data[7], 1);
            if (sent.error.native_code != 0) { return 8; }
            result = udp_recv_datagram(bound.value, &buffer[0], 4);
            if (result.error.native_code != 0 || result.count != 1 || result.truncated || buffer[0] != 99) { return 9; }
            index += 1;
        }
        mode += 1;
    }
    var sent: NetIoResult = udp_send_to(sender.value, local.value, &data[0], 7);
    if (sent.error.native_code != 0) { return 10; }
    var zero: NetDatagramResult = udp_recv_vectored_datagram(bound.value, null, 0);
    if (zero.error.native_code != 0 || zero.count != 0 || !zero.truncated) { return 11; }
    sent = udp_send_to(sender.value, local.value, &data[0], 0);
    if (sent.error.native_code != 0) { return 12; }
    zero = udp_recv_datagram(bound.value, null, 0);
    if (zero.error.native_code != 0 || zero.count != 0 || zero.truncated) { return 13; }
    sent = udp_send_to(sender.value, local.value, &data[0], 1);
    if (sent.error.native_code != 0) { return 14; }
    zero = udp_recv_datagram(bound.value, null, 0);
    if (zero.error.native_code != 0 || zero.count != 0 || !zero.truncated
        || !zero.original_size_known || zero.original_size != 1) { return 15; }
    sent = udp_send_to(sender.value, local.value, &data[7], 1);
    if (sent.error.native_code != 0) { return 16; }
    zero = udp_recv_datagram(bound.value, &data[0], 1);
    if (zero.error.native_code != 0 || zero.count != 1 || zero.truncated || data[0] != 99) { return 17; }
    udp_close(sender.value);
    udp_close(bound.value);
    return 0;
}
"###;

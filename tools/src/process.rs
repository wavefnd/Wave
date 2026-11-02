// SPDX-License-Identifier: MPL-2.0
//! One bounded owner for native tools and every descendant they launch.
use crate::{error, Result};
use std::collections::BTreeMap;
use std::ffi::OsString;
use std::fs::{self, File};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, OnceLock};
use std::thread;
use std::time::{Duration, Instant};

static CANCEL: OnceLock<Arc<AtomicUsize>> = OnceLock::new();

fn cancellation() -> Result<Arc<AtomicUsize>> {
    if let Some(flag) = CANCEL.get() {
        return Ok(flag.clone());
    }
    let flag = Arc::new(AtomicUsize::new(0));
    #[cfg(unix)]
    {
        signal_hook::flag::register_usize(signal_hook::consts::SIGINT, flag.clone(), 130)?;
        signal_hook::flag::register_usize(signal_hook::consts::SIGTERM, flag.clone(), 143)?;
    }
    #[cfg(windows)]
    {
        unsafe extern "system" fn handler(event: u32) -> i32 {
            if event <= 2 {
                if let Some(flag) = CANCEL.get() {
                    flag.store(130, Ordering::SeqCst);
                }
                1
            } else {
                0
            }
        }
        if unsafe { windows_sys::Win32::System::Console::SetConsoleCtrlHandler(Some(handler), 1) }
            == 0
        {
            return Err(std::io::Error::last_os_error().into());
        }
    }
    let _ = CANCEL.set(flag.clone());
    Ok(CANCEL.get().unwrap().clone())
}

pub fn cancelled() -> usize {
    CANCEL.get().map_or(0, |flag| flag.load(Ordering::SeqCst))
}

#[derive(Clone, Debug)]
pub struct Spec {
    pub args: Vec<OsString>,
    pub cwd: PathBuf,
    pub env: BTreeMap<OsString, OsString>,
    pub clean_env: bool,
    pub input: Option<Vec<u8>>,
    pub timeout: Duration,
    pub output_limit: usize,
    pub stream: bool,
    pub log: Option<PathBuf>,
    pub stdout_log: Option<PathBuf>,
    pub supervisor: Option<PathBuf>,
    pub stop: Option<Arc<AtomicBool>>,
    pub pass_fds: Vec<i32>,
    pub nofile: Option<u64>,
    pub stack: Option<u64>,
    pub nproc: Option<u64>,
    pub close_fds: Vec<i32>,
    /// Bounded recovery commands must finish even after the active task was cancelled.
    pub cleanup: bool,
}

impl Spec {
    pub fn new(args: impl IntoIterator<Item = impl Into<OsString>>) -> Self {
        Self {
            args: args.into_iter().map(Into::into).collect(),
            cwd: crate::root(),
            env: BTreeMap::new(),
            clean_env: false,
            input: None,
            timeout: Duration::from_secs(1800),
            output_limit: 64 * 1024 * 1024,
            stream: false,
            log: None,
            stdout_log: None,
            supervisor: None,
            stop: None,
            pass_fds: Vec::new(),
            nofile: None,
            stack: None,
            nproc: None,
            close_fds: Vec::new(),
            cleanup: false,
        }
    }

    pub fn run(&self) -> Result<Output> {
        run(self)
    }

    pub fn checked(&self) -> Result<Output> {
        let output = self.run()?;
        if output.status != 0 || output.failure.is_some() {
            return Err(error(format!(
                "command {:?}: {} (exit {}): {}",
                self.args,
                output.failure.as_deref().unwrap_or("failed"),
                output.status,
                output.text_stderr()
            )));
        }
        Ok(output)
    }
}

#[derive(Debug)]
pub struct Output {
    pub status: i32,
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
    pub failure: Option<String>,
    pub elapsed: Duration,
}

impl Output {
    pub fn text_stdout(&self) -> String {
        normalize(&String::from_utf8_lossy(&self.stdout))
    }

    pub fn text_stderr(&self) -> String {
        normalize(&String::from_utf8_lossy(&self.stderr))
    }

    pub fn strict_stdout(&self) -> Result<String> {
        Ok(normalize(std::str::from_utf8(&self.stdout)?))
    }
}

pub fn normalize(text: &str) -> String {
    text.replace("\r\n", "\n").replace('\r', "\n")
}

struct Owner {
    child: Child,
    #[cfg(windows)]
    job: windows_sys::Win32::Foundation::HANDLE,
    stopped: bool,
}

impl Owner {
    fn stop(&mut self) -> Result<()> {
        if self.stopped {
            return Ok(());
        }
        #[cfg(unix)]
        {
            use nix::sys::signal::{killpg, Signal};
            match killpg(nix::unistd::Pid::from_raw(self.child.id() as i32), Signal::SIGKILL) {
                Ok(()) | Err(nix::errno::Errno::ESRCH) => {},
                Err(e) => return Err(e.into()),
            }
        }
        #[cfg(windows)]
        {
            if unsafe { windows_sys::Win32::System::JobObjects::TerminateJobObject(self.job, 1) }
                == 0
            {
                return Err(std::io::Error::last_os_error().into());
            }
        }
        #[cfg(not(any(unix, windows)))]
        {
            self.child.kill()?;
        }
        self.child.wait()?;
        self.stopped = true;
        Ok(())
    }
}

impl Drop for Owner {
    fn drop(&mut self) {
        if let Err(e) = self.stop() {
            eprintln!("could not terminate owned process tree: {e}");
        }
        #[cfg(windows)]
        unsafe {
            windows_sys::Win32::Foundation::CloseHandle(self.job);
        }
    }
}

#[cfg(windows)]
fn attach_job(child: &Child) -> Result<windows_sys::Win32::Foundation::HANDLE> {
    use std::os::windows::io::AsRawHandle;
    use windows_sys::Win32::{Foundation::CloseHandle, System::JobObjects::*};
    let job = unsafe { CreateJobObjectW(std::ptr::null(), std::ptr::null()) };
    if job.is_null() {
        return Err(std::io::Error::last_os_error().into());
    }
    let mut limits: JOBOBJECT_EXTENDED_LIMIT_INFORMATION = unsafe { std::mem::zeroed() };
    limits.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
    let configured = unsafe {
        SetInformationJobObject(
            job,
            JobObjectExtendedLimitInformation,
            &limits as *const _ as *const _,
            std::mem::size_of_val(&limits) as u32,
        )
    };
    let assigned =
        configured != 0 && unsafe { AssignProcessToJobObject(job, child.as_raw_handle()) } != 0;
    if !assigned {
        let error = std::io::Error::last_os_error();
        unsafe {
            CloseHandle(job);
        }
        return Err(error.into());
    }
    Ok(job)
}

fn capture(
    mut source: impl Read + Send + 'static,
    limit: usize,
    overflow: Arc<AtomicBool>,
    stream: bool,
    stderr: bool,
    log: Option<Arc<std::sync::Mutex<File>>>,
    stdout_log: Option<File>,
) -> thread::JoinHandle<std::io::Result<Vec<u8>>> {
    thread::spawn(move || {
        let mut stdout_log = stdout_log;
        let mut bytes = Vec::new();
        let mut buffer = [0; 16 * 1024];
        loop {
            let count = source.read(&mut buffer)?;
            if count == 0 {
                break;
            }
            if let Some(log) = &mut stdout_log {
                log.write_all(&buffer[..count])?;
            }
            if let Some(log) = &log {
                log.lock()
                    .map_err(|_| std::io::Error::other("log mutex poisoned"))?
                    .write_all(&buffer[..count])?;
            }
            if stream {
                if stderr {
                    std::io::stderr().write_all(&buffer[..count])?;
                } else {
                    std::io::stdout().write_all(&buffer[..count])?;
                }
            }
            let retained = (limit.saturating_sub(bytes.len())).min(count);
            bytes.extend_from_slice(&buffer[..retained]);
            if retained != count {
                overflow.store(true, Ordering::SeqCst);
            }
        }
        Ok(bytes)
    })
}

pub fn run(spec: &Spec) -> Result<Output> {
    if spec.args.is_empty() {
        return Err(error("command vector cannot be empty"));
    }
    let cancel = cancellation()?;
    let directory = tempfile::Builder::new().prefix("wave-process-").tempdir()?;
    let ready = directory.path().join("ready");
    let completed = directory.path().join("completed");
    let executable = spec.supervisor.clone().map(Ok).unwrap_or_else(std::env::current_exe)?;
    let mut command = Command::new(executable);
    command
        .arg("__supervise")
        .arg(&ready)
        .arg(&completed)
        .arg(format!(
            "{}/{}/{}/{}",
            spec.nofile.map(|v| v.to_string()).unwrap_or_default(),
            spec.stack.map(|v| v.to_string()).unwrap_or_default(),
            spec.nproc.map(|v| v.to_string()).unwrap_or_default(),
            spec.close_fds.iter().map(i32::to_string).collect::<Vec<_>>().join(",")
        ))
        .args(&spec.args)
        .current_dir(&spec.cwd)
        .stdin(if spec.input.is_some() { Stdio::piped() } else { Stdio::null() })
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    if spec.clean_env {
        command.env_clear();
    }
    command.envs(&spec.env);
    #[cfg(unix)]
    {
        use std::os::unix::process::CommandExt;
        command.process_group(0);
        let descriptors = spec.pass_fds.clone();
        // Only async-signal-safe descriptor/rlimit operations execute after fork.
        unsafe {
            command.pre_exec(move || {
                for fd in &descriptors {
                    let borrowed = std::os::fd::BorrowedFd::borrow_raw(*fd);
                    let flags = nix::fcntl::fcntl(borrowed, nix::fcntl::FcntlArg::F_GETFD)
                        .map_err(|e| std::io::Error::from_raw_os_error(e as i32))?;
                    let flags = nix::fcntl::FdFlag::from_bits_truncate(flags)
                        & !nix::fcntl::FdFlag::FD_CLOEXEC;
                    nix::fcntl::fcntl(borrowed, nix::fcntl::FcntlArg::F_SETFD(flags))
                        .map_err(|e| std::io::Error::from_raw_os_error(e as i32))?;
                }
                Ok(())
            });
        }
    }
    let mut child = command.spawn()?;
    #[cfg(windows)]
    let job = match attach_job(&child) {
        Ok(job) => job,
        Err(error) => {
            let _ = child.kill();
            let _ = child.wait();
            return Err(error);
        },
    };
    let out = child.stdout.take().unwrap();
    let err = child.stderr.take().unwrap();
    let stdin = child.stdin.take();
    let mut owner = Owner {
        child,
        stopped: false,
        #[cfg(windows)]
        job,
    };
    let overflow = Arc::new(AtomicBool::new(false));
    let log = spec
        .log
        .as_ref()
        .map(|path| File::create(path).map(|f| Arc::new(std::sync::Mutex::new(f))))
        .transpose()?;
    let stdout_log = spec.stdout_log.as_ref().map(File::create).transpose()?;
    let stdout = capture(
        out,
        spec.output_limit,
        overflow.clone(),
        spec.stream,
        false,
        log.clone(),
        stdout_log,
    );
    let stderr = capture(err, spec.output_limit, overflow.clone(), spec.stream, true, log, None);
    let input = spec
        .input
        .clone()
        .zip(stdin)
        .map(|(bytes, mut stream)| thread::spawn(move || stream.write_all(&bytes)));
    fs::write(&ready, [])?;
    let started = Instant::now();
    let (status, failure) = loop {
        if spec.stop.as_ref().is_some_and(|stop| stop.load(Ordering::SeqCst)) {
            break (0, Some("stopped".into()));
        }
        let cancelled = cancel.load(Ordering::SeqCst);
        if cancelled != 0 && !spec.cleanup {
            break (cancelled as i32, Some("cancelled".into()));
        }
        if overflow.load(Ordering::SeqCst) {
            break (1, Some("output limit exceeded".into()));
        }
        if started.elapsed() >= spec.timeout {
            break (1, Some("timeout".into()));
        }
        if let Ok(text) = fs::read_to_string(&completed) {
            match text.trim().parse::<i32>() {
                Ok(status) => break (status, None),
                Err(_) => break (1, Some(format!("supervisor failed: {text}"))),
            }
        }
        thread::sleep(Duration::from_millis(10));
    };
    // The supervisor retains its PID until this point, even after the real tool
    // exits. Its process group cannot be recycled before we terminate descendants.
    owner.stop()?;
    if let Some(input) = input {
        let _ = input.join();
    }
    let stdout = stdout.join().map_err(|_| error("stdout collector panicked"))??;
    let stderr = stderr.join().map_err(|_| error("stderr collector panicked"))??;
    let failure = if failure.is_none() && overflow.load(Ordering::SeqCst) {
        Some("output limit exceeded".into())
    } else {
        failure
    };
    Ok(Output { status, stdout, stderr, failure, elapsed: started.elapsed() })
}

pub fn supervise(args: &[OsString]) -> Result<i32> {
    if args.len() < 4 {
        return Err(error("internal supervisor needs gate, completion and command"));
    }
    let ready = Path::new(&args[0]);
    let completed = Path::new(&args[1]);
    let started = Instant::now();
    while !ready.exists() {
        if started.elapsed() > Duration::from_secs(30) {
            return Err(error("supervisor was not assigned to its owner"));
        }
        thread::sleep(Duration::from_millis(10));
    }
    let limits = args[2]
        .to_str()
        .ok_or_else(|| error("invalid internal limits"))?
        .split('/')
        .collect::<Vec<_>>();
    if limits.len() != 4 {
        return Err(error("invalid internal limits"));
    }
    let mut command = Command::new(&args[3]);
    command.args(&args[4..]);
    #[cfg(unix)]
    {
        use std::os::unix::process::CommandExt;
        let parse = |value: &str| -> Result<Option<u64>> {
            if value.is_empty() {
                Ok(None)
            } else {
                Ok(Some(value.parse()?))
            }
        };
        let nofile = parse(limits[0])?;
        let stack = parse(limits[1])?;
        let nproc = parse(limits[2])?;
        #[cfg(target_vendor = "apple")]
        if nproc.is_some() {
            return Err(error(
                "process-count limit is unavailable through this platform resource API",
            ));
        }
        let closed = limits[3]
            .split(',')
            .filter(|v| !v.is_empty())
            .map(str::parse::<i32>)
            .collect::<std::result::Result<Vec<_>, _>>()?;
        // Limits belong to the actual tool, not the supervisor that must launch it.
        unsafe {
            command.pre_exec(move || {
                for (resource, limit) in [
                    (nix::sys::resource::Resource::RLIMIT_NOFILE, nofile),
                    (nix::sys::resource::Resource::RLIMIT_STACK, stack),
                    #[cfg(not(target_vendor = "apple"))]
                    (nix::sys::resource::Resource::RLIMIT_NPROC, nproc),
                ] {
                    if let Some(limit) = limit {
                        let (_, hard) = nix::sys::resource::getrlimit(resource)
                            .map_err(|e| std::io::Error::from_raw_os_error(e as i32))?;
                        let soft = (limit as u128)
                            .min(hard as u128)
                            .min(nix::sys::resource::rlim_t::MAX as u128)
                            as nix::sys::resource::rlim_t;
                        nix::sys::resource::setrlimit(resource, soft, hard)
                            .map_err(|e| std::io::Error::from_raw_os_error(e as i32))?;
                    }
                }

                for fd in &closed {
                    nix::unistd::close(*fd)
                        .map_err(|e| std::io::Error::from_raw_os_error(e as i32))?;
                }
                Ok(())
            });
        }
    }
    let status = match command.status() {
        Ok(status) => {
            #[cfg(unix)]
            {
                use std::os::unix::process::ExitStatusExt;
                status.code().unwrap_or_else(|| -status.signal().unwrap_or(1))
            }
            #[cfg(not(unix))]
            {
                status.code().unwrap_or(1)
            }
        },
        Err(error) => {
            crate::storage::atomic_write(completed, format!("launch failed: {error}").as_bytes())?;
            loop {
                thread::sleep(Duration::from_secs(1));
            }
        },
    };
    crate::storage::atomic_write(completed, status.to_string().as_bytes())?;
    loop {
        thread::sleep(Duration::from_secs(1));
    }
}

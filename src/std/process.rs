// SPDX-License-Identifier: MPL-2.0
//! Installer-specific subprocess limits; this does not change compiler execution.
use crate::errors::CliError;
use std::cell::Cell;
use std::io::Read;
use std::process::{Command, Output, Stdio};
use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc,
};
use std::time::{Duration, Instant};

thread_local! { static DEADLINE: Cell<Option<Instant>> = const { Cell::new(None) }; }
pub(super) struct Budget(Option<Instant>);
impl Budget {
    pub(super) fn begin() -> Self {
        Self(DEADLINE.with(|slot| slot.replace(Some(Instant::now() + Duration::from_secs(600)))))
    }
}
impl Drop for Budget {
    fn drop(&mut self) {
        DEADLINE.with(|slot| slot.set(self.0));
    }
}

pub(super) fn check_deadline() -> Result<(), CliError> {
    if DEADLINE.with(|slot| slot.get()).is_some_and(|deadline| Instant::now() >= deadline) {
        Err(CliError::InstallationTimeout("total installation deadline".into()))
    } else {
        Ok(())
    }
}

fn capture(mut stream: impl Read, overflow: Arc<AtomicBool>) -> std::io::Result<Vec<u8>> {
    let mut data = Vec::new();
    let mut buffer = [0; 8192];
    loop {
        let count = stream.read(&mut buffer)?;
        if count == 0 {
            return Ok(data);
        }
        let available = (1024 * 1024usize).saturating_sub(data.len());
        data.extend_from_slice(&buffer[..count.min(available)]);
        if count > available {
            overflow.store(true, Ordering::Release);
        }
    }
}

pub(super) fn output(command: &mut Command, limit: Duration) -> Result<Output, CliError> {
    let now = Instant::now();
    let deadline = DEADLINE.with(|slot| slot.get()).unwrap_or(now + limit).min(now + limit);
    if now >= deadline {
        return Err(CliError::InstallationTimeout("total installation deadline".into()));
    }
    command.stdin(Stdio::null()).stdout(Stdio::piped()).stderr(Stdio::piped());
    #[cfg(unix)]
    {
        use std::os::unix::process::CommandExt;
        command.process_group(0);
    }
    #[cfg(windows)]
    {
        use std::os::windows::process::CommandExt;
        command.creation_flags(0x00000004); // CREATE_SUSPENDED; attach before any child can escape.
    }
    let mut child = command.spawn()?;
    #[cfg(windows)]
    let tree = match windows::Job::attach(&child) {
        Ok(job) => job,
        Err(error) => {
            let _ = child.kill();
            let _ = child.wait();
            return Err(error.into());
        },
    };
    let overflow = Arc::new(AtomicBool::new(false));
    let stdout = child.stdout.take().expect("piped stdout");
    let stderr = child.stderr.take().expect("piped stderr");
    let a = overflow.clone();
    let b = overflow.clone();
    let stdout = std::thread::spawn(move || capture(stdout, a));
    let stderr = std::thread::spawn(move || capture(stderr, b));
    let mut failure = None;
    let mut status = None;
    loop {
        if overflow.load(Ordering::Acquire) {
            failure = Some(CliError::InstallationOutputLimit(
                "subprocess stdout/stderr exceeded 1 MiB".into(),
            ));
            break;
        }
        if Instant::now() >= deadline {
            failure = Some(CliError::InstallationTimeout("subprocess deadline exceeded".into()));
            break;
        }
        if status.is_none() {
            match child.try_wait() {
                Ok(result) => status = result,
                Err(error) => {
                    failure = Some(error.into());
                    break;
                },
            }
        }
        if status.is_some() {
            break;
        }
        std::thread::sleep(Duration::from_millis(5));
    }
    // Terminate the group even when the direct child has exited: descendants
    // may still own inherited pipes and must not keep capture threads alive.
    #[cfg(unix)]
    {
        let _ = Command::new("/bin/kill")
            .args(["-KILL", "--", &format!("-{}", child.id())])
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status();
    }
    #[cfg(windows)]
    drop(tree);
    if status.is_none() {
        let _ = child.kill();
    }
    let status = match status {
        Some(status) => Ok(status),
        None => child.wait(),
    };
    // Join both drains even when waiting or one drain fails.
    let stdout = stdout.join().map_err(|_| std::io::Error::other("stdout reader failed"));
    let stderr = stderr.join().map_err(|_| std::io::Error::other("stderr reader failed"));
    let status = status?;
    let stdout = stdout??;
    let stderr = stderr??;
    if overflow.load(Ordering::Acquire) && failure.is_none() {
        failure = Some(CliError::InstallationOutputLimit(
            "subprocess stdout/stderr exceeded 1 MiB".into(),
        ));
    }
    if let Some(error) = failure {
        let evidence = format!(
            "stdout: {}\nstderr: {}",
            String::from_utf8_lossy(&stdout[..stdout.len().min(4096)]),
            String::from_utf8_lossy(&stderr[..stderr.len().min(4096)])
        );
        return Err(match error {
            CliError::InstallationTimeout(message) => {
                CliError::InstallationTimeout(format!("{message}\n{evidence}"))
            },
            CliError::InstallationOutputLimit(message) => {
                CliError::InstallationOutputLimit(format!("{message}\n{evidence}"))
            },
            error => error,
        });
    }
    Ok(Output { status, stdout, stderr })
}

#[cfg(windows)]
mod windows {
    use std::ffi::c_void;
    use std::os::windows::io::AsRawHandle;
    type Handle = *mut c_void;
    #[repr(C)]
    #[derive(Default)]
    struct BasicLimits {
        process_time: i64,
        job_time: i64,
        flags: u32,
        minimum: usize,
        maximum: usize,
        active: u32,
        affinity: usize,
        priority: u32,
        scheduling: u32,
    }
    #[repr(C)]
    #[derive(Default)]
    struct ExtendedLimits {
        basic: BasicLimits,
        io: [u64; 6],
        process_memory: usize,
        job_memory: usize,
        peak_process: usize,
        peak_job: usize,
    }
    #[link(name = "kernel32")]
    unsafe extern "system" {
        fn CreateJobObjectW(attributes: *const c_void, name: *const u16) -> Handle;
        fn SetInformationJobObject(job: Handle, class: i32, info: *const c_void, size: u32) -> i32;
        fn AssignProcessToJobObject(job: Handle, process: Handle) -> i32;
        fn CloseHandle(handle: Handle) -> i32;
    }
    #[link(name = "ntdll")]
    unsafe extern "system" {
        fn NtResumeProcess(process: Handle) -> i32;
    }
    pub(super) struct Job(Handle);
    impl Job {
        pub(super) fn attach(child: &std::process::Child) -> std::io::Result<Self> {
            // SAFETY: handles are owned/live and info has the documented ABI layout.
            unsafe {
                let handle = CreateJobObjectW(std::ptr::null(), std::ptr::null());
                if handle.is_null() {
                    return Err(std::io::Error::last_os_error());
                }
                let job = Self(handle);
                let mut info = ExtendedLimits::default();
                info.basic.flags = 0x2000; // JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE
                if SetInformationJobObject(
                    handle,
                    9,
                    &info as *const _ as _,
                    std::mem::size_of_val(&info) as u32,
                ) == 0
                    || AssignProcessToJobObject(handle, child.as_raw_handle()) == 0
                {
                    return Err(std::io::Error::last_os_error());
                }
                let status = NtResumeProcess(child.as_raw_handle());
                if status < 0 {
                    return Err(std::io::Error::other(format!(
                        "resume subprocess NTSTATUS {status:#x}"
                    )));
                }
                Ok(job)
            }
        }
    }
    impl Drop for Job {
        fn drop(&mut self) {
            // SAFETY: this Job owns the handle and closes it exactly once.
            unsafe {
                CloseHandle(self.0);
            }
        }
    }
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;
    #[test]
    fn timeout_kills_descendants_holding_output_pipes_and_releases_budget() {
        let start = Instant::now();
        let error = output(
            Command::new("/bin/sh").args(["-c", "sleep 30 & echo started; wait"]),
            Duration::from_millis(50),
        )
        .unwrap_err();
        assert!(matches!(error, CliError::InstallationTimeout(_)));
        assert!(error.message().contains("started"));
        assert!(start.elapsed() < Duration::from_secs(3));
        assert!(output(Command::new("/bin/sh").args(["-c", "printf ok"]), Duration::from_secs(1))
            .unwrap()
            .status
            .success());
    }

    #[test]
    fn output_overflow_is_distinct_from_timeout_and_exit_failure() {
        let error = output(
            Command::new("/bin/sh").args(["-c", "head -c 1048577 /dev/zero"]),
            Duration::from_secs(3),
        )
        .unwrap_err();
        assert!(matches!(error, CliError::InstallationOutputLimit(_)));
        let failed = output(
            Command::new("/bin/sh").args(["-c", "printf error >&2; exit 7"]),
            Duration::from_secs(1),
        )
        .unwrap();
        assert_eq!(failed.status.code(), Some(7));
        assert_eq!(failed.stderr, b"error");
    }

    #[test]
    fn total_deadline_takes_precedence_over_per_command_budget() {
        let _budget = Budget::begin();
        DEADLINE.with(|slot| slot.set(Some(Instant::now())));
        assert!(matches!(
            output(&mut Command::new("/bin/sh"), Duration::from_secs(300)),
            Err(CliError::InstallationTimeout(_))
        ));
    }
}

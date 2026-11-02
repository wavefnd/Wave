// SPDX-License-Identifier: MPL-2.0
#[cfg(unix)]
mod native {
    use std::fs;
    use std::os::unix::fs::PermissionsExt;
    use std::path::Path;
    use std::process::Command;
    use wave_tools::config;

    fn compiler(path: &Path, build_exit: i32, program: Option<&str>) {
        let artifact = program
            .map(|body| {
                format!(
                    "printf '%s\\n' '#!/bin/sh' '{}' > \"$out\"\nchmod +x \"$out\"\n",
                    body.replace('\'', "'\\''")
                )
            })
            .unwrap_or_default();
        fs::write(path, format!("#!/bin/sh\nout=''\nwhile [ $# -gt 0 ]; do\n if [ \"$1\" = '-o' ]; then shift; out=$1; fi\n shift\ndone\nif [ {build_exit} -ne 0 ]; then printf 'compiler failure' >&2; exit {build_exit}; fi\n{artifact}exit 0\n")).unwrap();
        fs::set_permissions(path, fs::Permissions::from_mode(0o755)).unwrap();
    }

    #[test]
    fn compiler_failures_missing_artifacts_and_program_exits_are_distinct() {
        for (build_exit, program, expected, success, phase) in [
            (7, Some("exit 7"), 7, false, "build"),
            (0, Some("printf ok; exit 7"), 7, true, "run"),
            (0, None, 0, false, "build"),
            (0, Some("kill -SEGV $$"), 0, false, "run"),
            (0, Some("sleep 5"), 0, false, "run"),
        ] {
            let work = tempfile::tempdir().unwrap();
            let source = work.path().join("input.wave");
            let wavec = work.path().join("wavec");
            let report = work.path().join("report.json");
            fs::write(
                &source,
                format!("// wave-test: expected-exit={expected}\nfun main() {{}}\n"),
            )
            .unwrap();
            compiler(&wavec, build_exit, program);
            let result = Command::new(env!("CARGO_BIN_EXE_wave-tools"))
                .arg("test")
                .arg("--wavec")
                .arg(&wavec)
                .arg("--source")
                .arg(&source)
                .arg("--timeout")
                .arg("1")
                .arg("--report-json")
                .arg(&report)
                .output()
                .unwrap();
            assert_eq!(
                result.status.success(),
                success,
                "{}",
                String::from_utf8_lossy(&result.stderr)
            );
            let report = config::read_json(&report).unwrap();
            let row = &config::array(&report, "tests").unwrap()[0];
            assert_eq!(row.get_str("phase"), Some(phase));
            if build_exit != 0 {
                assert_eq!(row.get_u64("actual_exit"), Some(build_exit as u64));
                assert_eq!(config::array(row, "commands").unwrap().len(), 1);
            }
            if program == Some("sleep 5") {
                assert_eq!(row.get_str("status"), Some("timeout"));
            }
        }
    }

    #[test]
    fn output_aliases_fail_before_compiler_launch_and_preserve_source_bytes() {
        let work = tempfile::tempdir().unwrap();
        let source = work.path().join("input.wave");
        let wavec = work.path().join("wavec");
        fs::write(&source, "fun main() {}\n").unwrap();
        compiler(&wavec, 0, Some("exit 0"));
        let report = work.path().join("alias.json");
        fs::hard_link(&source, &report).unwrap();
        let result = Command::new(env!("CARGO_BIN_EXE_wave-tools"))
            .arg("test")
            .arg("--wavec")
            .arg(&wavec)
            .arg("--source")
            .arg(&source)
            .arg("--report-json")
            .arg(report)
            .output()
            .unwrap();
        assert!(!result.status.success());
        assert!(String::from_utf8_lossy(&result.stderr).contains("aliases an input"));
        assert_eq!(fs::read_to_string(source).unwrap(), "fun main() {}\n");
    }

    #[test]
    fn non_binary_outputs_use_the_compiler_out_dir_contract() {
        let work = tempfile::tempdir().unwrap();
        let compiler = work.path().join("wavec");
        fs::write(&compiler, r#"#!/bin/sh
output=''
emit=''
while [ $# -gt 0 ]; do
    case "$1" in
        -o) echo 'Error: -o is only allowed for final binary output, or single-input --emit=obj' >&2; exit 2 ;;
        --out-dir) shift; output=$1 ;;
        --emit=*) emit=${1#--emit=} ;;
    esac
    shift
done
case "$emit" in
    asm) suffix=s ;;
    ir) suffix=ll ;;
    bc) suffix=bc ;;
    *) exit 3 ;;
esac
printf 'ecall a7' > "$output/input.$suffix"
"#).unwrap();
        fs::set_permissions(&compiler, fs::Permissions::from_mode(0o755)).unwrap();
        for emit in ["asm", "ir", "bc"] {
            let source = work.path().join("input.wave");
            fs::write(
                &source,
                format!("// wave-test: mode=build, runner=compile, emit={emit}\nfun main() {{}}\n"),
            )
            .unwrap();
            let result = Command::new(env!("CARGO_BIN_EXE_wave-tools"))
                .arg("test")
                .arg("--wavec")
                .arg(&compiler)
                .arg("--source")
                .arg(&source)
                .output()
                .unwrap();
            assert!(result.status.success(), "{emit}: {}", String::from_utf8_lossy(&result.stderr));
        }
    }
}

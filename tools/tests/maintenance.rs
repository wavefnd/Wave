// SPDX-License-Identifier: MPL-2.0
use std::fs;
use std::process::Command;
use wave_tools::maintenance::{function_spacing, public_declarations};

#[test]
fn spacing_preserves_literals_lifetimes_comments_attributes_and_nested_scopes() {
    let bodies = [
        r##"let a = "escaped \" } fn fake() {}"; let b = r#"} fn fake() {}"#;"##,
        "let b = br##\"} fn fake() {}\"##; let c = '}';",
        "'outer: loop { let c = 'a'; break 'outer; }",
        "/* outer /* } fn fake() {} */ comment */",
        "fn local() {}\n    fn other() {}",
    ];
    for body in bodies {
        let source = format!("fn first<'℘>(_: &'℘ str) {{\n    {body}\n}}\nfn second() {{}}\n");
        let formatted = function_spacing(&source).unwrap();
        assert!(formatted.contains("}\n\nfn second"), "{formatted}");
        assert_eq!(function_spacing(&formatted).unwrap(), formatted);
        assert_eq!(formatted.replace("\n\n", "\n"), source);
    }
    for attachment in [
        "#[test]\n",
        "/// documentation\n#[inline]\n",
        "// ordinary comment\n",
        "/* outer\n\n /* nested */ comment */\n",
        "#[cfg_attr(test, doc = \"braces {}\")]\n",
    ] {
        let source = format!("fn first() {{}}\n{attachment}fn second() {{}}\n");
        assert_eq!(
            function_spacing(&source).unwrap(),
            format!("fn first() {{}}\n\n{attachment}fn second() {{}}\n")
        );
    }
    let nested = "impl Example {\n    fn first() {}\n    /// second\n    fn second() {}\n}\n";
    assert_eq!(function_spacing(nested).unwrap(), nested.replace("    ///", "\n    ///"));
    for prefix in [
        "pub(crate) const",
        "pub(in crate::widgets) const unsafe",
        "pub(self) async",
        "pub(super) unsafe extern \"C\"",
        "extern \"C-unwind\"",
    ] {
        let source = format!("{prefix} fn first() {{}}\n{prefix} fn second() {{}}\n");
        assert_eq!(
            function_spacing(&source).unwrap(),
            source.replace("\n", "\n\n").trim_end().to_owned() + "\n"
        );
    }
}

#[test]
fn spacing_leaves_separate_items_and_existing_separators_intact() {
    for between in [
        "const V: i32 = 1;\n#[test]\n",
        "struct Other {}\n/// Documentation\n",
        "\n// already separated\n",
        "extern \"C\" { fn imported(); }\n",
        "trait Contract { fn declared(); }\n",
    ] {
        let source = format!("fn first() {{}}\n{between}fn second() {{}}\n");
        assert_eq!(function_spacing(&source).unwrap(), source);
    }
    let source = "fn first() {} // previous\n// second\nfn second() {}\n";
    assert_eq!(function_spacing(source).unwrap(), source.replace("\n// second", "\n\n// second"));
}

#[test]
fn public_inventory_ignores_comment_and_string_lookalikes_and_retains_attributes() {
    let source = "// pub fun hidden() {}\nconst TEXT: str = \"pub fun fake() {}\";\n#[target_os(\"linux\")]\npub fun real(\n value: i32\n) -> i32 { return value; }\nimport(\"private\");\npub import(\"public\");\n";
    let rows = public_declarations(source).unwrap();
    assert_eq!(rows.len(), 2);
    assert!(rows[0].2.contains("real"));
    assert!(rows[0].1.iter().any(|a| a.contains("linux")));
    assert!(rows[1].2.contains("public"));
}

#[test]
fn offline_unicode_generation_matches_ranges_and_preserves_inputs() {
    let work = tempfile::tempdir().unwrap();
    let root = work.path();
    fs::write(
        root.join("UnicodeData.txt"),
        "0300;COMBINING;Mn\n0301;COMBINING;Mn\n0041;A;Lu\n200D;JOINER;Cf\n",
    )
    .unwrap();
    fs::write(root.join("EastAsianWidth.txt"), "4E00..4E02; W # range\nFF01; F\n0041; Na\n")
        .unwrap();
    fs::write(root.join("license.txt"), "Test license\n").unwrap();
    let output = root.join("tables.rs");
    let result = Command::new(env!("CARGO_BIN_EXE_wave-tools"))
        .args(["maintain", "unicode", "--input-dir"])
        .arg(root)
        .arg("--output")
        .arg(&output)
        .output()
        .unwrap();
    assert!(result.status.success(), "{}", String::from_utf8_lossy(&result.stderr));
    let text = fs::read_to_string(&output).unwrap();
    for range in [
        "(0x300, 0x301)",
        "(0x1160, 0x11FF)",
        "(0x200D, 0x200D)",
        "(0x4E00, 0x4E02)",
        "(0xFF01, 0xFF01)",
    ] {
        assert!(text.contains(range), "{text}");
    }
    assert!(!text.contains("(0x41, 0x41)"));
    let before = fs::read(root.join("UnicodeData.txt")).unwrap();
    assert!(!Command::new(env!("CARGO_BIN_EXE_wave-tools"))
        .args(["maintain", "unicode", "--input-dir"])
        .arg(root)
        .arg("--output")
        .arg(root.join("UnicodeData.txt"))
        .status()
        .unwrap()
        .success());
    assert_eq!(fs::read(root.join("UnicodeData.txt")).unwrap(), before);
}

#[test]
#[cfg(unix)]
fn patch_verification_restores_owned_branch_on_success_dco_and_apply_failures() {
    use std::os::unix::fs::PermissionsExt;
    fn git(root: &std::path::Path, args: &[&str]) -> String {
        let result = Command::new("git")
            .args([
                "-c",
                "user.name=Fixture",
                "-c",
                "user.email=fixture@example.invalid",
                "-c",
                "commit.gpgsign=false",
            ])
            .args(args)
            .current_dir(root)
            .output()
            .unwrap();
        assert!(result.status.success(), "{}", String::from_utf8_lossy(&result.stderr));
        String::from_utf8(result.stdout).unwrap()
    }
    for scenario in ["success", "unsigned", "conflict"] {
        let work = tempfile::tempdir().unwrap();
        let root = work.path().join("repo");
        fs::create_dir(&root).unwrap();
        git(&root, &["init", "--quiet", "-b", "original"]);
        // The child verifier runs git am independently of this fixture's git
        // helper. Its committer must not depend on the developer's global Git.
        git(&root, &["config", "--local", "user.name", "Fixture"]);
        git(&root, &["config", "--local", "user.email", "fixture@example.invalid"]);
        git(&root, &["config", "--local", "commit.gpgsign", "false"]);
        fs::create_dir_all(root.join("tools/ci")).unwrap();
        fs::copy(
            wave_tools::root().join("tools/ci/toolchains.json"),
            root.join("tools/ci/toolchains.json"),
        )
        .unwrap();
        fs::write(root.join("input"), "before\n").unwrap();
        git(&root, &["add", "."]);
        git(&root, &["commit", "--quiet", "-m", "base"]);
        let base = git(&root, &["rev-parse", "HEAD"]);
        fs::write(root.join("input"), "after\n").unwrap();
        git(&root, &["add", "."]);
        let mut commit = vec!["commit", "--quiet", "-m", "patch"];
        if scenario != "unsigned" {
            commit.push("--signoff");
        }
        git(&root, &commit);
        let patch = work.path().join("change.patch");
        fs::write(&patch, git(&root, &["format-patch", "-1", "--stdout"])).unwrap();
        git(&root, &["reset", "--hard", base.trim()]);
        if scenario == "conflict" {
            fs::write(root.join("input"), "conflict\n").unwrap();
            git(&root, &["add", "."]);
            git(&root, &["commit", "--quiet", "-m", "conflict"]);
        }
        let expected = git(&root, &["rev-parse", "HEAD"]);
        let shims = work.path().join("bin");
        fs::create_dir(&shims).unwrap();
        let record = work.path().join("cargo.log");
        let cargo = shims.join("cargo");
        fs::write(&cargo, format!("#!/bin/sh\nprintf '%s\\n' \"$*\" >> '{}'\n", record.display()))
            .unwrap();
        fs::set_permissions(&cargo, fs::Permissions::from_mode(0o755)).unwrap();
        let paths = std::env::join_paths(
            std::iter::once(shims).chain(std::env::split_paths(&std::env::var_os("PATH").unwrap())),
        )
        .unwrap();
        let result = Command::new(env!("CARGO_BIN_EXE_wave-tools"))
            .args(["maintain", "verify-patch"])
            .arg(patch)
            .env("WAVE_REPOSITORY", &root)
            .env("GIT_CONFIG_GLOBAL", "/dev/null")
            .env("GIT_CONFIG_NOSYSTEM", "1")
            .env("PATH", paths)
            .output()
            .unwrap();
        assert_eq!(
            result.status.success(),
            scenario == "success",
            "{}",
            String::from_utf8_lossy(&result.stderr)
        );
        assert_eq!(git(&root, &["rev-parse", "HEAD"]), expected);
        assert_eq!(git(&root, &["branch", "--show-current"]).trim(), "original");
        assert!(!git(&root, &["branch"]).contains("patch-verify"));
        if scenario == "success" {
            assert_eq!(fs::read_to_string(&record).unwrap().lines().count(), 4);
        } else {
            assert!(!record.exists());
        }
        assert!(git(&root, &["status", "--porcelain"]).is_empty());
    }
}

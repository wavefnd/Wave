//! CPU profiles refine base architectures without changing enabled CI cells.
use std::path::PathBuf;
use wave_tools::cases::Manifest;

const MANIFEST: &str = r#"
version = 3
[supported]
linux = { arm64 = true, riscv64 = true }
[ci]
linux = { arm64 = false, riscv64 = false }
[profiles]
rhea = { os = "linux", arch = "arm64", supported = false, ci = false }
shakti = { os = "linux", arch = "riscv64", supported = false, ci = false }
xiangshan = { os = "linux", arch = "riscv64", supported = false, ci = false }
t-head = { os = "linux", arch = "riscv64", supported = false, ci = false }
[[target]]
id = "linux-arm64"
os = "linux"
arch = "arm64"
executor = "native"
[[target]]
id = "linux-riscv64"
os = "linux"
arch = "riscv64"
executor = "native"
"#;

fn parse(value: &str) -> Result<Manifest, String> {
    Manifest::parse(value, PathBuf::from("not-used"), false).map_err(|e| e.to_string())
}

#[test]
fn profiles_inherit_base_suites_and_retain_separate_identities() {
    let manifest = parse(MANIFEST).unwrap();
    let base = manifest.target("linux-arm64").unwrap();
    assert_eq!(base.suites(), ["shared", "shared/arm64", "linux/arm64"]);
    assert_eq!(base.profile, None);
    let expected = [
        ("rhea", "arm64"),
        ("shakti", "riscv64"),
        ("xiangshan", "riscv64"),
        ("t-head", "riscv64"),
    ];
    for (name, arch) in expected {
        let target = manifest.target(&format!("linux-{name}")).unwrap();
        assert_eq!(target.arch, arch);
        assert_eq!(target.profile.as_deref(), Some(name));
        assert_eq!(
            target.suites(),
            [
                "shared".to_string(),
                format!("shared/{arch}"),
                format!("shared/{name}"),
                format!("linux/{arch}"),
                format!("linux/{name}"),
            ]
        );
        assert!(!target.enabled);
        assert!(!target.ci);
        assert!(target.rust_target().is_err());
        let value = target.value();
        assert_eq!(value.get_str("arch"), Some(arch));
        assert_eq!(value.get_str("profile"), Some(name));
        assert_eq!(value.get_str("suite"), Some(format!("linux/{name}").as_str()));
    }
}

#[test]
fn invalid_profile_architecture_status_and_names_are_rejected() {
    for invalid in [
        MANIFEST.replace(
            "rhea = { os = \"linux\", arch = \"arm64\"",
            "rhea = { os = \"linux\", arch = \"riscv64\"",
        ),
        MANIFEST.replace("rhea = {", "unknown = {"),
        MANIFEST.replace("shakti = { os = \"linux\"", "shakti = { os = \"windows\""),
        MANIFEST.replace("supported = false, ci = false }", "supported = false, ci = true }"),
        MANIFEST.replace(
            "rhea = { os = \"linux\", arch = \"arm64\", supported = false",
            "rhea = { os = \"linux\", arch = \"arm64\", supported = true",
        ),
        MANIFEST.replace("arch = \"arm64\"\nexecutor", "arch = \"rhea\"\nexecutor"),
        MANIFEST.replace("version = 3", "version = 2"),
    ] {
        assert!(parse(&invalid).is_err(), "accepted invalid manifest: {invalid}");
    }
}

#[test]
fn profile_target_tables_must_match_the_profile_mapping() {
    let enabled = MANIFEST.replace(
        "rhea = { os = \"linux\", arch = \"arm64\", supported = false",
        "rhea = { os = \"linux\", arch = \"arm64\", supported = true",
    );
    assert!(parse(&enabled).is_err(), "enabled profile without execution table");

    let valid = format!(
        "{enabled}
[[target]]
id = \"linux-rhea\"
os = \"linux\"
arch = \"arm64\"
profile = \"rhea\"
executor = \"compile\"
target = \"aarch64-unknown-linux-gnu\"
"
    );
    let parsed = parse(&valid).unwrap();
    let target = parsed.target("linux-rhea").unwrap();
    assert!(target.enabled);
    assert_eq!(target.profile.as_deref(), Some("rhea"));
    assert_eq!(target.arch, "arm64");
    assert_eq!(target.rust_target().unwrap(), "aarch64-unknown-linux-gnu");

    let mismatch = valid.replace(
        "arch = \"arm64\"\nprofile = \"rhea\"",
        "arch = \"riscv64\"\nprofile = \"rhea\"",
    );
    assert!(parse(&mismatch).is_err(), "accepted mismatched profile base");
    let unknown = valid.replace("profile = \"rhea\"", "profile = \"other\"");
    assert!(parse(&unknown).is_err(), "accepted unknown target profile");
}

#[test]
fn repository_enabled_matrices_remain_at_thirty_jobs() {
    let manifest = Manifest::load(&wave_tools::root(), true).unwrap();
    let matrix = manifest.matrices().unwrap();
    let utils::wson::Value::Object(groups) = matrix else {
        panic!("matrix groups")
    };
    assert_eq!(
        groups
            .iter()
            .map(|(_, v)| v.get_arr("include").unwrap().len())
            .sum::<usize>(),
        30
    );
    let rhea = manifest.target("linux-rhea").unwrap();
    assert_eq!(rhea.profile.as_deref(), Some("rhea"));
    assert!(!rhea.enabled);
}

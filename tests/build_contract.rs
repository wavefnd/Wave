// SPDX-License-Identifier: MPL-2.0
use std::{
    fs,
    io::Write,
    path::PathBuf,
    process::{Command, Output, Stdio},
    sync::atomic::{AtomicU64, Ordering},
};
use utils::wson::{self, Value};
static NEXT: AtomicU64 = AtomicU64::new(0);
struct Case(PathBuf);
impl Case {
    fn new() -> Self {
        let root = std::env::temp_dir().join(format!(
            "wave-build-contract-{}-{}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir_all(&root).unwrap();
        Self(root.canonicalize().unwrap())
    }

    fn package(&self, name: &str, source: &str) -> PathBuf {
        let path = self.0.join(name);
        fs::create_dir_all(path.join("src")).unwrap();
        fs::write(path.join("src/lib.wave"), source).unwrap();
        path
    }

    fn node(id: char, name: &str, root: PathBuf, edges: &[(&str, char)]) -> Value {
        Value::object([
            ("id", Value::string(id.to_string().repeat(64))),
            ("name", Value::string(name)),
            ("root", Value::string(root.to_str().unwrap())),
            (
                "dependencies",
                Value::object(
                    edges
                        .iter()
                        .map(|(alias, id)| (*alias, Value::string(id.to_string().repeat(64)))),
                ),
            ),
        ])
    }

    fn graph(&self) -> Value {
        let app = self.package("app", "export(c) fun wave_add(a:i32,b:i32)->i32{return a+b;}");
        fs::write(app.join("src/main.wave"),"import(\"left\")::{left}; import(\"right\")::{right}; fun main(){ println(\"{}\", left()+right()); }").unwrap();
        let left =
            self.package("left", "import(\"dep\")::{value}; pub fun left()->i32{return value();}");
        let right = self
            .package("right", "import(\"dep\")::{value}; pub fun right()->i32{return value();}");
        let one =
            self.package("one", "pub fun value()->i32{return 11;} fun secret()->i32{return 1;}");
        let two = self.package("two", "pub import(\"./leaf.wave\")::{value};");
        fs::write(two.join("src/leaf.wave"), "pub fun value()->i32{return 31;}").unwrap();
        Value::object([
            ("schema_version", Value::integer(1)),
            ("root", Value::string("a".repeat(64))),
            (
                "packages",
                Value::Array(vec![
                    Self::node('a', "app", app, &[("left", 'b'), ("right", 'c')]),
                    Self::node('b', "left", left, &[("dep", 'd')]),
                    Self::node('c', "right", right, &[("dep", 'e')]),
                    Self::node('d', "math", one, &[]),
                    Self::node('e', "math", two, &[]),
                ]),
            ),
        ])
    }

    fn run(&self, graph: &Value, args: &[&str]) -> Output {
        let mut child = Command::new(env!("CARGO_BIN_EXE_wavec"))
            .current_dir(&self.0)
            .arg("--package-graph-stdin")
            .args(args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap();
        let bytes = wson::dumps(graph, wson::Format::Json, false).unwrap();
        child.stdin.take().unwrap().write_all(bytes.as_bytes()).unwrap();
        child.wait_with_output().unwrap()
    }
}
impl Drop for Case {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}
fn success(output: Output) -> Output {
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    output
}

#[test]
fn conflicting_transitive_aliases_reexports_and_visibility_use_package_context() {
    let c = Case::new();
    let graph = c.graph();
    for opt in ["-O0", "-O2"] {
        let exe = c.0.join(format!("program{}", std::env::consts::EXE_SUFFIX));
        success(c.run(&graph, &["build", "app/src/main.wave", opt, "-o", exe.to_str().unwrap()]));
        let run = success(Command::new(&exe).output().unwrap());
        assert_eq!(String::from_utf8(run.stdout).unwrap().trim(), "42");
    }
    fs::write(
        c.0.join("left/src/lib.wave"),
        "import(\"dep\")::{secret}; pub fun left()->i32{return secret();}",
    )
    .unwrap();
    let error = c.run(&graph, &["check", "app/src/main.wave", "--error-format=json"]);
    assert!(!error.status.success());
    assert!(String::from_utf8_lossy(&error.stderr).contains("private"));
}

#[test]
fn malformed_graph_fails_before_replacing_outputs() {
    let c = Case::new();
    let mut graph = c.graph();
    fs::write(c.0.join("previous.o"), b"last good object").unwrap();
    if let Value::Object(fields) = &mut graph {
        fields.iter_mut().find(|(k, _)| k == "root").unwrap().1 = Value::string("f".repeat(64));
    }
    let error = c.run(&graph, &["build", "app/src/lib.wave", "--emit=obj", "-o", "previous.o"]);
    assert!(!error.status.success());
    assert_eq!(fs::read(c.0.join("previous.o")).unwrap(), b"last good object");
    assert!(String::from_utf8_lossy(&error.stderr).contains("missing root package"));
}

#[test]
fn logical_package_paths_produce_identical_objects_from_distinct_directories() {
    let first = Case::new();
    let second = Case::new();
    let mut a = first.graph();
    let mut b = second.graph();
    for graph in [&mut a, &mut b] {
        if let Value::Object(fields) = graph {
            fields.iter_mut().find(|(k, _)| k == "root").unwrap().1 = Value::string("b".repeat(64));
        }
    }

    for opt in ["-O0", "-O2"] {
        success(
            first.run(&a, &["build", "left/src/lib.wave", "--emit=obj", opt, "-o", "module.o"]),
        );
        success(
            second.run(&b, &["build", "left/src/lib.wave", "--emit=obj", opt, "-o", "module.o"]),
        );
        assert_eq!(
            fs::read(first.0.join("module.o")).unwrap(),
            fs::read(second.0.join("module.o")).unwrap()
        );
    }
    let a = first.graph();
    let plan = success(first.run(
        &a,
        &["build", "app/src/main.wave", "--dry-run", "--error-format=json", "-o", "planned"],
    ));
    let value = wson::parse_json(std::str::from_utf8(&plan.stdout).unwrap()).unwrap();
    assert_eq!(value.get("package_graph"), Some(&a));
    assert!(value.get_arr("source_inputs").unwrap().len() >= 6);
    assert!(!first.0.join("planned").exists());
}

#[test]
fn scoped_graph_rejects_wrong_input_owner_cycles_and_ambiguous_bindings() {
    let c = Case::new();
    let graph = c.graph();
    fs::write(c.0.join("prior.o"), b"previous generation").unwrap();
    let failure = c.run(&graph, &["build", "left/src/lib.wave", "--emit=obj", "-o", "prior.o"]);
    assert!(!failure.status.success());
    assert_eq!(fs::read(c.0.join("prior.o")).unwrap(), b"previous generation");
    let raw = wson::dumps(&graph, wson::Format::Json, false).unwrap();
    for invalid in [
        raw.replacen("\"schema_version\":1", "\"schema_version\":2", 1),
        raw.replacen(
            "\"dependencies\":{",
            &format!("\"dependencies\":{{\"loop\":\"{}\",", "a".repeat(64)),
            1,
        ),
        raw.replacen("\"name\":\"app\"", "\"name\":\"app\",\"name\":\"other\"", 1),
    ] {
        assert_ne!(invalid, raw);
        assert!(utils::build_contract::PackageGraph::parse(&invalid).is_err());
    }
    assert!(utils::build_contract::configure_paths(vec![(
        c.0.clone(),
        "/logical/../escape".into()
    )])
    .is_err());
}

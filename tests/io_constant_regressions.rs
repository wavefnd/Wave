use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Output, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};

static NEXT: AtomicUsize = AtomicUsize::new(0);

fn compile(source: &str, optimization: &str) -> (PathBuf, String) {
    let dir = std::env::temp_dir().join(format!(
        "wave-io-{}-{}",
        std::process::id(),
        NEXT.fetch_add(1, Ordering::Relaxed)
    ));
    fs::create_dir_all(&dir).unwrap();
    let file = dir.join("case.wave");
    fs::write(&file, source).unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_wavec"))
        .args(["build"])
        .arg(&file)
        .arg(optimization)
        .arg("--emit=ir,obj,bin")
        .arg("--out-dir")
        .arg(&dir)
        .output()
        .unwrap();
    assert!(output.status.success(), "{}\n{source}", String::from_utf8_lossy(&output.stderr));
    let ir = fs::read_to_string(dir.join("case.ll")).unwrap();
    (dir, ir)
}

fn execute(dir: &std::path::Path, input: &str) -> Output {
    let mut child = Command::new(dir.join(format!("case{}", std::env::consts::EXE_SUFFIX)))
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    child.stdin.take().unwrap().write_all(input.as_bytes()).unwrap();
    child.wait_with_output().unwrap()
}

fn run_both(source: &str, input: &str, expected: &str) {
    for opt in ["-O0", "-O2"] {
        let (dir, _) = compile(source, opt);
        let output = execute(&dir, input);
        assert!(output.status.success(), "{opt}: {output:?}");
        assert_eq!(
            String::from_utf8(output.stdout).unwrap().replace("\r\n", "\n"),
            expected,
            "{opt}"
        );
        fs::remove_dir_all(dir).unwrap();
    }
}

#[test]
fn output_preserves_percent_braces_bytes_and_semantic_types() {
    run_both(
        r#"
fun text() -> str { return "hello"; }
fun small() -> i8 { return -128; }
fun main() {
    print("100% %s %n done; }");
    println(" {} {", 7);
    println("{} {} {d} {x} {c} {s}", text(), small(), 4294967295 as u32, 65535 as u16, 'A', text());
    println("\xC3\xA9 {}", true);
}
"#,
        "",
        "100% %s %n done; } 7 {\nhello -128 4294967295 ffff A hello\né 1\n",
    );
}

// Expected decimal values are calculated independently of the compiler's
// literal conversion: double decimal digits to form powers of two.
fn power_of_two(bits: usize) -> String {
    let mut digits = vec![1u8];
    for _ in 0..bits {
        let mut carry = 0;
        for digit in &mut digits {
            let n = *digit * 2 + carry;
            *digit = n % 10;
            carry = n / 10;
        }
        if carry != 0 {
            digits.push(carry);
        }
    }
    digits.iter().rev().map(|n| char::from(b'0' + n)).collect()
}

fn minus_one(value: &str) -> String {
    let mut bytes = value.as_bytes().to_vec();
    for digit in bytes.iter_mut().rev() {
        if *digit > b'0' {
            *digit -= 1;
            break;
        }
        *digit = b'9';
    }
    String::from_utf8(bytes).unwrap().trim_start_matches('0').to_string()
}

#[test]
fn every_integer_width_prints_and_scans_exact_boundaries() {
    let mut source = String::from("fun main() -> i32 {\n");
    let mut input = String::new();
    let mut expected = String::new();
    for bits in [8, 16, 32, 64, 128, 256, 512, 1024] {
        let min = format!("-{}", power_of_two(bits - 1));
        let max = minus_one(&power_of_two(bits - 1));
        let umax = minus_one(&power_of_two(bits));
        source.push_str(&format!(
            r#"
var s{bits}: i{bits} = {min}; var unsigned{bits}: u{bits} = {umax};
println("{{}} {{}} {{x}}", s{bits}, unsigned{bits}, unsigned{bits});
input("{{}} {{}}", s{bits}, unsigned{bits});
if (s{bits} != {max} || unsigned{bits} != 1) {{ return 1; }}
println("{{}} {{}}", s{bits}, unsigned{bits});
input("{{}} {{}}", s{bits}, unsigned{bits});
if (s{bits} != {min} || unsigned{bits} != {umax}) {{ return 2; }}
println("{{}} {{}}", s{bits}, unsigned{bits});
"#
        ));
        input.push_str(&format!("+{max} {}1\n{min} {umax}\n", "0".repeat(330)));
        expected
            .push_str(&format!("{min} {umax} {}\n{max} 1\n{min} {umax}\n", "f".repeat(bits / 4)));
    }
    source.push_str("return 0; }");
    run_both(&source, &input, &expected);
}

#[test]
fn input_preserves_neighbors_and_evaluates_addresses_once() {
    run_both(
        r#"
struct Flags { before: u8; flag: bool; after: u8; }
static calls: i32 = 0;
fun index() -> i32 { calls += 1; return 1; }
fun main() -> i32 {
    var flags: Flags = Flags { before: 65, flag: false, after: 66 };
    var values: array<i128, 3> = [11, 22, 33];
    var letter: char = 'A'; var number: f64 = 0.0;
    input("{}%,{};{}:{}", flags.flag, values[index()], letter, number);
    if (flags.before != 65 || flags.after != 66 || !flags.flag || calls != 1) { return 1; }
    if (values[0] != 11 || values[1] != 18446744073709551616 || values[2] != 33) { return 2; }
    if (letter != 'Z' || number != 2.5) { return 3; }
    input("{}", flags.flag);
    if (flags.flag) { return 4; }
    return 0;
}
"#,
        "1%,18446744073709551616;Z:2.5 0",
        "",
    );
}

#[test]
fn invalid_integer_input_fails_without_executing_the_next_statement() {
    for (ty, invalid) in [
        ("bool", vec!["2", "-1", "10", "+", "", "true"]),
        ("u8", vec!["256", "-1", "99999999999999999999999"]),
        ("i8", vec!["128", "-129", "--1", "+ 1"]),
        ("u128", vec!["340282366920938463463374607431768211456"]),
    ] {
        let initial = if ty == "bool" { "false" } else { "0" };
        for opt in ["-O0", "-O2"] {
            let (dir, _) = compile(&format!("fun main() {{ var n: {ty} = {initial}; input(\"{{}}\", n); println(\"continued\"); }}"), opt);
            for input in &invalid {
                let output = execute(&dir, input);
                assert_eq!(output.status.code(), Some(1), "{ty} {input}: {output:?}");
                assert!(output.stdout.is_empty(), "{ty} {input}: {output:?}");
            }
            fs::remove_dir_all(dir).unwrap();
        }
    }
}

#[test]
fn global_scalar_and_aggregate_constants_match_runtime_conversions() {
    run_both(
        r#"
struct Data { text: str; letter: char; count: u64; }
const even: bool = 2 as bool;
const odd: bool = 3 as bool;
const nested_bool: i32 = (2 as bool) as i32;
const small: u8 = 255;
const widened: u64 = (small) as u64;
const implicit: u64 = small;
const signed: i8 = -1;
const signed_wide: i1024 = signed as i1024;
const wide: u256 = 340282366920938463463374607431768211456;
const wider: u1024 = wide as u1024;
const floating: f64 = 2147483648.0;
const unsigned_float: u32 = floating as u32;
const signed_float: i32 = -3.75 as i32;
const to_float: f64 = small as f64;
const data: Data = Data { text: "hello", letter: 'Z', count: small };
static text: str = "world";
static letter: char = 'A';
const strings: array<str, 2> = ["one", "two"];
fun main() -> i32 {
    var local: u32 = floating as u32;
    var local_even: bool = 2 as bool; var local_odd: bool = 3 as bool;
    if ((even as i32) != (local_even as i32) || (odd as i32) != (local_odd as i32) || nested_bool != 1 || !even || !odd) { return 3; }
    if (widened != 255 || implicit != 255 || signed_wide != -1 || wider != wide) { return 1; }
    if (unsigned_float != local || signed_float != -3 || to_float != 255.0) { return 2; }
    println("{} {c} {} {} {c} {} {}", data.text, data.letter, data.count, text, letter, strings[0], strings[1]);
    return 0;
}
"#,
        "",
        "hello Z 255 world A one two\n",
    );
}

#[test]
fn methods_keep_defaults_and_distinct_symbols_through_nested_calls() {
    run_both(
        r#"
struct S {
    x: i32;
    fun value(self: S, extra: i32 = 2) -> i32 { return self.x + extra; }
    fun identity(self: ptr<S>, unused: i32 = 0) -> ptr<S> { return self; }
    fun generic<T>(self: ptr<S>, arg: T, extra: i32 = 4) -> i32 { return self.x + extra; }
}
proto S { fun proto_value(self: ptr<S>, extra: i32 = 3) -> i32 { return self.x + extra; } }
fun S_value(self: S) -> i32 { return 99; }
fun free(self: S, extra: i32 = 7) -> i32 { return self.x + extra; }
struct A_B { fun c(self: A_B) -> i32 { return 1; } }
struct A { fun B_c(self: A) -> i32 { return 2; } }
fun main() -> i32 {
    var s: S = S { x: 3 }; var a: A_B = A_B {}; var b: A = A {};
    if (s.value() != 5 || s.value(5) != 8 || S_value(s) != 99 || s.free() != 10) { return 1; }
    if ((&s).identity().proto_value() != 6 || (&s).generic<i64>(8) != 7) { return 2; }
    if (a.c() != 1 || b.B_c() != 2) { return 3; }
    return 0;
}
"#,
        "",
        "",
    );
}

#[test]
fn integer_matches_preserve_wide_patterns_and_all_control_flow_exits() {
    run_both(
        r#"
const ONE: u64 = 1;
const NEG: i8 = -1;
fun classify(n: u8) -> i32 { match (n) { ONE => { return 0; } _ => { return 1; } } }
fun wide(n: u256) -> i32 { match (n) { 340282366920938463463374607431768211456 => { return 0; } _ => { return 1; } } }
fun main() -> i32 {
    if (classify(1) != 0 || classify(2) != 1 || wide(340282366920938463463374607431768211456) != 0) { return 1; }
    var n: i8 = -1;
    match (n) { NEG => {} _ => { return 2; } }
    var i: i32 = 0;
    while (i < 5) {
        i += 1;
        match (i) { 1 => { continue; } 2 => { break; } _ => { return 3; } }
    }
    match (i) { 1 => { return 4; } }
    if (i != 2) { return 5; }
    return 0;
}
"#,
        "",
        "",
    );
}

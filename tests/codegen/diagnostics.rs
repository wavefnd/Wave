// SPDX-License-Identifier: MPL-2.0
//! Compiler and CLI diagnostics regression cases.
//! These tests share the parent integration suite's fixtures and target gates.
use super::*;

#[test]
fn incompatible_std_is_rejected_from_an_isolated_home() {
    let dir = temp_case_dir("std-compatibility-home");
    let home = dir.join("home");
    let std_root = home.join(".wave/lib/wave/std");
    fs::create_dir_all(std_root.join("string")).unwrap();
    fs::write(std_root.join("manifest.json"), r#"{"name":"std","compatibility_revision":0}"#)
        .unwrap();
    fs::write(std_root.join("string/len.wave"), "pub fun len(value: str) -> i64 { return 0; }\n")
        .unwrap();
    let source = write_wave(
        &dir,
        "main.wave",
        "import(\"std::string::len\")::{len};\nfun main() { len(\"x\"); }\n",
    );

    let output = wavec_command().env("HOME", &home).arg("check").arg(&source).output().unwrap();
    assert!(!output.status.success());
    let error = String::from_utf8_lossy(&output.stderr);
    assert!(error.contains("installed std compatibility revision 0"), "{error}");
    assert!(
        error.contains(&format!("requires {}", parser::import::STD_COMPATIBILITY_REVISION)),
        "{error}"
    );
    assert!(error.contains("wavec update std"), "{error}");
}

#[test]
fn json_path_matching_accepts_unix_and_escaped_windows_separators() {
    let components = ["crt", "riscv64-unknown-linux-gnu", "crt1.o"];
    assert!(json_contains_path_components(
        r#"{"args":["/opt/wave/crt/riscv64-unknown-linux-gnu/crt1.o"]}"#,
        &components
    ));
    assert!(json_contains_path_components(
        r#"{"args":["C:\\wave\\crt\\riscv64-unknown-linux-gnu\\crt1.o"]}"#,
        &components
    ));
    assert!(json_contains_path_components(
        r#"{"args":["D:\\sysroot\\usr/lib\\crt1.o"]}"#,
        &["sysroot", "usr", "lib", "crt1.o"]
    ));
    assert!(json_contains_path_value(
        r#"{"args":["-LD:\\wave\\sysroot\\lib"]}"#,
        r#"-LD:\wave\sysroot\lib"#
    ));
}

#[test]
fn retired_let_declarations_are_rejected() {
    let dir = temp_case_dir("retired-let-syntax");
    let cases = [
        ("let.wave", "fun main() { let value: i32 = 1; }\n"),
        ("let_mut.wave", "fun main() { let mut value: i32 = 1; value += 1; }\n"),
        ("for_let.wave", "fun main() { for (let index: i32 = 0; index < 1; index += 1) {} }\n"),
    ];

    for (file_name, source) in cases {
        let source = write_wave(&dir, file_name, source);
        let error = run_wavec_expect_failure([OsStr::new("check"), source.as_os_str()]);
        assert!(error.contains("error[E2001]"), "{}: {}", file_name, error);
        assert!(error.contains("Let (`let`)"), "{}: {}", file_name, error);
    }
}

#[test]
fn noninteger_index_results_report_source_diagnostics() {
    let dir = temp_case_dir("noninteger-index-results");
    for (name, ty, value) in [("float", "f64", "1.0"), ("text", "str", "\"bad\"")] {
        let source = write_wave(&dir, &format!("{name}.wave"), &format!(
            "fun select() -> {ty} {{ return {value}; }}\nfun main() {{\n    var values: array<i32, 2> = [1, 2];\n    println(\"{{}}\", values[select()]);\n}}\n"
        ));
        for format in ["--error-format=human", "--error-format=json"] {
            let output =
                run_wavec_raw([OsStr::new("check"), source.as_os_str(), OsStr::new(format)]);
            assert!(!output.status.success());
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("E3001") && stderr.contains("index expression must be an integer"),
                "{stderr}"
            );
            assert!(!stderr.contains("E9001"), "{stderr}");
            if format.ends_with("json") {
                assert!(stderr.contains("\"line\":4"), "{stderr}");
            } else {
                assert!(stderr.contains(&format!("{name}.wave:4:")), "{stderr}");
            }
        }
    }
}

#[test]
fn import_graph_rejects_cycles_path_escape_and_public_main() {
    let dir = temp_case_dir("module-import-errors");
    fs::write(dir.join("a.wave"), "import(\"./b\");\npub fun from_a() -> i32 { return 1; }\n")
        .unwrap();
    fs::write(dir.join("b.wave"), "import(\"./a\");\npub fun from_b() -> i32 { return 2; }\n")
        .unwrap();
    let cycle = write_wave(&dir, "cycle.wave", "import(\"./a\");\nfun main() {}\n");
    let error = run_wavec_expect_failure([OsStr::new("check"), cycle.as_os_str()]);
    assert!(error.contains("import cycle detected"), "{}", error);

    let escape = write_wave(&dir, "escape.wave", "import(\"./../outside\");\nfun main() {}\n");
    let error = run_wavec_expect_failure([OsStr::new("check"), escape.as_os_str()]);
    assert!(error.contains("escapes its module directory"), "{}", error);

    let package_escape =
        write_wave(&dir, "package_escape.wave", "import(\"add::..\");\nfun main() {}\n");
    let error = run_wavec_expect_failure([OsStr::new("check"), package_escape.as_os_str()]);
    assert!(error.contains("package and module names must be identifiers"), "{}", error);

    let public_main = write_wave(&dir, "public_main.wave", "pub fun main() {}\n");
    let error = run_wavec_expect_failure([OsStr::new("check"), public_main.as_os_str()]);
    assert!(error.contains("entry function `main` cannot be public"), "{}", error);

    fs::write(dir.join("library_main.wave"), "fun main() {}\n").unwrap();
    let imported_main =
        write_wave(&dir, "imported_main.wave", "import(\"./library_main\");\nfun main() {}\n");
    let error = run_wavec_expect_failure([OsStr::new("check"), imported_main.as_os_str()]);
    assert!(
        error.contains("function 'main' may only be declared in the entry module"),
        "{}",
        error
    );
}

#[test]
fn semantic_validation_rejects_invalid_returns_calls_and_loop_control() {
    let dir = temp_case_dir("semantic-validation");
    let cases = [
        (
            "wrong_return_type.wave",
            r#"
fun value() -> i32 {
    return "not an integer";
}

"#,
            "type mismatch in return value of function `value`",
        ),
        (
            "value_from_void.wave",
            r#"
fun noop() {
    return 1;
}
"#,
            "void function `noop` cannot return a value",
        ),
        (
            "empty_non_void_return.wave",
            r#"
fun value() -> i32 {
    return;
}
"#,
            "non-void function `value` must return `i32`",
        ),
        (
            "missing_return_path.wave",
            r#"
fun value(flag: bool) -> i32 {
    if (flag) {
        return 1;
    }
}
"#,
            "non-void function `value` may exit without returning `i32`",
        ),
        (
            "break_outside_loop.wave",
            r#"
fun main() {
    break;
}
"#,
            "`break` can only be used inside a loop",
        ),
        (
            "continue_outside_loop.wave",
            r#"
fun main() {
    continue;
}
"#,
            "`continue` can only be used inside a loop",
        ),
        (
            "wrong_call_type.wave",
            r#"
fun identity(value: i32) -> i32 {
    return value;
}

fun main() {
    identity("text");
}
"#,
            "type mismatch in argument 1 of function `identity`",
        ),
        (
            "wrong_main_return.wave",
            r#"
fun main() -> f64 {
    return 0.0;
}
"#,
            "entry function `main` must return `i32` or omit its return type",
        ),
        (
            "generic_main.wave",
            r#"
fun main<T>() {}
"#,
            "entry function `main` cannot declare generic parameters",
        ),
    ];

    for (file_name, source, expected) in cases {
        let source = write_wave(&dir, file_name, source);
        for mode in ["check", "build"] {
            let output = if mode == "check" {
                run_wavec_raw([OsStr::new("check"), source.as_os_str()])
            } else {
                run_wavec_raw([
                    OsStr::new("build"),
                    source.as_os_str(),
                    OsStr::new("--emit=obj"),
                    OsStr::new("--out-dir"),
                    dir.as_os_str(),
                ])
            };
            assert!(
                !output.status.success(),
                "{} unexpectedly passed semantic validation in {} mode",
                file_name,
                mode
            );
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(stderr.contains("error[E3001]"), "{} ({}): {}", file_name, mode, stderr);
            assert!(stderr.contains(expected), "{} ({}): {}", file_name, mode, stderr);
            assert!(
                !stderr.contains("E9001") && !stderr.contains("compiler internal error"),
                "{} ({}) leaked a backend failure: {}",
                file_name,
                mode,
                stderr
            );
        }
    }
}

#[test]
fn semantic_validation_rejects_backend_only_type_failures_early() {
    let dir = temp_case_dir("semantic-backend-parity");
    let cases = [
        (
            "narrow_return.wave",
            "fun narrow(x: i64) -> i32 { return x; }\nfun main() {}\n",
            "expected `i32`, found `i64`",
        ),
        (
            "narrow_call.wave",
            "fun take(x: i32) {}\nfun main() { var wide: i64 = 1; take(wide); }\n",
            "argument 1 of function `take`",
        ),
        (
            "narrow_initializer.wave",
            "fun main() { var wide: i64 = 1; var narrow: i32 = wide; }\n",
            "initializer for `narrow`",
        ),
        (
            "narrow_assignment.wave",
            "fun main() { var wide: i64 = 1; var narrow: i32 = 0; narrow = wide; }\n",
            "assignment to `narrow`",
        ),
        (
            "unknown_function.wave",
            "fun main() { missing_function(); }\n",
            "call to unknown function `missing_function`",
        ),
        (
            "unknown_field.wave",
            "struct Point { x: i32; }\nfun read(p: Point) -> i32 { return p.missing; }\nfun main() {}\n",
            "struct `Point` has no field `missing`",
        ),
        (
            "unknown_method.wave",
            "struct Point { x: i32; }\nfun main() { var p: Point = Point { x: 1 }; p.missing(); }\n",
            "struct `Point` has no method `missing`",
        ),
        (
            "unknown_struct_literal_field.wave",
            "struct Point { x: i32; }\nfun make() -> Point { return Point { missing: 1 }; }\nfun main() {}\n",
            "struct `Point` has no field `missing`",
        ),
        (
            "missing_struct_literal_field.wave",
            "struct Point { x: i32; y: i32; }\nfun main() { var p: Point = Point { x: 1 }; }\n",
            "struct literal `Point` is missing field(s): y",
        ),
        (
            "array_return.wave",
            "fun values() -> i32 { return [1, 2]; }\nfun main() {}\n",
            "found `array literal`",
        ),
        (
            "array_element.wave",
            "fun main() { var values: array<i32, 1> = [\"text\"]; }\n",
            "element 0 of initializer for `values`",
        ),
        (
            "invalid_condition.wave",
            "struct Flag { value: i32; }\nfun main() { var flag: Flag = Flag { value: 1 }; if (flag) {} }\n",
            "if condition must be bool, numeric, pointer, or string",
        ),
        (
            "invalid_match.wave",
            "struct Value { x: i32; }\nfun main() { var v: Value = Value { x: 1 }; match (v) { _ => {} } }\n",
            "match value must be an integer, enum, or variant",
        ),
        (
            "invalid_deref.wave",
            "fun main() { var value: i32 = 1; println(\"{}\", deref value); }\n",
            "deref expects a pointer",
        ),
        (
            "invalid_index_target.wave",
            "fun main() { var value: i32 = 1; println(\"{}\", value[0]); }\n",
            "index access requires an array or pointer",
        ),
        (
            "invalid_index_type.wave",
            "fun main() { var values: array<i32, 1> = [1]; println(\"{}\", values[\"text\"]); }\n",
            "index expression must be an integer",
        ),
        (
            "invalid_unary.wave",
            "fun main() { println(\"{}\", -\"text\"); }\n",
            "unary operator `Neg` is not supported for `str`",
        ),
        (
            "invalid_increment.wave",
            "fun main() { var flag: bool = true; flag++; }\n",
            "++/-- requires a numeric or pointer lvalue",
        ),
        (
            "invalid_export.wave",
            "export(rust) fun exposed() -> i32 { return 1; }\nfun main() {}\n",
            "unsupported export ABI 'rust'",
        ),
    ];

    for (file_name, source, expected) in cases {
        let source = write_wave(&dir, file_name, source);
        for mode in ["check", "build"] {
            let output = if mode == "check" {
                run_wavec_raw([OsStr::new("check"), source.as_os_str()])
            } else {
                run_wavec_raw([
                    OsStr::new("build"),
                    source.as_os_str(),
                    OsStr::new("--emit=obj"),
                    OsStr::new("--out-dir"),
                    dir.as_os_str(),
                ])
            };
            assert!(
                !output.status.success(),
                "{} unexpectedly succeeded in {} mode",
                file_name,
                mode
            );
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(stderr.contains("error[E3001]"), "{}: {}", file_name, stderr);
            assert!(stderr.contains(expected), "{}: {}", file_name, stderr);
            assert!(
                !stderr.contains("E9001")
                    && !stderr.contains("compiler internal error")
                    && !stderr.contains("panic location"),
                "{} leaked a backend failure in {} mode: {}",
                file_name,
                mode,
                stderr
            );
        }
    }
}

#[test]
fn semantic_validation_rejects_mutation_in_conditions() {
    let dir = temp_case_dir("condition-mutation");
    let cases = [
        (
            "indexed_assignment_in_if.wave",
            "fun main() { var command: array<char, 2> = ['x', 'y']; if (command[0] = 'h') {} }\n",
            "assignment `=` is not allowed in if condition",
            "use `==` for comparison, or move the assignment before the condition",
        ),
        (
            "assignment_in_else_if.wave",
            "fun main() { var value: i32 = 0; if (false) {} else if (value = 1) {} }\n",
            "assignment `=` is not allowed in else-if condition",
            "use `==` for comparison, or move the assignment before the condition",
        ),
        (
            "compound_assignment_in_while.wave",
            "fun main() { var value: i32 = 0; while (value += 1) {} }\n",
            "compound assignment `+=` is not allowed in while condition",
            "move the mutation before the condition",
        ),
        (
            "assignment_in_for_condition.wave",
            "fun main() { for (var value: i32 = 0; value = 1; value += 1) {} }\n",
            "assignment `=` is not allowed in for condition",
            "use `==` for comparison, or move the assignment before the condition",
        ),
        (
            "nested_assignment_in_if.wave",
            "fun main() { var value: i32 = 0; if ((value = 1) == 1) {} }\n",
            "assignment `=` is not allowed in if condition",
            "use `==` for comparison, or move the assignment before the condition",
        ),
        (
            "increment_in_if.wave",
            "fun main() { var value: i32 = 0; if (value++) {} }\n",
            "increment or decrement `++` is not allowed in if condition",
            "move the mutation before the condition",
        ),
    ];

    for (file_name, source, expected, help) in cases {
        let source = write_wave(&dir, file_name, source);
        for mode in ["check", "build"] {
            let output = if mode == "check" {
                run_wavec_raw([OsStr::new("check"), source.as_os_str()])
            } else {
                run_wavec_raw([
                    OsStr::new("build"),
                    source.as_os_str(),
                    OsStr::new("--emit=obj"),
                    OsStr::new("--out-dir"),
                    dir.as_os_str(),
                ])
            };
            assert!(
                !output.status.success(),
                "{} unexpectedly accepted mutation in a condition in {} mode",
                file_name,
                mode
            );
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(stderr.contains("error[E3001]"), "{}: {}", file_name, stderr);
            assert!(stderr.contains(expected), "{}: {}", file_name, stderr);
            assert!(stderr.contains(help), "{}: {}", file_name, stderr);
            assert!(
                !stderr.contains("E9001") && !stderr.contains("compiler internal error"),
                "{} ({}) leaked a backend failure: {}",
                file_name,
                mode,
                stderr
            );
        }
    }
}

#[test]
fn semantic_validation_allows_mutation_outside_conditions() {
    let dir = temp_case_dir("condition-mutation-valid");
    let source = write_wave(
        &dir,
        "valid.wave",
        r#"
fun main() {
    var value: i32 = 0;
    value = 1;
    if (value == 1) {}

    while (value < 2) {
        value += 1;
    }

    for (var index: i32 = 0; index < 2; index += 1) {}
}
"#,
    );

    run_wavec([OsStr::new("check"), source.as_os_str()]);
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        dir.as_os_str(),
    ]);
}

#[test]
fn semantic_validation_preserves_pointer_depth_for_address_of() {
    let dir = temp_case_dir("address-of-pointer-depth");
    let valid = write_wave(
        &dir,
        "valid.wave",
        r#"
fun main() {
    var message: str = "Hello";
    var correct: ptr<str> = &message;
    var copied: ptr<str> = correct;
    var converted: ptr<i8> = &message as ptr<i8>;
}
"#,
    );

    run_wavec([OsStr::new("check"), valid.as_os_str()]);
    run_wavec([
        OsStr::new("build"),
        valid.as_os_str(),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        dir.as_os_str(),
    ]);

    let invalid_cases = [
        (
            "string_depth.wave",
            r#"
fun main() {
    var message: str = "Hello";
    var wrong: ptr<i8> = &message;
}
"#,
            "type mismatch in initializer for `wrong`: expected `ptr<i8>`, found `ptr<str>`",
        ),
        (
            "pointer_pointee.wave",
            r#"
fun consume(value: ptr<u8>) {}

fun main() {
    var number: i32 = 1;
    consume(&number);
}
"#,
            "type mismatch in argument 1 of function `consume`: expected `ptr<u8>`, found `ptr<i32>`",
        ),
    ];

    for (file_name, source, expected) in invalid_cases {
        let source = write_wave(&dir, file_name, source);
        for mode in ["check", "build"] {
            let output = if mode == "check" {
                run_wavec_raw([OsStr::new("check"), source.as_os_str()])
            } else {
                run_wavec_raw([
                    OsStr::new("build"),
                    source.as_os_str(),
                    OsStr::new("--emit=obj"),
                    OsStr::new("--out-dir"),
                    dir.as_os_str(),
                ])
            };
            assert!(
                !output.status.success(),
                "{} unexpectedly accepted an implicit pointer conversion in {} mode",
                file_name,
                mode
            );
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(stderr.contains("error[E3001]"), "{}: {}", file_name, stderr);
            assert!(stderr.contains(expected), "{}: {}", file_name, stderr);
            assert!(
                !stderr.contains("E9001")
                    && !stderr.contains("compiler internal error")
                    && !stderr.contains("panic location"),
                "{} leaked a backend failure in {} mode: {}",
                file_name,
                mode,
                stderr
            );
        }
    }
}

#[test]
fn second_semantic_audit_rejects_unsafe_programs_before_codegen() {
    let dir = temp_case_dir("semantic-audit-two");
    let cases = [
        (
            "array_format.wave",
            "fun main() { println(\"{}\", [1, 2]); }\n",
            "format argument must be a scalar",
        ),
        (
            "void_value.wave",
            "fun noop() {}\nfun main() { println(\"{}\", noop()); }\n",
            "found `void`",
        ),
        (
            "struct_format.wave",
            "struct Pair { x: i32; }\nfun main() { var p: Pair = Pair { x: 1 }; println(\"{}\", p); }\n",
            "found `Pair`",
        ),
        (
            "compound_string.wave",
            "fun main() { var text: str = \"a\"; text += \"b\"; }\n",
            "compound assignment `AddAssign` requires numeric operands",
        ),
        (
            "compound_bool.wave",
            "fun main() { var flag: bool = true; flag += false; }\n",
            "compound assignment `AddAssign` requires numeric operands",
        ),
        (
            "input_literal.wave",
            "fun main() { input(\"{}\", 1); }\n",
            "input argument must be a mutable lvalue",
        ),
        (
            "invalid_struct_cast.wave",
            "struct Pair { x: i32; }\nfun main() { var p: Pair = Pair { x: 1 }; var bits: i32 = p as i32; }\n",
            "invalid cast from `Pair` to `i32`",
        ),
        (
            "invalid_string_float_cast.wave",
            "fun main() { var value: f32 = \"text\" as f32; }\n",
            "invalid cast from `str` to `f32`",
        ),
        (
            "invalid_void_cast.wave",
            "fun noop() {}\nfun main() { var value: i32 = noop() as i32; }\n",
            "invalid cast from `void` to `i32`",
        ),
        (
            "unknown_cast_type.wave",
            "fun main() { var value: i32 = 1 as Missing; }\n",
            "unknown type `Missing` in cast target",
        ),
        (
            "unknown_variable_type.wave",
            "fun main() { var value: Missing; }\n",
            "unknown type `Missing` in variable `value`",
        ),
        (
            "unknown_return_type.wave",
            "fun make() -> Missing { var value: Missing; return value; }\nfun main() {}\n",
            "unknown type `Missing` in return type of function `make`",
        ),
        (
            "unknown_struct_field_type.wave",
            "struct Holder { value: Missing; }\nfun main() {}\n",
            "unknown type `Missing` in field `Holder.value`",
        ),
        (
            "unknown_alias_target.wave",
            "type Alias = Missing;\nfun main() {}\n",
            "unknown type `Missing` in type alias `Alias`",
        ),
        (
            "unknown_proto_target.wave",
            "proto Missing { fun read(self: Missing) -> i32 { return 1; } }\nfun main() {}\n",
            "proto implementation targets unknown type `Missing`",
        ),
        (
            "void_variable.wave",
            "fun main() { var value: void; }\n",
            "variable `value` cannot use the `void` type",
        ),
        (
            "void_struct_field.wave",
            "struct Invalid { value: void; }\nfun main() {}\n",
            "field `Invalid.value` cannot use the `void` type",
        ),
        (
            "cyclic_pointer_alias.wave",
            "type Loop = ptr<Loop>;\nfun main() {}\n",
            "cyclic type alias involving `Loop`",
        ),
        (
            "float_enum_repr.wave",
            "enum Invalid -> f32 { Value = 1 }\nfun main() {}\n",
            "enum `Invalid` representation must be an integer type",
        ),
        (
            "out_of_range_literal.wave",
            "fun main() { var value: i8 = 300; }\n",
            "initializer for `value`",
        ),
        (
            "negative_out_of_range_literal.wave",
            "fun main() { var value: i8 = -129; }\n",
            "initializer for `value`",
        ),
        (
            "negative_unsigned_literal.wave",
            "fun main() { var value: u8 = -1; }\n",
            "initializer for `value`",
        ),
        (
            "wrong_addressed_array_element.wave",
            "fun values() -> ptr<array<i32, 1>> { return &[\"text\"]; }\nfun main() {}\n",
            "element 0 of return value of function `values`",
        ),
        (
            "duplicate_local.wave",
            "fun main() { var value: i32 = 1; var value: i32 = 2; }\n",
            "duplicate variable declaration `value` in the same scope",
        ),
        (
            "duplicate_struct_field.wave",
            "struct Pair { x: i32; x: i64; }\nfun main() {}\n",
            "duplicate field `x` in struct `Pair`",
        ),
        (
            "duplicate_enum_variant.wave",
            "enum Mode -> i32 { Same = 1, Same = 2 }\nfun main() {}\n",
            "duplicate variant `Same` in enum `Mode`",
        ),
        (
            "duplicate_method.wave",
            "struct Pair { x: i32; }\nproto Pair { fun read(self: Pair) -> i32 { return self.x; } fun read(self: Pair) -> i32 { return self.x; } }\nfun main() {}\n",
            "duplicate method `Pair.read`",
        ),
        (
            "duplicate_match_value.wave",
            "enum Mode -> i32 { First = 1, Second = 1 }\nfun main() { var mode: Mode = First; match (mode) { First => {} Second => {} } }\n",
            "duplicate match case pattern `value:1`",
        ),
        (
            "mixed_float_widths.wave",
            "fun add(left: f32, right: f64) -> f32 { return left + right; }\nfun main() {}\n",
            "mixed float widths require an explicit cast",
        ),
    ];

    for (file_name, source, expected) in cases {
        let source = write_wave(&dir, file_name, source);
        for mode in ["check", "build"] {
            let output = if mode == "check" {
                run_wavec_raw([OsStr::new("check"), source.as_os_str()])
            } else {
                run_wavec_raw([
                    OsStr::new("build"),
                    source.as_os_str(),
                    OsStr::new("--emit=obj"),
                    OsStr::new("--out-dir"),
                    dir.as_os_str(),
                ])
            };
            assert!(
                !output.status.success(),
                "{} unexpectedly succeeded in {} mode",
                file_name,
                mode
            );
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(stderr.contains("error[E3001]"), "{}: {}", file_name, stderr);
            assert!(stderr.contains(expected), "{}: {}", file_name, stderr);
            assert!(
                !stderr.contains("E9001")
                    && !stderr.contains("compiler internal error")
                    && !stderr.contains("panic location"),
                "{} leaked a backend failure in {} mode: {}",
                file_name,
                mode,
                stderr
            );
        }
    }
}

#[test]
fn second_semantic_audit_preserves_explicit_and_contextual_valid_cases() {
    let dir = temp_case_dir("semantic-audit-two-valid");
    let source = write_wave(
        &dir,
        "valid.wave",
        r#"
enum Mode -> i32 {
    First = 1,
    Alias = 1
}

fun noop() {}

fun add(left: f32, right: f64) -> f32 {
    return left + (right as f32);
}

fun main() {
    noop();
    var minimum: i8 = -128;
    var bit_pattern: i8 = 0xFF;
    var unsigned_max: u128 = 340282366920938463463374607431768211455;
    var explicit: i8 = 300 as i8;
    var values: ptr<array<i32, 2>> = &[1, 2];
    var input_value: i32 = 0;
    if (true) {
        var minimum: i32 = 1;
        println("{}", minimum);
    }
    println("{} {} {} {} {}", minimum, bit_pattern, unsigned_max, explicit, values);
}
"#,
    );

    run_wavec([OsStr::new("check"), source.as_os_str()]);
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--emit=obj"),
        OsStr::new("--out-dir"),
        dir.as_os_str(),
    ]);
}

#[test]
fn semantic_diagnostics_point_at_the_relevant_source() {
    let dir = temp_case_dir("semantic-source-position");
    let source = write_wave(
        &dir,
        "wrong_return.wave",
        r#"
fun value() -> i32 {
    return "text";
}
"#,
    );
    let output = run_wavec_raw([OsStr::new("check"), source.as_os_str()]);
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("wrong_return.wave:3:12"), "{}", stderr);
    assert!(stderr.contains("return \"text\";"), "{}", stderr);

    let repeated_return = write_wave(
        &dir,
        "repeated_return.wave",
        r#"
fun value(flag: bool) -> i32 {
    if (flag) {
        return 1;
    }
    return "text";
}
"#,
    );
    let output = run_wavec_raw([OsStr::new("check"), repeated_return.as_os_str()]);
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("repeated_return.wave:6:12"), "{}", stderr);
    assert!(stderr.contains("return \"text\";"), "{}", stderr);

    let duplicate = write_wave(
        &dir,
        "duplicate.wave",
        r#"
fun other() {
    var value: i32 = 0;
}

fun main() {
    var value: i32 = 1;
    var value: i32 = 2;
}
"#,
    );
    let output = run_wavec_raw([OsStr::new("check"), duplicate.as_os_str()]);
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("duplicate.wave:8:9"), "{}", stderr);
    assert!(stderr.contains("var value: i32 = 2;"), "{}", stderr);

    let duplicate_type = write_wave(
        &dir,
        "duplicate_type.wave",
        "struct Item { first: i32; }\nstruct Item { second: i32; }\nfun main() {}\n",
    );
    let output = run_wavec_raw([OsStr::new("check"), duplicate_type.as_os_str()]);
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("duplicate_type.wave:2:8"), "{}", stderr);
    assert!(stderr.contains("struct Item { second: i32; }"), "{}", stderr);

    let imported = dir.join("broken.wave");
    fs::write(&imported, "fun broken() {\n    var value: Missing = 1;\n}\n").unwrap();
    let entry = write_wave(&dir, "import_main.wave", "import(\"./broken\");\n\nfun main() {}\n");
    let output = run_wavec_raw([OsStr::new("check"), entry.as_os_str()]);
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("broken.wave:2:16"), "{}", stderr);
    assert!(stderr.contains("var value: Missing = 1;"), "{}", stderr);
    assert!(!stderr.contains("import_main.wave:1:1"), "{}", stderr);

    let generic_import = dir.join("generic_broken.wave");
    fs::write(
        &generic_import,
        "struct Box<T> { value: T; }\nfun broken() {\n    var value: Box<i32, i64>;\n}\n",
    )
    .unwrap();
    let generic_entry = write_wave(
        &dir,
        "generic_import_main.wave",
        "import(\"./generic_broken\");\n\nfun main() {}\n",
    );
    let output = run_wavec_raw([OsStr::new("check"), generic_entry.as_os_str()]);
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("generic_broken.wave:3:16"), "{}", stderr);
    assert!(stderr.contains("type `Box` expects 1 generic argument(s), found 2"), "{}", stderr);
    assert!(!stderr.contains("generic monomorphization failed"), "{}", stderr);
}

#[test]
fn semantic_validation_accepts_complete_returns_and_explicit_casts() {
    let dir = temp_case_dir("semantic-validation-valid");
    let source = write_wave(
        &dir,
        "valid.wave",
        r#"
struct Pair {
    x: i32;
    y: i32;
}

fun choose(flag: bool) -> i32 {
    if (flag) {
        return 1;
    } else {
        return 2;
    }
    println("unreachable");
}

fun dead_after_return() {
    return;
    println("unreachable");
}

fun widen(value: i32) -> i64 {
    return value;
}

fun narrow_explicitly(value: i64) -> i32 {
    return value as i32;
}

fun values() -> array<i32, 2> {
    return [1, 2];
}

fun pointer_bits() -> i64 {
    return "text" as i64;
}

fun infinite() -> i32 {
    while (true) {
        continue;
    }
}

fun main() -> i32 {
    var value: i32 = choose(true);
    var bits: i64 = pointer_bits();
    var pair: Pair = Pair { x: 1, y: 2 };
    var items: array<i32, 2> = values();
    var pointer: ptr<Pair> = &pair;
    if (pointer) {
        if (value == 1 && bits != 0 && items[0] == 1 && widen(pair.y) == 2) {
            return narrow_explicitly(0);
        }
    }
    return infinite();
}
"#,
    );
    let out_dir = dir.join("out");
    run_wavec([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        out_dir.as_os_str(),
    ]);
}

#[test]
fn vex_cli_print_json_contracts_are_machine_readable() {
    let target = frontend_target();
    let spec = llvm::codegen::target::target_spec_for_triple(target).unwrap();
    let (stdout, stderr) = run_wavec_capture([
        OsStr::new("print"),
        OsStr::new("target-spec"),
        OsStr::new("--target"),
        OsStr::new(target),
        OsStr::new("--format=json"),
    ]);
    assert!(stderr.trim().is_empty(), "unexpected stderr:\n{}", stderr);
    let json = stdout.trim();
    assert!(json.starts_with('{') && json.ends_with('}'), "{}", json);
    assert!(json.contains(&format!("\"triple\":\"{}\"", target)), "{}", json);
    assert!(
        json.contains(&format!("\"arch\":\"{}\"", target.split('-').next().unwrap())),
        "{}",
        json
    );
    assert!(json.contains(&format!("\"os\":\"{}\"", spec.os)), "{}", json);
    assert!(json.contains(&format!("\"env\":\"{}\"", spec.env)), "{}", json);
    assert!(json.contains(&format!("\"object_format\":\"{}\"", spec.object_format)), "{}", json);
    assert!(json.contains("\"default_linker\":"), "{}", json);

    let (stdout, _) = run_wavec_capture([
        OsStr::new("print"),
        OsStr::new("supported-emit-kinds"),
        OsStr::new("--format=json"),
    ]);
    let emit_json = stdout.trim();
    assert!(
        emit_json.contains("\"check\"")
            && emit_json.contains("\"ir\"")
            && emit_json.contains("\"obj\"")
            && emit_json.contains("\"bin\""),
        "{}",
        emit_json
    );

    let (stdout, _) = run_wavec_capture([
        OsStr::new("print"),
        OsStr::new("supported-input-types"),
        OsStr::new("--format=json"),
    ]);
    let input_json = stdout.trim();
    assert!(
        input_json.contains("\"wave\"")
            && input_json.contains("\"ir\"")
            && input_json.contains("\"bc\"")
            && input_json.contains("\"asm\"")
            && input_json.contains("\"obj\"")
            && input_json.contains("\"archive\""),
        "{}",
        input_json
    );

    let (stdout, _) = run_wavec_capture([
        OsStr::new("print"),
        OsStr::new("supported-print-items"),
        OsStr::new("--format=json"),
    ]);
    let print_items = stdout.trim();
    assert!(
        print_items.contains("\"cpu-list\"")
            && print_items.contains("\"target-features\"")
            && print_items.contains("\"default-linker\"")
            && print_items.contains("\"supported-input-types\"")
            && print_items.contains("\"supported-emit-kinds\""),
        "{}",
        print_items
    );

    let (stdout, _) = run_wavec_capture([
        OsStr::new("print"),
        OsStr::new("cpu-list"),
        OsStr::new("--target"),
        OsStr::new(target),
        OsStr::new("--format=json"),
    ]);
    assert!(stdout.trim().starts_with('['), "{}", stdout);

    let (stdout, _) = run_wavec_capture([
        OsStr::new("print"),
        OsStr::new("target-features"),
        OsStr::new("--target"),
        OsStr::new(target),
        OsStr::new("--format=json"),
    ]);
    for feature in spec.features {
        assert!(stdout.contains(&format!("\"{feature}\"")), "{stdout}");
    }
}

#[test]
fn vex_cli_json_errors_and_dry_run_are_stable() {
    let bad = run_wavec_raw([
        OsStr::new("--error-format=json"),
        OsStr::new("build"),
        OsStr::new("--bad-option"),
    ]);
    assert!(
        !bad.status.success(),
        "invalid CLI should fail\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&bad.stdout),
        String::from_utf8_lossy(&bad.stderr)
    );
    assert_eq!(bad.status.code(), Some(2));
    let stderr = String::from_utf8_lossy(&bad.stderr);
    assert!(stderr.contains("\"error\""), "{}", stderr);
    assert!(stderr.contains("\"kind\":\"usage\""), "{}", stderr);
    assert!(stderr.contains("\"exit_code\":2"), "{}", stderr);

    let dir = temp_case_dir("vex-dry-run-json");
    let src = write_wave(
        &dir,
        "main.wave",
        r#"
fun main() -> i32 {
    return 0;
}
"#,
    );
    let out_dir = dir.join("out");
    let (stdout, stderr) = run_wavec_capture([
        OsStr::new("--error-format=json"),
        OsStr::new("build"),
        src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new(frontend_target()),
        OsStr::new("--freestanding"),
        OsStr::new("--emit=obj"),
        OsStr::new("--dry-run"),
        OsStr::new("--out-dir"),
        out_dir.as_os_str(),
    ]);
    assert!(stderr.trim().is_empty(), "unexpected stderr:\n{}", stderr);
    let plan = stdout.trim();
    assert!(plan.starts_with('{') && plan.ends_with('}'), "{}", plan);
    assert!(plan.contains("\"schema_version\":1"), "{}", plan);
    assert!(plan.contains(&format!("\"target\":\"{}\"", frontend_target())), "{}", plan);
    let parsed = utils::wson::parse_json(plan).unwrap();
    let spec = llvm::codegen::target::target_spec_for_triple(frontend_target()).unwrap();
    let options = llvm::codegen::target::resolve_target_options(spec, None, None, None).unwrap();
    assert_eq!(parsed.get_str("cpu"), Some(options.cpu.as_str()));
    assert_eq!(parsed.get_str("features"), Some(options.features.as_str()));
    assert_eq!(parsed.get_str("abi"), options.abi.as_deref());
    assert_eq!(parsed.get_str("isa"), options.isa.as_deref());
    assert!(plan.contains("\"mode\":\"compile-only\""), "{}", plan);
    assert!(plan.contains("\"emit_kinds\":[\"obj\"]"), "{}", plan);
    assert!(plan.contains("\"control_mode\":null"), "{}", plan);
    assert!(plan.contains("\"freestanding\":true"), "{}", plan);
    assert!(plan.contains("\"compile\""), "{}", plan);
    assert!(plan.contains("\"link\":null"), "{}", plan);
    assert!(plan.contains("\"execute\":null"), "{}", plan);

    let syntax_src = write_wave(
        &dir,
        "syntax_error.wave",
        r#"
fun main( {
}
"#,
    );
    let syntax = run_wavec_raw([
        OsStr::new("build"),
        syntax_src.as_os_str(),
        OsStr::new("--emit=check"),
        OsStr::new("--error-format=json"),
    ]);
    assert!(
        !syntax.status.success(),
        "syntax error should fail\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&syntax.stdout),
        String::from_utf8_lossy(&syntax.stderr)
    );
    let stderr = String::from_utf8_lossy(&syntax.stderr);
    assert!(stderr.contains("\"error\""), "{}", stderr);
    assert!(stderr.contains("\"kind\":\"syntax-error\""), "{}", stderr);
    assert!(stderr.contains("\"line\":"), "{}", stderr);
    assert!(stderr.contains("\"column\":"), "{}", stderr);

    let ordered_format = run_wavec_raw([
        OsStr::new("--error-format=human"),
        OsStr::new("build"),
        OsStr::new("--bad-option"),
        OsStr::new("--error-format=json"),
    ]);
    assert_eq!(ordered_format.status.code(), Some(2));
    let stderr = String::from_utf8_lossy(&ordered_format.stderr);
    assert!(
        stderr.contains("\"kind\":\"usage\""),
        "later build-level json format must control parser errors:\n{}",
        stderr
    );
}

#[test]
fn vex_cli_mixed_inputs_and_mode_conflicts_are_stable() {
    let dir = temp_case_dir("vex-cli-mixed-inputs");
    let src = write_wave(
        &dir,
        "main.wave",
        r#"
fun main() -> i32 {
    return 0;
}
"#,
    );
    let obj = dir.join("native.o");
    let archive = dir.join("libnative.a");
    fs::write(&obj, b"not-a-real-object").unwrap();
    fs::write(&archive, b"not-a-real-archive").unwrap();

    let out_dir = dir.join("artifacts");
    let target_dir = dir.join("intermediate");
    let binary = dir.join("app");
    let (stdout, stderr) = run_wavec_capture([
        OsStr::new("--error-format=json"),
        OsStr::new("build"),
        src.as_os_str(),
        obj.as_os_str(),
        archive.as_os_str(),
        OsStr::new("--emit=obj,bin"),
        OsStr::new("--dry-run"),
        OsStr::new("--out-dir"),
        out_dir.as_os_str(),
        OsStr::new("--target-dir"),
        target_dir.as_os_str(),
        OsStr::new("-o"),
        binary.as_os_str(),
    ]);
    assert!(stderr.trim().is_empty(), "unexpected stderr:\n{}", stderr);
    let plan = stdout.trim();
    assert!(plan.contains("\"kind\":\"wave\""), "{}", plan);
    assert!(plan.contains("\"kind\":\"obj\""), "{}", plan);
    assert!(plan.contains("\"kind\":\"archive\""), "{}", plan);
    assert!(plan.contains("\"emit_kinds\":[\"obj\",\"bin\"]"), "{}", plan);
    assert!(
        plan.contains(&format!("\"output\":{}", json_string_for_test(&binary.to_string_lossy()))),
        "-o must apply to final linked binary only:\n{}",
        plan
    );
    assert!(
        plan.contains(&format!(
            "\"output\":{}",
            json_string_for_test(&out_dir.join("main.o").to_string_lossy())
        )),
        "obj artifact must follow --out-dir, not -o:\n{}",
        plan
    );
    assert!(
        plan.contains(&json_string_for_test(&archive.to_string_lossy())),
        "archive input must be forwarded to the link plan:\n{}",
        plan
    );

    let mut run_command = wavec_command();
    if !native_target_enabled() && frontend_target() == "wasm64-unknown-unknown" {
        run_command.args(["--target", "wasm32-wasip1"]);
    }
    let run_plan = run_command
        .args([
            OsStr::new("--error-format=json"),
            OsStr::new("build"),
            src.as_os_str(),
            OsStr::new("--run"),
            OsStr::new("--dry-run"),
            OsStr::new("--"),
            OsStr::new("arg one"),
            OsStr::new("--flag"),
        ])
        .output()
        .unwrap();
    assert!(
        run_plan.status.success(),
        "run dry-run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run_plan.stdout),
        String::from_utf8_lossy(&run_plan.stderr)
    );
    let stdout = String::from_utf8_lossy(&run_plan.stdout);
    assert!(stdout.contains("\"mode\":\"build+run\""), "{}", stdout);
    assert!(stdout.contains("\"execute\":{"), "{}", stdout);
    let plan = utils::wson::parse_json(&stdout).unwrap();
    let expected = [utils::wson::Value::string("arg one"), utils::wson::Value::string("--flag")];
    assert_eq!(plan.get_arr("run_args"), Some(expected.as_slice()));
    let execute_args = plan.get("execute").unwrap().get_arr("args").unwrap();
    assert!(execute_args.ends_with(&expected), "{stdout}");

    let cases: &[(&str, &[&str], &[&str])] = &[
        (
            "check cannot be combined",
            &["build", "main.wave", "--emit=check,ast"],
            &["--emit=check", "alone"],
        ),
        (
            "link-only obj emit conflict",
            &["build", "native.o", "--link-only", "--emit=obj"],
            &["--link-only", "--emit=bin"],
        ),
        (
            "shared static conflict",
            &["build", "main.wave", "--shared", "--static", "--dry-run"],
            &["--shared", "--static"],
        ),
        (
            "run shared conflict",
            &["build", "main.wave", "--shared", "--run", "--dry-run"],
            &["--run", "--shared"],
        ),
        (
            "no-pie relocation conflict",
            &["-C", "relocation-model=pie", "build", "main.wave", "--no-pie", "--dry-run"],
            &["--no-pie", "relocation-model=pie"],
        ),
        (
            "forced input conflict",
            &["build", "native.o", "--input-type=wave", "--dry-run"],
            &["--input-type=wave", "conflicts"],
        ),
        (
            "unknown input inference",
            &["build", "unknown.blob", "--dry-run"],
            &["cannot infer input type", "--input-type"],
        ),
    ];

    for (name, args, needles) in cases {
        let full_args =
            std::iter::once("--error-format=json").chain(args.iter().copied()).map(OsStr::new);
        let out = run_wavec_raw(full_args);
        assert!(
            !out.status.success(),
            "{} unexpectedly succeeded\nstdout:\n{}\nstderr:\n{}",
            name,
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        );
        assert_eq!(out.status.code(), Some(2), "{}", name);
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert!(stderr.contains("\"kind\":\"usage\""), "{}: {}", name, stderr);
        // WebAssembly rejects native link modes before checking their mutual
        // conflicts. Keep the native conflict assertions on native backends,
        // and check the documented unsupported-mode diagnostic on wasm-only.
        let needles = if !native_target_enabled()
            && frontend_target() == "wasm64-unknown-unknown"
            && (args.contains(&"--shared") || args.contains(&"--no-pie"))
        {
            &["--shared", "--static", "--pie/--no-pie", "not supported for WebAssembly"][..]
        } else {
            *needles
        };
        for needle in needles {
            assert!(stderr.contains(needle), "{} missing '{}': {}", name, needle, stderr);
        }
    }
}

#[test]
fn target_configuration_is_rejected_before_frontend_or_backend_work() {
    let dir = temp_case_dir("target-validation");
    let source = write_wave(&dir, "main.wave", "fun main() -> i32 { return 0; }\n");

    let run_failure = |options: &[&str], expected: &str| {
        let mut args = vec![
            OsString::from("--error-format=json"),
            OsString::from("build"),
            source.as_os_str().to_os_string(),
        ];
        args.extend(options.iter().map(OsString::from));
        let output = run_wavec_raw(args);
        assert_eq!(
            output.status.code(),
            Some(2),
            "target validation should fail with usage exit code 2\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.trim().is_empty(), "JSON usage errors must not write to stdout: {stdout}");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains("\"kind\":\"usage\""), "{}", stderr);
        assert!(stderr.contains(expected), "{}", stderr);
        assert!(
            !stderr.contains("E9001")
                && !stderr.contains("compiler internal error")
                && !stderr.contains("panic location"),
            "target validation leaked a backend failure: {}",
            stderr
        );
    };

    for mode in [
        &["--target", "mips64-unknown-linux-gnu", "--emit=check"][..],
        &["--target", "mips64-unknown-linux-gnu", "--emit=obj"][..],
        &["--target", "mips64-unknown-linux-gnu", "--emit=obj", "--dry-run"][..],
    ] {
        run_failure(mode, "unsupported target 'mips64-unknown-linux-gnu'");
    }

    for (options, expected) in [
        (
            &["--target", "x86_64-garbage-linux-gnu", "--emit=check"][..],
            "unsupported target 'x86_64-garbage-linux-gnu'",
        ),
        (
            &["--target", "riscv64-unknown-linux-musl", "--emit=check"][..],
            "unsupported target 'riscv64-unknown-linux-musl'",
        ),
        (
            &["--target", "x86_64-unknown-linux-gnu", "--cpu", "sifive-u74", "--emit=check"][..],
            "unsupported CPU 'sifive-u74'",
        ),
        (
            &["--target", "riscv64-unknown-none-elf", "--cpu", "rocket", "--emit=obj"][..],
            "unsupported CPU 'rocket'",
        ),
        (
            &["--target", "x86_64-unknown-linux-gnu", "--features", "+m", "--emit=check"][..],
            "unsupported feature 'm'",
        ),
        (
            &["--target", "aarch64-unknown-linux-gnu", "--features", "+fp", "--emit=obj"][..],
            "unsupported feature 'fp'",
        ),
        (
            &["--target", "x86_64-unknown-linux-gnu", "--abi", "lp64d", "--emit=check"][..],
            "unsupported ABI 'lp64d'",
        ),
        (
            &["--target", "x86_64-unknown-linux-gnu", "--features", "sse2", "--emit=check"][..],
            "invalid target feature 'sse2'",
        ),
        (
            &["--target", "x86_64-unknown-linux-gnu", "--features", "+sse2,-sse2", "--emit=check"]
                [..],
            "target feature 'sse2' is specified more than once",
        ),
        (
            &["--target", "x86_64-unknown-linux-gnu", "--features", "+sse2,,+avx", "--emit=check"]
                [..],
            "invalid empty target feature",
        ),
        (
            &[
                "--target",
                "riscv64-unknown-none-elf",
                "--features",
                "+m,+a,-f,+d,+c",
                "--abi",
                "lp64d",
                "--emit=check",
            ][..],
            "feature 'd' requires feature 'f'",
        ),
        (
            &[
                "--target",
                "riscv64-unknown-none-elf",
                "--features",
                "+f,-d",
                "--abi",
                "lp64d",
                "--emit=check",
            ][..],
            "ABI 'lp64d' for target 'riscv64-unknown-none-elf' requires features 'f' and 'd'",
        ),
        (
            &[
                "--target",
                "riscv64-unknown-none-elf",
                "--features",
                "-f",
                "--abi",
                "lp64f",
                "--emit=check",
            ][..],
            "ABI 'lp64f' for target 'riscv64-unknown-none-elf' requires feature 'f'",
        ),
        (
            &["--target", "riscv64-unknown-linux-gnu", "--features", "-zicsr", "--emit=check"][..],
            "feature 'f' requires feature 'zicsr'",
        ),
    ] {
        let target = options.windows(2).find(|pair| pair[0] == "--target").map(|pair| pair[1]);
        if !expected.starts_with("unsupported target")
            && target.is_some_and(|target| {
                llvm::codegen::target::target_spec_for_triple(target).is_none()
            })
        {
            continue;
        }
        run_failure(options, expected);
    }

    #[cfg(any(feature = "llvm-target-all", feature = "llvm-target-x86"))]
    {
        let x86_out = dir.join("valid-x86");
        run_wavec([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new("x86_64-unknown-linux-gnu"),
            OsStr::new("--cpu"),
            OsStr::new("x86-64-v2"),
            OsStr::new("--features"),
            OsStr::new("+sse2,-avx"),
            OsStr::new("--emit=obj"),
            OsStr::new("--out-dir"),
            x86_out.as_os_str(),
        ]);
        assert!(x86_out.join("main.o").is_file());
    }
    #[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
    {
        let riscv_out = dir.join("valid-riscv");
        run_wavec([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new("riscv64-unknown-none-elf"),
            OsStr::new("--cpu"),
            OsStr::new("sifive-u74"),
            OsStr::new("--features"),
            OsStr::new("+m,+a,+f,+d,+c"),
            OsStr::new("--abi"),
            OsStr::new("lp64d"),
            OsStr::new("--emit=obj"),
            OsStr::new("--out-dir"),
            riscv_out.as_os_str(),
        ]);
        assert!(riscv_out.join("main.o").is_file());

        let linux_riscv_out = dir.join("valid-linux-riscv");
        run_wavec([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new("riscv64-unknown-linux-gnu"),
            OsStr::new("--emit=obj"),
            OsStr::new("--out-dir"),
            linux_riscv_out.as_os_str(),
        ]);
        let linux_riscv_object = linux_riscv_out.join("main.o");
        assert!(linux_riscv_object.is_file());
        assert_eq!(riscv64_elf_flags(&linux_riscv_object) & 0x7, 0x5);
        let linux_riscv_bytes = fs::read(&linux_riscv_object).unwrap();
        assert!(bytes_contains(&linux_riscv_bytes, b"zicsr"));
        assert!(bytes_contains(&linux_riscv_bytes, b"zifencei"));

        let (stdout, stderr) = run_wavec_capture([
            OsStr::new("print"),
            OsStr::new("target-spec"),
            OsStr::new("--target"),
            OsStr::new("riscv64-unknown-none-elf"),
            OsStr::new("--format=json"),
        ]);
        assert!(stderr.trim().is_empty(), "{}", stderr);
        assert!(stdout.contains("\"hosted\":false"), "{}", stdout);
        assert!(stdout.contains("\"freestanding\":true"), "{}", stdout);
        assert!(stdout.contains("\"cpu\":\"generic-rv64\""), "{}", stdout);
        assert!(stdout.contains("\"features\":\"+m,+a,-f,-d,+c,-zicsr,-zifencei\""), "{}", stdout);
        assert!(stdout.contains("\"abi\":\"lp64\""), "{}", stdout);
        assert!(stdout.contains("\"isa\":\"rv64imac\""), "{}", stdout);

        let (stdout, stderr) = run_wavec_capture([
            OsStr::new("print"),
            OsStr::new("target-spec"),
            OsStr::new("--target"),
            OsStr::new("riscv64-unknown-linux-gnu"),
            OsStr::new("--format=json"),
        ]);
        assert!(stderr.trim().is_empty(), "{}", stderr);
        assert!(stdout.contains("\"hosted\":true"), "{}", stdout);
        assert!(stdout.contains("\"freestanding\":false"), "{}", stdout);
        assert!(stdout.contains("\"features\":\"+m,+a,+f,+d,+c,+zicsr,+zifencei\""), "{}", stdout);
        assert!(stdout.contains("\"abi\":\"lp64d\""), "{}", stdout);
        assert!(stdout.contains("\"isa\":\"rv64gc\""), "{}", stdout);

        let (stdout, stderr) = run_wavec_capture([
            OsStr::new("--error-format=json"),
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new("riscv64-unknown-linux-gnu"),
            OsStr::new("--emit=obj"),
            OsStr::new("--dry-run"),
        ]);
        assert!(stderr.trim().is_empty(), "{}", stderr);
        assert!(stdout.contains("\"cpu\":\"generic-rv64\""), "{}", stdout);
        assert!(stdout.contains("\"features\":\"+m,+a,+f,+d,+c,+zicsr,+zifencei\""), "{}", stdout);
        assert!(stdout.contains("\"abi\":\"lp64d\""), "{}", stdout);
        assert!(stdout.contains("\"isa\":\"rv64gc\""), "{}", stdout);

        let (stdout, stderr) = run_wavec_capture([
            OsStr::new("--error-format=json"),
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new("riscv64-unknown-linux-gnu"),
            OsStr::new("--abi=lp64"),
            OsStr::new("--emit=bin"),
            OsStr::new("--dry-run"),
        ]);
        assert!(stderr.trim().is_empty(), "{}", stderr);
        assert!(stdout.contains("--dynamic-linker=/lib/ld-linux-riscv64-lp64.so.1"), "{}", stdout);
        assert!(!stdout.contains("ld-linux-riscv64-lp64d.so.1"), "{}", stdout);
        assert!(
            json_contains_path_components(
                &stdout,
                &["crt", "riscv64-unknown-linux-gnu", "lp64", "crt1.o",],
            ),
            "LP64 link plan must select Wave's ABI-matched CRT:\n{}",
            stdout
        );
        for host_path in [
            "/usr/lib64/crt1.o",
            "/usr/lib64/crti.o",
            "/usr/lib64/crtn.o",
            "-L/usr/lib64",
            "-L/lib64",
            "-L/usr/lib ",
            "-L/lib ",
        ] {
            assert!(
                !stdout.contains(host_path),
                "cross-target link plan consumed host runtime path '{}':\n{}",
                host_path,
                stdout
            );
        }

        let (stdout, stderr) = run_wavec_capture([
            OsStr::new("--error-format=json"),
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new("riscv64-unknown-linux-gnu"),
            OsStr::new("--static"),
            OsStr::new("--emit=bin"),
            OsStr::new("--dry-run"),
        ]);
        assert!(stderr.trim().is_empty(), "{}", stderr);
        assert!(stdout.contains("-static"), "{}", stdout);
        assert!(
            !stdout.contains("--dynamic-linker="),
            "static link plan must not select a dynamic loader:\n{}",
            stdout
        );
        assert!(
            json_contains_path_components(
                &stdout,
                &["crt", "riscv64-unknown-linux-gnu", "lp64d", "crt1.o",],
            ),
            "static link plan must retain Wave's LP64D CRT entry point:\n{}",
            stdout
        );

        let (stdout, stderr) = run_wavec_capture([
            OsStr::new("--error-format=json"),
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new("riscv64-unknown-linux-gnu"),
            OsStr::new("--static"),
            OsStr::new("--pie"),
            OsStr::new("--emit=bin"),
            OsStr::new("--dry-run"),
        ]);
        assert!(stderr.trim().is_empty(), "{}", stderr);
        assert!(
            stdout.contains("rcrt1.o"),
            "static PIE link plan must use the relocatable CRT entry point:\n{}",
            stdout
        );
    }
}

#[test]
fn advertised_target_options_reach_object_codegen_without_backend_diagnostics() {
    let dir = temp_case_dir("target-option-matrix");
    let source = write_wave(&dir, "matrix.wave", "fun main() -> i32 { return 0; }\n");

    let build_object = |target: &str, label: &str, options: &[OsString]| -> PathBuf {
        let out_dir = dir.join(label);
        let mut args = vec![
            OsString::from("build"),
            source.as_os_str().to_os_string(),
            OsString::from("--target"),
            OsString::from(target),
        ];
        args.extend_from_slice(options);
        args.extend([
            OsString::from("--emit=obj"),
            OsString::from("--out-dir"),
            out_dir.as_os_str().to_os_string(),
        ]);

        let (stdout, stderr) = run_wavec_capture(args);
        assert!(stdout.trim().is_empty(), "{}", stdout);
        let only_nonstandard_lp64f_warnings = target.starts_with("loongarch64-")
            && !stderr.trim().is_empty()
            && stderr.lines().all(|line| line == "warning: 'lp64f' has not been standardized");
        assert!(
            stderr.trim().is_empty() || only_nonstandard_lp64f_warnings,
            "advertised target option emitted a backend diagnostic for {target}: {stderr}"
        );
        let object = out_dir.join("matrix.o");
        assert!(object.is_file(), "{target} {label}");
        object
    };

    let (targets, stderr) = run_wavec_capture([OsStr::new("print"), OsStr::new("target-list")]);
    assert!(stderr.trim().is_empty(), "{}", stderr);

    for target in targets.lines().filter(|line| !line.is_empty()) {
        let target_label = target.replace('-', "_");
        let object = build_object(target, &format!("{target_label}_default"), &[]);

        let (target_spec, stderr) = run_wavec_capture([
            OsStr::new("print"),
            OsStr::new("target-spec"),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--format=json"),
        ]);
        assert!(stderr.trim().is_empty(), "{}", stderr);
        let object = fs::read(object).unwrap();
        if target_spec.contains("\"object_format\":\"elf\"") {
            assert!(object.starts_with(b"\x7fELF"), "{target}: {target_spec}");
        } else if target_spec.contains("\"object_format\":\"macho\"") {
            assert!(object.starts_with(&[0xcf, 0xfa, 0xed, 0xfe]), "{target}: {target_spec}");
        } else if target_spec.contains("\"object_format\":\"coff\"") {
            let machine = if target_spec.contains("\"arch\":\"aarch64\"") {
                [0x64, 0xaa]
            } else {
                [0x64, 0x86]
            };
            assert!(object.starts_with(&machine), "{target}: {target_spec}");
        } else if target_spec.contains("\"object_format\":\"wasm\"") {
            assert!(object.starts_with(b"\0asm"), "{target}: {target_spec}");
        } else {
            panic!("target spec has an unknown object format: {target_spec}");
        }

        let (cpus, stderr) = run_wavec_capture([
            OsStr::new("print"),
            OsStr::new("cpu-list"),
            OsStr::new("--target"),
            OsStr::new(target),
        ]);
        assert!(stderr.trim().is_empty(), "{}", stderr);
        for cpu in cpus.lines().filter(|line| !line.is_empty()) {
            let cpu_label = cpu.replace('-', "_");
            build_object(
                target,
                &format!("{target_label}_cpu_{cpu_label}"),
                &[OsString::from("--cpu"), OsString::from(cpu)],
            );
        }

        let (features, stderr) = run_wavec_capture([
            OsStr::new("print"),
            OsStr::new("target-features"),
            OsStr::new("--target"),
            OsStr::new(target),
        ]);
        assert!(stderr.trim().is_empty(), "{}", stderr);
        for feature in features.lines().filter(|line| !line.is_empty()) {
            let feature_label = feature.replace(['-', '.'], "_");
            for (sign, action) in [("+", "enable"), ("-", "disable")] {
                let float_abi_target =
                    target.starts_with("riscv64-") || target.starts_with("loongarch64-");
                let setting = match (float_abi_target, feature, sign) {
                    (true, "f", "-") => "-f,-d".to_string(),
                    (true, "d", "+") => "+f,+d".to_string(),
                    _ if target.starts_with("riscv64-") && feature == "zicsr" && sign == "-" => {
                        "-f,-d,-zicsr".to_string()
                    },
                    _ => format!("{sign}{feature}"),
                };
                if target == "riscv64-unknown-freebsd"
                    && sign == "-"
                    && matches!(feature, "f" | "d" | "zicsr")
                {
                    let output = wavec_command()
                        .arg("build")
                        .arg(&source)
                        .args(["--target", target, "--features", &setting, "--emit=obj"])
                        .arg("--out-dir")
                        .arg(dir.join("rejected-freebsd-abi"))
                        .output()
                        .unwrap();
                    assert!(!output.status.success());
                    assert!(
                        String::from_utf8_lossy(&output.stderr).contains("supported ABIs: lp64d")
                    );
                    continue;
                }
                build_object(
                    target,
                    &format!("{target_label}_feature_{action}_{feature_label}"),
                    &[OsString::from("--features"), OsString::from(setting)],
                );
            }
        }
    }

    #[cfg(any(feature = "llvm-target-all", feature = "llvm-target-riscv"))]
    {
        for (abi, features, expected_flags) in [
            ("lp64", "+m,+a,-f,-d,+c", 0x1),
            ("lp64f", "+m,+a,+f,-d,+c", 0x3),
            ("lp64d", "+m,+a,+f,+d,+c", 0x5),
        ] {
            for target in ["riscv64-unknown-linux-gnu", "riscv64-unknown-none-elf"] {
                let object = build_object(
                    target,
                    &format!("{}_abi_{abi}", target.replace('-', "_")),
                    &[
                        OsString::from("--features"),
                        OsString::from(features),
                        OsString::from("--abi"),
                        OsString::from(abi),
                    ],
                );
                assert_eq!(riscv64_elf_flags(&object) & 0x7, expected_flags);
            }
        }
    }

    #[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
    {
        for (abi, expected_flags) in [("lp64s", 0x1), ("lp64f", 0x2), ("lp64d", 0x3)] {
            let object = build_object(
                "loongarch64-unknown-linux-gnu",
                &format!("loongarch64_unknown_linux_gnu_abi_{abi}"),
                &[OsString::from("--abi"), OsString::from(abi)],
            );
            assert_eq!(loongarch64_elf_flags(&object) & 0x7, expected_flags);
        }
    }
}

#[test]
#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-x86"))]
fn inline_asm_rejects_invalid_contracts_and_allows_local_jumps() {
    let dir = temp_case_dir("asm-contract-extra");

    let local_jump_src = write_wave(
        &dir,
        "local_jump.wave",
        r#"
fun main() {
    asm {
        "jmp 1f"
        "1:"
    }
}

"#,
    );
    let local_jump_dir = dir.join("local-jump");
    run_wavec([
        OsStr::new("build"),
        local_jump_src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        local_jump_dir.as_os_str(),
    ]);

    let conflict_src = write_wave(
        &dir,
        "conflicting_stack.wave",
        r#"
fun main() {
    asm {
        "nop"
        clobber("stack")
        clobber("nostack")
    }
}
"#,
    );
    let conflict_dir = dir.join("conflict");
    let err = run_wavec_expect_failure([
        OsStr::new("build"),
        conflict_src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        conflict_dir.as_os_str(),
    ]);
    assert!(
        err.contains("cannot declare both"),
        "stack/nostack conflict should be rejected:\n{}",
        err
    );

    let expr_noreturn_src = write_wave(
        &dir,
        "expr_noreturn.wave",
        r#"
fun invalid_noreturn_expression() -> i64 {
    var x: i64 = asm {
        "jmp rax"
        in("rax") 0
        clobber("noreturn")
    };
    return x;
}

fun main() {
    invalid_noreturn_expression();
}
"#,
    );
    let expr_noreturn_dir = dir.join("expr-noreturn");
    let err = run_wavec_expect_failure([
        OsStr::new("build"),
        expr_noreturn_src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        expr_noreturn_dir.as_os_str(),
    ]);
    assert!(
        err.contains("asm expression cannot declare"),
        "asm expressions must reject noreturn:\n{}",
        err
    );

    let clobber_operand_conflict_src = write_wave(
        &dir,
        "clobber_operand_conflict.wave",
        r#"
fun main() {
    var x: i64 = 1;
    asm {
        "mov rax, rax"
        in("rax") x
        clobber("rax")
    }
}
"#,
    );
    let clobber_operand_conflict_dir = dir.join("clobber-operand-conflict");
    let err = run_wavec_expect_failure([
        OsStr::new("build"),
        clobber_operand_conflict_src.as_os_str(),
        OsStr::new("--target"),
        OsStr::new("x86_64-unknown-linux-gnu"),
        OsStr::new("--emit=ir"),
        OsStr::new("--out-dir"),
        clobber_operand_conflict_dir.as_os_str(),
    ]);
    assert!(
        err.contains("conflicts with an input/output operand register"),
        "clobber/operand register conflict should be rejected:\n{}",
        err
    );
}

#[cfg(any(feature = "llvm-target-all", feature = "llvm-target-loongarch"))]
#[test]
fn loongarch64_lp64f_hosted_linking_is_rejected_before_the_linker() {
    let dir = temp_case_dir("loongarch64-lp64f-hosted-link");
    let source = write_wave(&dir, "main.wave", "fun main() -> i32 { return 0; }\n");
    let output = run_wavec_raw([
        OsStr::new("build"),
        source.as_os_str(),
        OsStr::new("--target=loongarch64-unknown-linux-gnu"),
        OsStr::new("--abi=lp64f"),
        OsStr::new("-Clinker=/bin/false"),
        OsStr::new("--out-dir"),
        dir.join("link-attempt").as_os_str(),
    ]);

    assert!(!output.status.success());
    let error = String::from_utf8_lossy(&output.stderr);
    assert!(error.contains("glibc does not provide an LP64F runtime"), "{error}");
    assert!(
        !error.contains("link failed"),
        "external linker ran before LP64F runtime validation: {error}"
    );
}

#[test]
fn msvc_rejects_foreign_link_input_before_launch_and_preserves_output() {
    let dir = temp_case_dir("msvc-foreign-object");
    let input = dir.join("foreign.obj");
    let mut header = vec![0_u8; 20];
    header[..2].copy_from_slice(&0xaa64_u16.to_le_bytes());
    fs::write(&input, header).unwrap();
    let output = dir.join("retained.exe");
    fs::write(&output, b"retained executable").unwrap();
    if llvm::codegen::target::target_spec_for_triple("x86_64-pc-windows-msvc").is_none() {
        return;
    }
    let error = run_wavec_expect_failure([
        OsStr::new("build"),
        input.as_os_str(),
        OsStr::new("--target=x86_64-pc-windows-msvc"),
        OsStr::new("-Cno-default-libs"),
        OsStr::new("--entry=entry"),
        OsStr::new("-Clinker=wave-linker-that-must-not-run"),
        OsStr::new("-o"),
        output.as_os_str(),
    ]);
    assert!(error.contains("foreign.obj") && error.contains("machine"), "{error}");
    assert_eq!(fs::read(output).unwrap(), b"retained executable");
}

#[test]
fn retired_windows_gnu_targets_report_the_msvc_replacement() {
    let targets = run_wavec_capture(["print", "target-list"]).0;
    assert!(!targets.lines().any(|target| target.contains("windows-gnu")));
    let dir = temp_case_dir("retired-windows-targets");
    let source = write_wave(&dir, "main.wave", "fun main() -> i32 { return 0; }\n");
    for (target, replacement) in [
        ("x86_64-pc-windows-gnu", "x86_64-pc-windows-msvc"),
        ("x86_64-w64-windows-gnu", "x86_64-pc-windows-msvc"),
        ("aarch64-pc-windows-gnu", "aarch64-pc-windows-msvc"),
        ("aarch64-w64-windows-gnu", "aarch64-pc-windows-msvc"),
    ] {
        let error = run_wavec_expect_failure([
            OsStr::new("build"),
            source.as_os_str(),
            OsStr::new("--target"),
            OsStr::new(target),
            OsStr::new("--emit=obj"),
        ]);
        assert!(error.contains("has been retired") && error.contains(replacement), "{error}");
    }
}

#[test]
fn phase1_diagnostics_are_located_in_human_and_json_modes() {
    let dir = temp_case_dir("phase1-diagnostics");
    for newline in ["\n", "\r\n", "\r"] {
        for (body, message, code) in [
            ("fun f() { const x: i32 = 1; }", "top level", "E2001"),
            ("fun f() { for (static x: i32 = 1; x < 2; x += 1) {} }", "top level", "E2001"),
            ("fun f() { match (1) { 1 {} } }", "match arm", "E2001"),
            ("extern(c) fun f(x: ptr<,>);", "extern parameter type", "E2001"),
            ("extern(c) fun f() -> ptr<,>;", "extern return type", "E2001"),
            ("fun main(x: i32 = 42) -> i32 { return x; }", "must have zero parameters", "E3001"),
            (r#"fun f() { var s: str = "\x00"; }"#, "NUL", "E1004"),
        ] {
            let source = write_wave(&dir, "bad.wave", &format!("// 한글{newline}{body}"));
            for format in ["human", "json"] {
                let output = run_wavec_raw([
                    OsStr::new("check"),
                    source.as_os_str(),
                    OsStr::new(&format!("--error-format={format}")),
                ]);
                assert!(!output.status.success(), "{body}");
                assert!(output.stdout.is_empty(), "unexpected parser output: {:?}", output.stdout);
                let stderr = String::from_utf8(output.stderr).unwrap();
                assert!(stderr.contains(message), "{stderr}");
                assert!(stderr.contains(code), "{stderr}");
                if format == "json" {
                    let parsed = utils::wson::parse_json(stderr.trim()).unwrap();
                    assert_eq!(parsed.get("error").unwrap().get_num("line"), Some(2.0));
                } else {
                    assert!(stderr.contains(body), "{stderr}");
                }
            }
        }
    }
}

#[test]
#[cfg(feature = "llvm-target-wasm")]
fn wasm_missing_host_imports_have_actionable_human_and_json_errors() {
    for tool in ["node", "wasm-ld"] {
        if !Command::new(tool).arg("--version").output().is_ok_and(|out| out.status.success()) {
            eprintln!("skipping WebAssembly missing-import execution: {tool} unavailable");
            return;
        }
    }
    let dir = temp_case_dir("wasm-missing-imports");
    let source = write_wave(&dir, "missing.wave", "extern(c) fun host_add(a: i32, b: i32) -> i32;\nfun main() -> i32 { return host_add(2, 3); }\n");
    for target in ["wasm32-unknown-unknown", "wasm64-unknown-unknown", "wasm32-wasip1"] {
        for format in ["human", "json"] {
            let output = wavec_command()
                .arg("build")
                .arg(&source)
                .args(["--target", target, "--run", "--error-format", format, "--out-dir"])
                .arg(dir.join(target))
                .output()
                .unwrap();
            assert_eq!(output.status.code(), Some(1), "{target}: {:?}", output);
            let stderr = String::from_utf8(output.stderr).unwrap();
            let name = if target == "wasm32-wasip1" {
                "wasi_snapshot_preview1.host_add"
            } else {
                "env.host_add"
            };
            assert!(stderr.contains(name), "{stderr}");
            assert!(stderr.contains("explicit JavaScript host"), "{stderr}");
            assert!(!stderr.contains("LinkError") && !stderr.contains("at async"), "{stderr}");
            if format == "json" {
                let value = utils::wson::parse_json(stderr.trim()).unwrap();
                let error = value.get("error").unwrap();
                assert_eq!(error.get_str("kind"), Some("command-failed"));
                assert_eq!(error.get_u64("exit_code"), Some(1));
            }
        }
    }
    fs::remove_dir_all(dir).unwrap();
}

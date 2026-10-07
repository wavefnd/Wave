// SPDX-License-Identifier: MPL-2.0
use lexer::Lexer;
use parser::{parse_syntax_only, parse_syntax_with_spans};

#[test]
fn parser_depth_budget_is_restored_after_errors_and_counts_mixed_shapes() {
    for expression in [
        format!("{}1{}", "(".repeat(129), ")".repeat(129)),
        format!("{}1{}", "f([(".repeat(43), ")])".repeat(43)),
        format!("{}1", "- ".repeat(129)),
        format!("{}1", "x = ".repeat(129)),
    ] {
        let source = format!("fun main() {{ {expression}; }}");
        let tokens = Lexer::new_with_file(&source, "depth.wave").tokenize().unwrap();
        let error = parse_syntax_with_spans(&tokens).unwrap_err();
        assert!(error.message().contains("maximum of 128"), "{error:?}");
        assert_eq!(error.span().unwrap().file, "depth.wave");
        let tokens = Lexer::new("fun main() { (1); }").tokenize().unwrap();
        assert!(parse_syntax_only(&tokens).is_ok());
    }
}

#[test]
fn struct_depth_boundaries_fit_a_small_native_stack() {
    // Leave headroom below MSVC's 1 MiB main-thread stack. In particular,
    // constructing the error for level 129 must fit as well as valid input.
    std::thread::Builder::new()
        .stack_size(768 * 1024)
        .spawn(|| {
            for depth in [128, 129, 5000] {
                let source = format!(
                    "fun main() {{ return {}1{}; }}",
                    "Item { value: ".repeat(depth),
                    " }".repeat(depth),
                );
                let tokens = Lexer::new_with_file(&source, "struct-depth.wave").tokenize().unwrap();
                for (with_spans, parse) in [
                    (false, parse_syntax_only as fn(&[lexer::Token]) -> _),
                    (true, parse_syntax_with_spans),
                ] {
                    let result = parse(&tokens);
                    if depth == 128 {
                        assert!(result.is_ok(), "{result:?}");
                    } else {
                        let error = result.unwrap_err();
                        assert!(error.message().contains("maximum of 128"), "{error:?}");
                        if with_spans {
                            assert_eq!(error.span().unwrap().file, "struct-depth.wave");
                        }
                    }
                }
            }
        })
        .unwrap()
        .join()
        .unwrap();
}

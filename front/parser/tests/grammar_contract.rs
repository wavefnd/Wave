//! Normative grammar fixtures and complete token-inventory drift detection.
use lexer::Lexer;
use parser::parse_syntax_with_spans;
use std::collections::BTreeSet;
use std::path::Path;

#[test]
fn every_token_kind_is_classified_and_its_example_matches_the_lexer() {
    let vocabulary = include_str!("../../lexer/src/token.rs")
        .split("pub enum TokenType {")
        .nth(1)
        .unwrap()
        .split("\n}")
        .next()
        .unwrap();
    let variants: BTreeSet<_> = vocabulary
        .lines()
        .filter_map(|line| {
            let line = line.trim();
            if line.starts_with("//") || line.is_empty() {
                return None;
            }
            Some(line.split(['(', ',']).next().unwrap())
        })
        .collect();
    let grammar = include_str!("../../../spec/alpha-0.ebnf");
    let productions: BTreeSet<_> = grammar
        .lines()
        .filter_map(|line| line.split_once(" = ").map(|p| p.0))
        .collect();
    let mut documented = BTreeSet::new();
    for row in include_str!("../../../spec/tokens.tsv")
        .lines()
        .filter(|line| !line.starts_with('#'))
    {
        let fields: Vec<_> = row.split('\t').collect();
        assert_eq!(fields.len(), 5, "{row}");
        assert!(
            documented.insert(fields[0]),
            "duplicate token {}",
            fields[0]
        );
        assert!(
            matches!(
                fields[1],
                "implemented" | "reserved" | "removed" | "internal"
            ),
            "{row}"
        );
        assert!(productions.contains(fields[3]), "{row}");
        if fields[4] != "-" {
            let tokens = Lexer::new(fields[4]).tokenize().unwrap();
            assert_eq!(tokens.len(), 2, "{row}: {tokens:?}");
            if fields[1] == "reserved" {
                assert!(tokens[0].token_type.reserved_spelling().is_some(), "{row}");
                for body in [format!("{};", fields[4]), format!("1 {} 2;", fields[4])] {
                    let source = format!("fun f() {{ {body} }}");
                    let tokens = Lexer::new(&source).tokenize().unwrap();
                    assert!(parse_syntax_with_spans(&tokens).is_err(), "{source}");
                }
            }
            let actual = format!("{:?}", tokens[0].token_type);
            assert_eq!(actual.split('(').next().unwrap(), fields[0], "{row}");
        }
    }
    assert_eq!(
        variants, documented,
        "update spec/tokens.tsv when changing the token vocabulary"
    );
}

#[test]
fn grammar_examples_accept_and_reject_as_documented() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../spec");
    let grammar = include_str!("../../../spec/alpha-0.ebnf");
    for row in include_str!("../../../spec/fixtures.tsv")
        .lines()
        .filter(|line| !line.starts_with('#'))
    {
        let (name, productions) = row.split_once('\t').unwrap();
        for production in productions.split(',') {
            assert!(grammar.contains(&format!("{production} = ")));
        }
        for (verdict, accept) in [("accept", true), ("reject", false)] {
            let file = root.join("fixtures").join(format!("{name}.{verdict}.wave"));
            let source = std::fs::read_to_string(&file).unwrap();
            let result = Lexer::new_with_file(&source, file.display().to_string())
                .tokenize()
                .map_err(|e| format!("{e:?}"))
                .and_then(|tokens| parse_syntax_with_spans(&tokens).map_err(|e| format!("{e:?}")));
            assert_eq!(result.is_ok(), accept, "{}: {result:?}", file.display());
        }
    }
}

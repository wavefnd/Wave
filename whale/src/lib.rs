// SPDX-License-Identifier: MPL-2.0
//! Whale backend for Wave, alongside the LLVM backend crate.
//!
//! This crate owns typed HIR lowering, Whale IR verification and serialization,
//! and the upstream Whale dependency. CLI dispatch and source preparation belong
//! to the compiler driver.

// Source diagnostics retain their complete span at the backend boundary.
#![allow(clippy::result_large_err)]

mod codegen;

pub use codegen::LowerError;

/// Lower verified Wave HIR to verified textual Whale IR.
///
/// Unsupported constructs return a source diagnostic. This backend never
/// delegates lowering to LLVM.
pub fn emit_ir(program: &hir::TypedProgram) -> Result<String, LowerError> {
    let (module, spans) = codegen::lower_with_sources(program)?;
    let sources = spans.into_iter().enumerate().map(|(id, span)| (id as u64, span)).collect();
    write_ir_sources(&whale_ir::print_module(&module), &sources).map_err(|mut error| {
        if !error.message.contains("resource limit") {
            error.message =
                format!("internal compiler error in Whale source serialization: {}", error.message);
        }
        error
    })
}

/// Full Wave ranges supplement the upstream file/line metadata. They are
/// nonsemantic IR comments, not another output artifact or a new IR format.
/// Consumers rewriting IR can retain them with read_ir_sources/write_ir_sources.
pub type IrSources = std::collections::BTreeMap<u64, error::SourceSpan>;
const SOURCE_RECORD: &str = "// wave-source-provenance-v1 ";

fn source_error(message: impl Into<String>) -> LowerError {
    LowerError { message: message.into(), span: None }
}

fn span_value(span: &error::SourceSpan, depth: usize) -> Result<utils::wson::Value, LowerError> {
    use utils::wson::Value;
    if depth >= 128 {
        return Err(source_error("Whale source metadata resource limit exceeded"));
    }
    if span.start > span.end || (span.line, span.column) > (span.end_line, span.end_column) {
        return Err(source_error("invalid Wave source provenance range"));
    }
    Ok(Value::object([
        ("file", Value::string(&span.file)),
        ("start", Value::integer(span.start as u64)),
        ("end", Value::integer(span.end as u64)),
        ("line", Value::integer(span.line as u64)),
        ("column", Value::integer(span.column as u64)),
        ("end_line", Value::integer(span.end_line as u64)),
        ("end_column", Value::integer(span.end_column as u64)),
        ("expansion", Value::strings(&span.expansion)),
        (
            "focus",
            match span.focus.as_deref() {
                Some(focus) => span_value(focus, depth + 1)?,
                None => Value::Null,
            },
        ),
    ]))
}

fn value_span(value: &utils::wson::Value, depth: usize) -> Result<error::SourceSpan, LowerError> {
    use utils::wson::Value;
    if depth >= 128 {
        return Err(source_error("Whale source metadata resource limit exceeded"));
    }
    let fail = || source_error("invalid Wave source provenance record");
    let number = |name| value.get_u64(name).and_then(|n| usize::try_from(n).ok()).ok_or_else(fail);
    let Value::Object(fields) = value else {
        return Err(fail());
    };
    if fields.len() != 9 {
        return Err(fail());
    }
    let expansion = value
        .get_arr("expansion")
        .ok_or_else(fail)?
        .iter()
        .map(|v| match v {
            Value::String(s) => Ok(s.clone()),
            _ => Err(fail()),
        })
        .collect::<Result<Vec<_>, _>>()?;
    let span = error::SourceSpan {
        file: value.get_str("file").ok_or_else(fail)?.to_string(),
        start: number("start")?,
        end: number("end")?,
        line: number("line")?,
        column: number("column")?,
        end_line: number("end_line")?,
        end_column: number("end_column")?,
        expansion,
        focus: match value.get("focus") {
            Some(Value::Null) => None,
            Some(focus) => Some(Box::new(value_span(focus, depth + 1)?)),
            None => return Err(fail()),
        },
    };
    if span.start > span.end || (span.line, span.column) > (span.end_line, span.end_column) {
        return Err(fail());
    }
    Ok(span)
}

fn validate_sources(module: &whale_ir::Module, sources: &IrSources) -> Result<(), LowerError> {
    let originals: std::collections::BTreeMap<_, _> =
        module.sources.originals.iter().map(|node| (node.id, &node.span)).collect();
    let files: std::collections::BTreeMap<_, _> =
        module.sources.files.iter().map(|file| (file.id, &file.name)).collect();
    for (id, source) in sources {
        let original = originals
            .get(id)
            .ok_or_else(|| source_error("Wave provenance references a missing original node"))?;
        if let Some(original) = original {
            if !files.contains_key(&original.file)
                || (
                    u64::from(original.line),
                    u64::from(original.column),
                    u64::from(original.end_line),
                    u64::from(original.end_column),
                ) != (
                    source.line as u64,
                    source.column as u64,
                    source.end_line as u64,
                    source.end_column as u64,
                )
            {
                return Err(source_error(
                    "Wave provenance disagrees with verified Whale source metadata",
                ));
            }
        }
    }
    Ok(())
}

pub fn read_ir_sources(text: &str) -> Result<IrSources, LowerError> {
    let module =
        whale_ir::parse_module(text).map_err(|e| source_error(format!("invalid Whale IR: {e}")))?;
    let limits = whale_ir::IrLimits::default();
    let mut sources = IrSources::new();
    for line in text.lines() {
        if let Some(record) = line.strip_prefix(SOURCE_RECORD) {
            let (id, encoded) = record
                .split_once(' ')
                .ok_or_else(|| source_error("invalid Wave provenance header"))?;
            let id =
                id.parse::<u64>().map_err(|_| source_error("invalid Wave provenance identity"))?;
            let value = utils::wson::parse_with_limits(
                encoded,
                utils::wson::Format::Json,
                utils::wson::Limits {
                    input_bytes: limits.max_input_bytes,
                    value_nodes: limits.max_nodes,
                    output_bytes: limits.max_input_bytes,
                },
            )
            .map_err(|e| source_error(e.to_string()))?;
            if sources.insert(id, value_span(&value, 0)?).is_some() {
                return Err(source_error("duplicate Wave provenance identity"));
            }
        }
    }
    validate_sources(&module, &sources)?;
    Ok(sources)
}

pub fn write_ir_sources(text: &str, sources: &IrSources) -> Result<String, LowerError> {
    let module =
        whale_ir::parse_module(text).map_err(|e| source_error(format!("invalid Whale IR: {e}")))?;
    validate_sources(&module, sources)?;
    let limits = whale_ir::IrLimits::default();
    let mut output = String::new();
    for line in text.lines().filter(|line| !line.starts_with(SOURCE_RECORD)) {
        output.push_str(line);
        output.push('\n');
    }
    for (id, source) in sources {
        let header = format!("{SOURCE_RECORD}{id} ");
        let remaining = limits
            .max_input_bytes
            .checked_sub(output.len())
            .and_then(|n| n.checked_sub(header.len() + 1))
            .ok_or_else(|| source_error("Whale source metadata resource limit exceeded"))?;
        let encoded = utils::wson::dumps_with_limits(
            &span_value(source, 0)?,
            utils::wson::Format::Json,
            false,
            utils::wson::Limits {
                input_bytes: limits.max_input_bytes,
                value_nodes: limits.max_nodes,
                output_bytes: remaining,
            },
        )
        .map_err(|e| source_error(format!("Whale source metadata resource limit exceeded: {e}")))?;
        output.push_str(&header);
        output.push_str(&encoded);
        output.push('\n');
    }
    if output.len() > limits.max_input_bytes {
        return Err(source_error("Whale source metadata resource limit exceeded"));
    }
    Ok(output)
}

#[cfg(test)]
mod provenance_tests {
    use super::*;
    #[test]
    fn full_ranges_and_generated_default_provenance_survive_serialization() {
        let source =
            "fun entry() -> i32 { return target(); } fun target(n: i32 = 7) -> i32 { return n; }";
        let tokens = lexer::Lexer::new_with_file(source, "defaults.wave").tokenize().unwrap();
        let program =
            hir::TypedProgram::lower(parser::parse_syntax_with_spans(&tokens).unwrap()).unwrap();
        let text = emit_ir(&program).unwrap();
        let sources = read_ir_sources(&text).unwrap();
        assert!(!sources.is_empty());
        assert!(sources.values().any(|span| !span.expansion.is_empty()));
        for span in sources.values() {
            assert_eq!(span.file, "defaults.wave");
            assert!(span.start <= span.end && span.end <= source.len());
        }
        let module = whale_ir::parse_module(&text).unwrap();
        let rewritten = write_ir_sources(&whale_ir::print_module(&module), &sources).unwrap();
        assert_eq!(read_ir_sources(&rewritten).unwrap(), sources);
        let missing = whale_ir::print_module(&module);
        assert!(read_ir_sources(&missing).unwrap().is_empty());
        let mut bad = sources.clone();
        bad.values_mut().next().unwrap().column += 1;
        assert!(write_ir_sources(&missing, &bad).is_err());
        let mut bad = sources.clone();
        let span = bad.values_mut().next().unwrap();
        span.start = span.end + 1;
        assert!(write_ir_sources(&missing, &bad).is_err());
        let duplicate = text.lines().find(|line| line.starts_with(SOURCE_RECORD)).unwrap();
        assert!(read_ir_sources(&format!("{text}{duplicate}\n")).is_err());
    }

    #[test]
    fn multiple_source_snapshots_and_metadata_limits_keep_their_ownership() {
        let _scope = error::CompilationSources::begin();
        let mut syntax = Vec::new();
        for (file, source) in [
            ("entry.wave", "fun entry() -> i32 { return target(); }"),
            ("library.wave", "fun target() -> i32 { return 7; }"),
        ] {
            error::SourceManager::record(file, source);
            let tokens = lexer::Lexer::new_with_file(source, file).tokenize().unwrap();
            syntax.extend(parser::parse_syntax_with_spans(&tokens).unwrap());
        }
        let program = hir::TypedProgram::lower(syntax).unwrap();
        let text = emit_ir(&program).unwrap();
        let module = whale_ir::parse_module(&text).unwrap();
        assert_eq!(module.sources.files.len(), 2);
        assert_ne!(module.sources.files[0].id, module.sources.files[1].id);
        let sources = read_ir_sources(&text).unwrap();
        assert!(sources.values().any(|span| span.file == "entry.wave"));
        assert!(sources.values().any(|span| span.file == "library.wave"));
        let mut span = sources.values().next().unwrap().clone();
        for _ in 0..128 {
            let mut parent = span.clone();
            parent.focus = Some(Box::new(span));
            span = parent;
        }
        let mut oversized = sources.clone();
        oversized.insert(*sources.keys().next().unwrap(), span);
        assert!(write_ir_sources(&text, &oversized)
            .unwrap_err()
            .message
            .contains("resource limit"));
        let mut oversized = sources;
        oversized.values_mut().next().unwrap().expansion =
            vec!["x".repeat(whale_ir::IrLimits::default().max_input_bytes)];
        assert!(write_ir_sources(&text, &oversized)
            .unwrap_err()
            .message
            .contains("resource limit"));
    }
}

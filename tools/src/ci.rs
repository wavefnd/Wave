// SPDX-License-Identifier: MPL-2.0
//! Read-only Actions evidence, not another workflow/procedure interpreter.
use crate::{cases, cli::Args, config, error, storage, Result};
use std::ffi::OsString;
use utils::wson::Value;

pub fn action_report(steps: &Value, target: &str) -> Result<Value> {
    let Value::Object(steps) = steps else {
        return Err(error("Actions step outcomes must be an object"));
    };
    let mut rows = Vec::new();
    for (name, step) in steps {
        let outcome = config::text(step, "outcome")?;
        let status = match outcome {
            "success" => "pass",
            "failure" => "fail",
            "cancelled" => "cancelled",
            "skipped" => "skip",
            _ => return Err(error(format!("unknown Actions step outcome: {outcome}"))),
        };
        rows.push(Value::object([
            ("id", Value::string(name)),
            ("name", Value::string(name)),
            ("status", Value::string(status)),
            ("outcome", Value::string(outcome)),
            ("conclusion", Value::string(config::text(step, "conclusion")?)),
            ("commands", Value::Array(vec![])),
        ]));
    }
    Ok(Value::object([
        ("schema_version", Value::integer(1)),
        ("phase", Value::string("actions")),
        ("target", Value::string(target)),
        ("source_sha", Value::string(std::env::var("GITHUB_SHA").unwrap_or_default())),
        ("stages", Value::Array(rows)),
    ]))
}

pub fn runtime_evidence(
    report: &Value,
    target: &cases::Target,
    shard: usize,
    shards: usize,
) -> Result<usize> {
    if shards == 0 || shard >= shards {
        return Err(error("invalid runtime evidence shard"));
    }
    let manifest = cases::Manifest::load(&crate::root(), true)?;
    let sources = manifest.sources(Some(target))?;
    let selected = sources
        .into_iter()
        .enumerate()
        .filter(|(i, _)| i % shards == shard)
        .map(|(_, source)| source);
    let rows = config::array(report, "tests")?;
    let mut checked = 0;
    for source in selected {
        let metadata = crate::runner::Metadata::parse(&std::fs::read_to_string(&source)?)?;
        if !crate::runner::runtime_eligible(&metadata, target) {
            continue;
        }
        let name = source.strip_prefix(crate::root())?.to_string_lossy().replace('\\', "/");
        let row = rows
            .iter()
            .find(|row| row.get_str("name") == Some(&name))
            .ok_or_else(|| error(format!("missing runtime evidence for {name}")))?;
        if row.get_str("status") != Some("pass")
            || !config::array(row, "commands")?.iter().any(|command| {
                command.get_str("phase") == Some("run")
                    && command.get_str("status") == Some("pass")
                    && command.get_u64("actual_exit") == Some(0)
            })
        {
            return Err(error(format!("runtime source was not successfully executed: {name}")));
        }
        checked += 1;
    }
    if checked == 0 {
        return Err(error("runtime evidence selection is empty"));
    }
    Ok(checked)
}

pub fn main(arguments: &[OsString]) -> Result<i32> {
    let (command, rest) =
        arguments.split_first().ok_or_else(|| error("expected CI evidence command"))?;
    let mut args = Args::new(rest);
    if command == "artifact" {
        let id = args.required("--target")?;
        let output = args.path("--github-output")?;
        args.finish()?;
        let manifest = cases::Manifest::load(&crate::root(), false)?;
        let target = manifest.resolve(&id)?;
        let version = std::env::var("RELEASE_VERSION").unwrap_or(config::compiler_version()?);
        crate::publication::validate_version(&version, false)?;
        let archive = format!(
            "wave-v{version}-{}{}",
            target.archive_target()?,
            if target.os == "windows" { ".zip" } else { ".tar.gz" }
        );
        let fields = std::collections::BTreeMap::from([
            ("archive".into(), archive),
            ("id".into(), target.id.clone()),
            ("target".into(), target.rust_target()?),
        ]);
        if let Some(path) = &output {
            storage::validate_output(path, &crate::runner::protected_inputs(&[])?)?;
        }
        crate::provision::export(&fields, output.as_deref())?;
    } else if command == "report" {
        let output = args.path("--output")?.ok_or_else(|| error("missing --output"))?;
        let target = args.required("--target-id")?;
        let file = args.path("--steps-json")?;
        args.finish()?;
        let steps = if let Some(file) = file {
            storage::validate_output(&output, std::slice::from_ref(&file))?;
            config::read_json(&file)?
        } else {
            config::parse_json(
                &std::env::var("WAVE_STEP_OUTCOMES")
                    .map_err(|_| error("WAVE_STEP_OUTCOMES is required"))?,
            )?
        };
        storage::validate_output(&output, &crate::runner::protected_inputs(&[])?)?;
        storage::atomic_write(&output, config::json(&action_report(&steps, &target)?)?.as_bytes())?;
    } else if command == "runtime-evidence" {
        let input = args.path("--report")?.ok_or_else(|| error("missing --report"))?;
        let output = args.path("--output")?;
        let id = args.required("--target-id")?;
        let shard = args.text("--shard-index")?.unwrap_or_else(|| "0".into()).parse()?;
        let shards = args.text("--shard-count")?.unwrap_or_else(|| "1".into()).parse()?;
        args.finish()?;
        let manifest = cases::Manifest::load(&crate::root(), true)?;
        let report = config::read_json(&input)?;
        let count = runtime_evidence(&report, manifest.target(&id)?, shard, shards)?;
        if let Some(output) = output {
            let mut inputs = crate::runner::protected_inputs(std::slice::from_ref(&input))?;
            inputs.push(input.clone());
            storage::validate_output(&output, &inputs)?;
            let mut evidence = report.clone();
            config::field(&mut evidence, "phase", Value::string("runtime"))?;
            config::field(&mut evidence, "validated_runtime_cases", Value::integer(count as u64))?;
            // Keep the original build/run records and selection, rather than
            // manufacturing a second successful execution of these cases.
            config::field(&mut evidence, "derived_from", Value::string(input.to_string_lossy()))?;
            storage::atomic_write(&output, config::json(&evidence)?.as_bytes())?;
        }
        println!("Verified {count} executed runtime cases");
    } else {
        return Err(error("unknown CI evidence command"));
    }
    Ok(0)
}

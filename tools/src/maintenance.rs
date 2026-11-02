// SPDX-License-Identifier: MPL-2.0
const API_HEADER: &str = r###"# Public standard-library API

Generated with `wave-tools maintain std-api`; run the same command after changing
public declarations. `--check` detects a stale inventory. Signatures and public
re-exports below come from source; private imports are omitted. A source path under
`sys/<os>/<arch>` is target-specific even when no additional attribute is shown.
Attributes select declarations; source presence alone does not establish runtime support.
See [system capabilities](SYSTEMS.md) and [compiler targets](../README.md#target-families).

```wave
import("std::fs::file")::{fs_open};
import("std::fs::file" as file);
```

`fs_open` is the portable file API; `std::sys::fs::open` is its raw OS boundary.
The former `std::libc` binding modules have been removed; they are not public std API.

"###;
const SYSTEMS_HEADER: &str = r###"# Standard-library system capabilities

Compiler target acceptance and std provider coverage are separate. `source` below
means provider code or a target-selected facade is present; it does not promise every
operation is implemented or runtime-tested. `limited` has explicit unsupported paths.
Native/QEMU/VM describes the configured CI execution route, not universal API coverage.
Exact declarations and target attributes are in [the public inventory](API.md).

| Platform | Compiler | fs/process/memory/time | env | sockets/tty/random | Signals | CI route |
|---|---|---|---|---|---|---|
| Linux amd64/arm64 | registered | source | source | source | unsupported raw signal API | native |
| Linux riscv64/loong64 | registered | source | source | source | unsupported raw signal API | QEMU user |
| macOS amd64/arm64 | registered | source | source; dedicated boundary regression | source | unsupported raw signal API | native |
| Windows amd64/arm64 | registered MSVC | source; some process/console operations limited | source | source; interface enumeration limited | separate console-control contract pending | native |
| FreeBSD amd64 | registered | source; incomplete operations remain | source/limited | source/limited | unsupported raw signal API | VM |
| FreeBSD arm64/riscv64 | registered | source contracts; not native-validated | source contracts | partial source contracts | unsupported | compile only |
| wasm32 WASI Preview 1 | registered | WASI fs/clocks/memory, limited process | source | sockets/tty unsupported; randomness reports NOSYS | unsupported | Node WASI |
| bare wasm32/wasm64 | registered | linear memory; hosted OS operations unsupported | unsupported | unsupported | unsupported | Node host, Memory64 for wasm64 |

Linux startup objects are owned by the compiler CRT; native Darwin/Windows/FreeBSD
linking requires the platform runtime. WASI `_start` is compiler-generated.
Freestanding object emission supplies no boot/startup contract. Other BSDs, Android,
iOS, Fuchsia and wasm64 WASI are planned and have no supported runtime claim here.

Remaining work: [Windows providers #385](https://github.com/wavefnd/Wave/issues/385),
[FreeBSD #386](https://github.com/wavefnd/Wave/issues/386),
[signals #384](https://github.com/wavefnd/Wave/issues/384),
[Linux interfaces #566](https://github.com/wavefnd/Wave/issues/566),
[Windows interfaces #790](https://github.com/wavefnd/Wave/issues/790).

## Provider source index

Every current `std::sys` source module is listed below. Facades preserve target
attributes and re-exports; an implementation with an unavailable path is not classified
as fully implemented from its filename. Follow its exact declarations in the API inventory.

| Module | Source role |
|---|---|
"###;

use crate::{cli::Args, error, package, process::Spec, storage, Result};
use std::collections::BTreeSet;
use std::ffi::OsString;
use std::path::PathBuf;
use syn::spanned::Spanned;

#[derive(Default)]
struct Functions {
    scope: usize,
    next: usize,
    ranges: Vec<(usize, usize, usize)>,
}
impl Functions {
    fn enter(&mut self) -> usize {
        let previous = self.scope;
        self.next += 1;
        self.scope = self.next;
        previous
    }

    fn record(&mut self, start: usize, end: usize) {
        self.ranges.push((start, end, self.scope));
    }
}
impl<'ast> syn::visit::Visit<'ast> for Functions {
    fn visit_item_fn(&mut self, item: &'ast syn::ItemFn) {
        self.record(item.span().start().line, item.block.span().end().line);
        syn::visit::visit_item_fn(self, item);
    }

    fn visit_impl_item_fn(&mut self, item: &'ast syn::ImplItemFn) {
        self.record(item.span().start().line, item.block.span().end().line);
        syn::visit::visit_impl_item_fn(self, item);
    }

    fn visit_trait_item_fn(&mut self, item: &'ast syn::TraitItemFn) {
        if let Some(block) = &item.default {
            self.record(item.span().start().line, block.span().end().line);
        }
        syn::visit::visit_trait_item_fn(self, item);
    }

    fn visit_block(&mut self, item: &'ast syn::Block) {
        let previous = self.enter();
        syn::visit::visit_block(self, item);
        self.scope = previous;
    }

    fn visit_item_mod(&mut self, item: &'ast syn::ItemMod) {
        let previous = self.enter();
        syn::visit::visit_item_mod(self, item);
        self.scope = previous;
    }

    fn visit_item_impl(&mut self, item: &'ast syn::ItemImpl) {
        let previous = self.enter();
        syn::visit::visit_item_impl(self, item);
        self.scope = previous;
    }

    fn visit_item_trait(&mut self, item: &'ast syn::ItemTrait) {
        let previous = self.enter();
        syn::visit::visit_item_trait(self, item);
        self.scope = previous;
    }
}

fn comment_only(text: &str) -> bool {
    let bytes = text.as_bytes();
    let mut i = 0;
    let mut depth = 0;
    while i < bytes.len() {
        if bytes[i..].starts_with(b"/*") {
            if depth == 0 && bytes[i..].starts_with(b"/*!") {
                return false;
            }
            depth += 1;
            i += 2;
        } else if depth > 0 && bytes[i..].starts_with(b"*/") {
            depth -= 1;
            i += 2;
        } else if depth > 0 || bytes[i].is_ascii_whitespace() {
            i += 1;
        } else if bytes[i..].starts_with(b"//") {
            if bytes[i..].starts_with(b"//!") {
                return false;
            }
            while i < bytes.len() && bytes[i] != b'\n' {
                i += 1;
            }
        } else {
            return false;
        }
    }
    depth == 0
}

pub fn function_spacing(text: &str) -> Result<String> {
    use syn::visit::Visit;
    let tree = syn::parse_file(text)?;
    let mut visitor = Functions::default();
    visitor.visit_file(&tree);
    let lines: Vec<_> = text.split_inclusive('\n').collect();
    let mut insert = BTreeSet::new();
    for scope in visitor.ranges.iter().map(|r| r.2).collect::<BTreeSet<_>>() {
        let mut functions: Vec<_> =
            visitor.ranges.iter().filter(|r| r.2 == scope).copied().collect();
        functions.sort();
        for pair in functions.windows(2) {
            let (_, end, _) = pair[0];
            let (start, _, _) = pair[1];
            if end >= start {
                continue;
            }
            let between = lines[end..start - 1].concat();
            if !comment_only(&between) {
                continue;
            }
            // Blank lines inside comments/attached doc attributes do not separate items.
            let mut block = 0usize;
            let mut separated = false;
            for line in &lines[end..start - 1] {
                if block == 0 && line.trim().is_empty() {
                    separated = true;
                    break;
                }
                let b = line.as_bytes();
                let mut i = 0;
                while i < b.len() {
                    if b[i..].starts_with(b"/*") {
                        block += 1;
                        i += 2;
                    } else if block > 0 && b[i..].starts_with(b"*/") {
                        block -= 1;
                        i += 2;
                    } else {
                        i += 1;
                    }
                }
            }
            if !separated {
                insert.insert(end);
            }
        }
    }
    let mut result = String::new();
    for (index, line) in lines.iter().enumerate() {
        result.push_str(line);
        if insert.contains(&(index + 1)) {
            result.push('\n');
        }
    }
    Ok(result)
}

fn tracked(pattern: &str) -> Result<Vec<PathBuf>> {
    let data = Spec::new(["git", "ls-files", "-z", "--", pattern]).checked()?.stdout;
    data.split(|b| *b == 0)
        .filter(|s| !s.is_empty())
        .map(|bytes| Ok(crate::root().join(std::str::from_utf8(bytes)?)))
        .collect()
}

pub fn format(args: &[OsString]) -> Result<i32> {
    let mut args = Args::new(args);
    let check = args.flag("--check");
    let spacing = args.flag("--spacing-only");
    let stdin = args.flag("--stdin");
    args.finish()?;
    if stdin {
        use std::io::Read;
        let mut text = String::new();
        std::io::stdin().read_to_string(&mut text)?;
        print!("{}", function_spacing(&text)?);
        return Ok(0);
    }
    if !spacing {
        let mut args = vec![
            OsString::from("cargo"),
            format!("+{}", crate::config::pin("RUST_VERSION")?).into(),
            "fmt".into(),
            "--all".into(),
        ];
        if check {
            args.extend(["--".into(), "--check".into()]);
        }
        Spec::new(args).checked()?;
    }
    let mut changed = false;
    let mut files = tracked("*.rs")?;
    files.extend(
        storage::files(&crate::root().join("tools/src"))?
            .into_iter()
            .filter(|p| p.extension().is_some_and(|v| v == "rs")),
    );
    files.sort();
    files.dedup();
    for path in files {
        if !path.is_file() {
            continue;
        }
        let text = std::fs::read_to_string(&path)?;
        let formatted = function_spacing(&text)?;
        if text != formatted {
            changed = true;
            if check {
                eprintln!("missing function spacing: {}", path.display());
            } else {
                storage::atomic_write(&path, formatted.as_bytes())?;
            }
        }
    }
    Ok(i32::from(check && changed))
}

pub fn public_declarations(source: &str) -> Result<Vec<(usize, Vec<String>, String)>> {
    let tokens = crate::source::tokens(source)?;
    let cleaned = crate::source::comments_masked(source, &tokens);
    let mut result = Vec::new();
    let lines: Vec<_> = cleaned.lines().collect();
    for (index, token) in tokens.iter().enumerate() {
        if token.kind != crate::source::Kind::Word || token.lexeme != "pub" {
            continue;
        }
        let span = token;
        let line_start = cleaned[..span.start].rfind('\n').map_or(0, |i| i + 1);
        if !cleaned[line_start..span.start].trim().is_empty() {
            continue;
        }
        let mut next = index + 1;
        if tokens.get(next).is_some_and(|t| t.lexeme == "async") {
            next += 1;
        }
        let kind = tokens.get(next).map(|t| t.lexeme.as_str()).unwrap_or("");
        if !["fun", "struct", "enum", "variant", "type", "const", "var", "import"].contains(&kind) {
            continue;
        }
        let mut parentheses = 0;
        let mut braces = 0;
        let mut end = None;
        for token in &tokens[next + 1..] {
            if token.kind != crate::source::Kind::Punct {
                continue;
            }
            let span = token;
            match token.lexeme.as_str() {
                "(" => parentheses += 1,
                ")" => parentheses -= 1,
                "{" => {
                    if kind == "fun" && parentheses == 0 {
                        end = Some(span.start);
                        break;
                    }
                    braces += 1;
                },
                "}" => {
                    braces -= 1;
                    if ["struct", "enum", "variant"].contains(&kind) && braces == 0 {
                        end = Some(span.end);
                        break;
                    }
                },
                ";" if parentheses == 0 && braces == 0 => {
                    end = Some(span.end);
                    break;
                },
                _ => {},
            }
        }
        let end = end.ok_or_else(|| error("unterminated public declaration"))?;
        let mut guards = Vec::new();
        let mut previous = token.line.saturating_sub(1);
        while previous > 0 {
            let line = lines[previous - 1].trim();
            if line.is_empty() {
                previous -= 1;
                continue;
            }
            if line.starts_with("#[") {
                guards.push(line.to_string());
                previous -= 1;
            } else {
                break;
            }
        }
        guards.reverse();
        result.push((token.line, guards, cleaned[span.start..end].trim().to_string()));
    }
    Ok(result)
}

pub fn std_api(arguments: &[OsString]) -> Result<i32> {
    let mut args = Args::new(arguments);
    let check = args.flag("--check");
    let output = args.path("--output-dir")?;
    args.finish()?;
    let root = crate::root();
    let output = output.unwrap_or_else(|| root.join("std"));
    std::fs::create_dir_all(&output)?;
    let mut api = API_HEADER.to_string();
    let mut systems = SYSTEMS_HEADER.to_string();
    for path in storage::files(&root.join("std"))?
        .into_iter()
        .filter(|p| p.extension().is_some_and(|v| v == "wave"))
    {
        let relative = path.strip_prefix(root.join("std"))?.to_string_lossy().replace('\\', "/");
        let source = std::fs::read_to_string(&path)?;
        let module = format!("std::{}", relative.trim_end_matches(".wave").replace('/', "::"));
        api += &format!("## `{module}`\n\n[Source]({relative})\n\n");
        let entries = public_declarations(&source)?;
        if entries.is_empty() {
            api += "No public declarations.\n\n";
        }
        for (line, guards, signature) in entries {
            api += &format!(
                "[Declaration]({relative}#L{line})\n\n```wave\n{}{signature}\n```\n\n",
                if guards.is_empty() { String::new() } else { guards.join("\n") + "\n" }
            );
        }
        if relative.starts_with("sys/") {
            let mut role = if source.contains("pub import(") {
                "target-selected facade/re-export"
            } else {
                "implementation source"
            }
            .to_string();
            if ["ERR_NOT_IMPLEMENTED", "return -38;", "return -52;"]
                .iter()
                .any(|marker| source.contains(marker))
            {
                role += "; unavailability markers present: inspect the operation contract";
            }
            systems+=&format!("| [{relative}]({relative}) | {role}; runtime coverage must be checked separately |\n");
        }
    }
    systems+="\nRefresh this index with `wave-tools maintain std-api` after adding a provider.\nReview the capability summary and exact target/CI table when its contract changes.\n";
    // Existing inventories end with one final LF, rather than an extra empty record.
    api = api.trim_end_matches('\n').to_string() + "\n";
    let mut stale = false;
    for (name, text) in [("API.md", api), ("SYSTEMS.md", systems)] {
        let path = output.join(name);
        if std::fs::read_to_string(&path).unwrap_or_default() != text {
            stale = true;
            if check {
                eprintln!("std/{name} is stale; run wave-tools maintain std-api");
            } else {
                storage::atomic_write(&path, text.as_bytes())?;
            }
        }
    }
    Ok(i32::from(check && stale))
}

pub fn maintainers(arguments: &[OsString]) -> Result<i32> {
    let mut args = Args::new(arguments);
    let validate = args.flag("--check");
    let text = std::fs::read_to_string(crate::root().join("MAINTAINERS"))?;
    let mut sections = Vec::new();
    let mut current: Option<(Vec<String>, Vec<String>)> = None;
    for line in text.lines().map(str::trim).filter(|l| !l.is_empty() && !l.starts_with('#')) {
        if line.starts_with('[') && line.ends_with(']') {
            if let Some(section) = current.take() {
                sections.push(section);
            }
            current = Some((vec![], vec![]));
        } else if let Some(person) = line.strip_prefix("M:") {
            current
                .as_mut()
                .ok_or_else(|| error("MAINTAINERS field has no section"))?
                .0
                .push(person.trim().into());
        } else if let Some(path) = line.strip_prefix("F:") {
            current
                .as_mut()
                .ok_or_else(|| error("MAINTAINERS field has no section"))?
                .1
                .push(path.trim().into());
        }
    }
    if let Some(section) = current {
        sections.push(section);
    }
    let matches = |pattern: &str, path: &str| {
        if let Some(directory) = pattern.strip_suffix('/') {
            path == directory || path.starts_with(pattern)
        } else {
            path == pattern
        }
    };
    if validate {
        args.finish()?;
        let tracked = tracked("*")?;
        let mut stale = BTreeSet::new();
        for (_, patterns) in &sections {
            for pattern in patterns {
                if !tracked.iter().any(|p| {
                    matches(
                        pattern,
                        &p.strip_prefix(crate::root())
                            .unwrap()
                            .to_string_lossy()
                            .replace('\\', "/"),
                    )
                }) {
                    stale.insert(pattern);
                }
            }
        }
        for pattern in &stale {
            eprintln!("stale maintainer pattern: {pattern}");
        }
        return Ok(i32::from(!stale.is_empty()));
    }
    let mut people = BTreeSet::new();
    for argument in args.values {
        let path = argument.to_string_lossy().replace('\\', "/");
        if path.starts_with('/') || path.contains(':') {
            continue;
        }
        let mut parts = Vec::new();
        let mut escaped = false;
        for part in path.split('/') {
            match part {
                "" | "." => {},
                ".." => {
                    if parts.pop().is_none() {
                        escaped = true;
                    }
                },
                part => parts.push(part),
            }
        }
        if escaped {
            continue;
        }
        let path = parts.join("/");
        for (maintainers, patterns) in &sections {
            if patterns.iter().any(|p| matches(p, &path)) {
                people.extend(maintainers);
            }
        }
    }
    for person in people {
        println!("{person}");
    }
    Ok(0)
}

pub fn main(arguments: &[OsString]) -> Result<i32> {
    let (command, rest) = arguments
        .split_first()
        .ok_or_else(|| error("maintain requires format/std-api/maintainers/unicode"))?;
    match command.to_str() {
        Some("format") => format(rest),
        Some("std-api") => std_api(rest),
        Some("maintainers") => maintainers(rest),
        Some("unicode") => unicode(rest),
        Some("verify-patch") => verify_patch(rest),
        _ => Err(error("unknown maintenance command")),
    }
}

fn unicode(arguments: &[OsString]) -> Result<i32> {
    let mut args = Args::new(arguments);
    let input = args.path("--input-dir")?;
    let output = args
        .path("--output")?
        .unwrap_or_else(|| crate::root().join("utils/src/display_width_tables.rs"));
    args.finish()?;
    let temporary = tempfile::tempdir()?;
    let data = input.as_deref().unwrap_or(temporary.path());
    let version = "17.0.0";
    let mut zero: BTreeSet<u32> = (0x1160..0x1200).collect();
    let mut wide = BTreeSet::new();
    for name in ["UnicodeData.txt", "EastAsianWidth.txt", "license.txt"] {
        let url = if name == "license.txt" {
            "https://www.unicode.org/license.txt".into()
        } else {
            format!("https://www.unicode.org/Public/{version}/ucd/{name}")
        };
        let path = data.join(name);
        if input.is_none() {
            package::run([
                OsString::from("curl"),
                "--fail".into(),
                "--location".into(),
                "--max-time".into(),
                "60".into(),
                url.into(),
                "-o".into(),
                path.clone().into_os_string(),
            ])?;
        }
        if name == "UnicodeData.txt" {
            for line in std::fs::read_to_string(path)?.lines() {
                let fields: Vec<_> = line.split(';').collect();
                if fields.len() > 2 && ["Mn", "Mc", "Me", "Cf"].contains(&fields[2]) {
                    zero.insert(u32::from_str_radix(fields[0], 16)?);
                }
            }
        } else if name == "EastAsianWidth.txt" {
            for line in std::fs::read_to_string(path)?.lines() {
                let line = line.split('#').next().unwrap().trim();
                if line.is_empty() {
                    continue;
                }
                let (range, width) =
                    line.split_once(';').ok_or_else(|| error("bad Unicode width record"))?;
                if !["W", "F"].contains(&width.trim()) {
                    continue;
                }
                let values: Vec<_> = range.trim().split("..").collect();
                let start = u32::from_str_radix(values[0], 16)?;
                let end = u32::from_str_radix(values.last().unwrap(), 16)?;
                wide.extend(start..=end);
            }
        }
    }
    let mut text=format!("// Generated by wave-tools maintain unicode; do not edit.\n// Unicode {version}: https://www.unicode.org/Public/{version}/ucd/UnicodeData.txt and EastAsianWidth.txt\n// SPDX-License-Identifier: Unicode-3.0\n//\n");
    text += &std::fs::read_to_string(data.join("license.txt"))?
        .lines()
        .map(|l| format!("// {l}\n"))
        .collect::<String>();
    text += &format!("\npub const UNICODE_VERSION: &str = \"{version}\";\n");
    for (name, values) in [("ZERO_WIDTH", zero), ("WIDE", wide)] {
        let mut ranges: Vec<(u32, u32)> = Vec::new();
        for value in values {
            if let Some(last) = ranges.last_mut().filter(|last| last.1 + 1 == value) {
                last.1 = value;
            } else {
                ranges.push((value, value));
            }
        }
        text += &format!("pub(super) const {name}: &[(u32, u32)] = &[\n");
        for (a, b) in ranges {
            text += &format!("    (0x{a:X}, 0x{b:X}),\n");
        }
        text += "];\n";
    }
    let inputs =
        ["UnicodeData.txt", "EastAsianWidth.txt", "license.txt"].map(|name| data.join(name));
    storage::validate_output(&output, &inputs)?;
    storage::atomic_write(&output, text.as_bytes())?;
    Ok(0)
}

fn patch_git(arguments: &[&str], input: Option<Vec<u8>>, cleanup: bool) -> Result<String> {
    let mut command = Spec::new(std::iter::once("git").chain(arguments.iter().copied()));
    command.input = input;
    command.cleanup = cleanup;
    command.timeout = std::time::Duration::from_secs(if cleanup { 30 } else { 120 });
    command.checked()?.strict_stdout()
}

fn verify_patch(arguments: &[OsString]) -> Result<i32> {
    if arguments.len() != 1 {
        return Err(error("maintain verify-patch requires one mail patch file"));
    }
    let patch = std::fs::canonicalize(std::path::Path::new(&arguments[0]))?;
    if !patch.is_file() {
        return Err(error("patch must be a regular file"));
    }
    let git_path = |name: &str| -> Result<PathBuf> {
        Ok(crate::root().join(patch_git(&["rev-parse", "--git-path", name], None, true)?.trim()))
    };
    if !patch_git(&["status", "--porcelain"], None, false)?.trim().is_empty()
        || git_path("rebase-apply")?.exists()
        || git_path("rebase-merge")?.exists()
    {
        return Err(error(
            "patch verification requires a clean checkout without an active rebase or git am",
        ));
    }
    let original = patch_git(&["rev-parse", "--symbolic-full-name", "HEAD"], None, false)?;
    let base = patch_git(&["rev-parse", "HEAD"], None, false)?;
    let original = if original.trim() == "HEAD" {
        base.trim()
    } else {
        original
            .trim()
            .strip_prefix("refs/heads/")
            .ok_or_else(|| error("unexpected checkout reference"))?
    };
    let mut random = [0u8; 16];
    getrandom::fill(&mut random)?;
    let branch =
        format!("patch-verify-{}", random.iter().map(|b| format!("{b:02x}")).collect::<String>());
    patch_git(&["checkout", "--quiet", "-b", &branch], None, false)?;
    // Ownership begins only after successful branch creation. Never force a
    // checkout/reset or delete a pre-existing branch during recovery.
    let result = (|| -> Result<()> {
        patch_git(
            &["am", patch.to_str().ok_or_else(|| error("patch path must be UTF-8"))?],
            None,
            false,
        )?;
        let commits = patch_git(&["rev-list", &format!("{}..HEAD", base.trim())], None, false)?;
        if commits.trim().is_empty() {
            return Err(error("patch contains no commits"));
        }
        let trailer = regex_lite::Regex::new(r"(?m)^Signed-off-by: .+ <[^<>\s]+@[^<>\s]+>$")?;
        for commit in commits.lines() {
            let body = patch_git(&["show", "-s", "--format=%B", commit], None, false)?;
            let trailers =
                patch_git(&["interpret-trailers", "--parse"], Some(body.into_bytes()), false)?;
            if !trailer.is_match(&trailers) {
                return Err(error(format!("missing DCO Signed-off-by trailer: {commit}")));
            }
        }
        if format(&["--check".into()])? != 0 {
            return Err(error("patch formatting failed"));
        }
        let toolchain = format!("+{}", crate::config::pin("RUST_VERSION")?);
        for arguments in [
            vec!["build", "--locked", "--release", "--jobs", "2"],
            vec!["test", "--locked", "--workspace", "--all-targets", "--jobs", "2"],
            vec![
                "clippy",
                "--locked",
                "--workspace",
                "--all-targets",
                "--jobs",
                "2",
                "--",
                "-D",
                "warnings",
            ],
        ] {
            let mut command = Spec::new(
                std::iter::once("cargo")
                    .chain(std::iter::once(toolchain.as_str()))
                    .chain(arguments),
            );
            command.stream = true;
            command.checked()?;
        }
        Ok(())
    })();
    let recovery = (|| -> Result<()> {
        if git_path("rebase-apply")?.exists() {
            patch_git(&["am", "--abort"], None, true)?;
        }
        patch_git(&["checkout", "--quiet", original], None, true)?;
        patch_git(&["branch", "-D", &branch], None, true)?;
        Ok(())
    })();
    if let Err(recovery) = recovery {
        return Err(error(format!("could not restore {original}; preserved {branch} for recovery: {recovery}; verification: {}", result.err().map(|e| e.to_string()).unwrap_or_else(|| "passed".into()))));
    }
    result?;
    println!("Patch verification passed; original checkout restored.");
    Ok(0)
}

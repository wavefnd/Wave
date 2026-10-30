//! Import graph construction, module visibility, and namespace lowering.
//!
//! Wave's backend still consumes one concrete AST. This pass preserves module
//! boundaries while resolving imports, then gives every imported declaration a
//! collision-free internal name before the existing semantic and LLVM phases.

use ::error::{RelatedDiagnostic, SourceSpan, WaveError, WaveErrorKind};
use ::parser::ast::*;
use ::parser::import::{local_import_unit_with_config, ImportConfig};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct ModuleSource {
    pub path: PathBuf,
    pub source: String,
}

#[derive(Debug)]
pub struct ResolvedModuleGraph {
    pub ast: Vec<ASTNode>,
    pub origins: Vec<usize>,
    pub sources: Vec<ModuleSource>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SymbolKind {
    Function,
    Struct,
    VariantConstructor,
    Type,
    Value,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ModuleSymbol {
    lowered: String,
    visibility: Visibility,
    kind: SymbolKind,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct ModuleInterface {
    symbols: HashMap<String, ModuleSymbol>,
    unavailable_socket: Option<SocketCapability>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SocketCapability {
    target: String,
    exports: HashSet<String>,
}

#[derive(Default)]
struct NameContext {
    own: HashMap<String, ModuleSymbol>,
    selected: HashMap<String, ModuleSymbol>,
    namespaces: HashMap<String, ModuleInterface>,
    selected_sites: HashMap<String, SourceSpan>,
    namespace_sites: HashMap<String, SourceSpan>,
}

struct Resolver<'a> {
    config: &'a ImportConfig,
    interfaces: HashMap<PathBuf, ModuleInterface>,
    visiting: Vec<PathBuf>,
    source_indices: HashMap<PathBuf, usize>,
    ast: Vec<ASTNode>,
    origins: Vec<usize>,
    sources: Vec<ModuleSource>,
}

pub fn resolve_import_graph(
    entry_path: &Path,
    entry_source: &str,
    ast: Vec<ASTNode>,
    config: &ImportConfig,
) -> Result<ResolvedModuleGraph, WaveError> {
    ::error::SourceManager::record(entry_path.display().to_string(), entry_source.to_string());
    let entry_key = canonical_key(entry_path);
    let mut resolver = Resolver {
        config,
        interfaces: HashMap::new(),
        visiting: Vec::new(),
        source_indices: HashMap::from([(entry_key.clone(), 0)]),
        ast: Vec::new(),
        origins: Vec::new(),
        sources: vec![ModuleSource {
            path: entry_path.to_path_buf(),
            source: entry_source.to_string(),
        }],
    };

    resolver.resolve_module(entry_key.clone(), ast, 0, true)?;
    Ok(ResolvedModuleGraph {
        ast: resolver.ast,
        origins: resolver.origins,
        sources: resolver.sources,
    })
}

/// AST dumps do not require dependencies to exist or exported names to bind.
/// Still retain every existing source opened by the normal import loader, even
/// when that source cannot be parsed, so a dump cannot replace an input.
pub(crate) fn existing_import_paths(
    entry_path: &Path,
    ast: Vec<ASTNode>,
    config: &ImportConfig,
) -> Result<Vec<PathBuf>, WaveError> {
    let entry = canonical_key(entry_path);
    let mut loaded = HashSet::from([entry.to_string_lossy().into_owned()]);
    let mut pending = vec![(entry, ast, 1usize)];
    while let Some((path, nodes, depth)) = pending.pop() {
        let base = path.parent().unwrap_or(Path::new("."));
        for node in nodes {
            if let ASTNode::Statement(StatementNode::Import(import)) = node.unspanned() {
                if let Ok(unit) =
                    local_import_unit_with_config(&import.path, &mut loaded, base, config)
                {
                    if depth >= 512 {
                        return Err(module_error(
                            &path,
                            "Import depth exceeded",
                            "import nesting exceeds the maximum of 512 levels",
                            "reduce the depth of the import chain",
                        )
                        .with_span(node.span()));
                    }
                    pending.push((unit.abs_path, unit.ast, depth + 1));
                }
            }
        }
    }
    let mut paths: Vec<_> = loaded.into_iter().map(PathBuf::from).collect();
    paths.sort();
    Ok(paths)
}

/// Removes the internal module hash prefix from names shown to Wave users.
pub fn demangle_module_names(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut rest = text;
    const PREFIX: &str = "__wave_";
    while let Some(index) = rest.find(PREFIX) {
        out.push_str(&rest[..index]);
        let candidate = &rest[index + PREFIX.len()..];
        if candidate.len() >= 17
            && candidate.as_bytes()[..16].iter().all(|byte| byte.is_ascii_hexdigit())
            && candidate.as_bytes()[16] == b'_'
        {
            rest = &candidate[17..];
        } else {
            out.push_str(PREFIX);
            rest = candidate;
        }
    }
    out.push_str(rest);
    out
}

impl Resolver<'_> {
    fn resolve_module(
        &mut self,
        key: PathBuf,
        ast: Vec<ASTNode>,
        origin: usize,
        is_entry: bool,
    ) -> Result<ModuleInterface, WaveError> {
        struct Frame {
            key: PathBuf,
            ast: Vec<ASTNode>,
            origin: usize,
            is_entry: bool,
            interface: ModuleInterface,
            names: NameContext,
            next: usize,
            pending: Option<(usize, PathBuf)>,
        }
        let config = self.config;
        let frame =
            |key: PathBuf, ast: Vec<ASTNode>, origin, is_entry| -> Result<Frame, WaveError> {
                let mut interface = collect_symbols(&key, &ast, is_entry)?;
                interface.unavailable_socket = unavailable_socket(&key, config);
                let names =
                    NameContext { own: interface.symbols.clone(), ..NameContext::default() };
                Ok(Frame { key, ast, origin, is_entry, interface, names, next: 0, pending: None })
            };
        let root = key.clone();
        let mut stack = vec![frame(key.clone(), ast, origin, is_entry)?];
        self.visiting.push(key);
        let operation = (|| {
            while let Some(mut current) = stack.pop() {
                if let Some((index, child_key)) = current.pending.take() {
                    let node = &current.ast[index];
                    let ASTNode::Statement(StatementNode::Import(import)) = node.unspanned() else {
                        unreachable!()
                    };
                    let child = self.interfaces[&child_key].clone();
                    bind_import(
                        &current.key,
                        import,
                        child.clone(),
                        &mut current.names,
                        node.span(),
                        &self.sources[current.origin].source,
                    )
                    .map_err(|e| {
                        import_failure(
                            e,
                            node.span(),
                            &self.sources[current.origin].source,
                            &import.path,
                        )
                    })?;
                    if import.visibility == Visibility::Public {
                        for selected in &import.selections {
                            let symbol = child.symbols[selected].clone();
                            if current.interface.symbols.insert(selected.clone(), symbol).is_some()
                            {
                                return Err(module_error(
                                    &current.key,
                                    "Duplicate public symbol",
                                    format!(
                                        "re-exported symbol '{}' conflicts in this module",
                                        selected
                                    ),
                                    "remove one re-export or rename the local declaration",
                                ));
                            }
                            let prefix = format!("{selected}::");
                            for (qualified, constructor) in &child.symbols {
                                if qualified.starts_with(&prefix)
                                    && constructor.kind == SymbolKind::VariantConstructor
                                {
                                    current
                                        .interface
                                        .symbols
                                        .insert(qualified.clone(), constructor.clone());
                                }
                            }
                        }
                    }
                }
                let imported = (current.next..current.ast.len()).find(|&index| {
                    matches!(
                        current.ast[index].unspanned(),
                        ASTNode::Statement(StatementNode::Import(_))
                    )
                });
                let Some(index) = imported else {
                    let mut lowered = Vec::new();
                    for node in current.ast {
                        if !matches!(node.unspanned(), ASTNode::Statement(StatementNode::Import(_)))
                        {
                            lowered.push(rewrite_top_level(
                                node,
                                &current.names,
                                &current.key,
                                current.is_entry,
                            )?);
                        }
                    }
                    self.visiting.pop();
                    self.interfaces.insert(current.key, current.interface);
                    self.origins.extend(std::iter::repeat_n(current.origin, lowered.len()));
                    self.ast.extend(lowered);
                    continue;
                };
                current.next = index + 1;
                let node = &current.ast[index];
                let ASTNode::Statement(StatementNode::Import(import)) = node.unspanned() else {
                    unreachable!()
                };
                let base_dir = current.key.parent().unwrap_or(Path::new("."));
                let unit = local_import_unit_with_config(
                    &import.path,
                    &mut HashSet::new(),
                    base_dir,
                    config,
                )
                .map_err(|e| {
                    import_failure(
                        e,
                        node.span(),
                        &self.sources[current.origin].source,
                        &import.path,
                    )
                })?;
                ::error::SourceManager::record(
                    unit.abs_path.display().to_string(),
                    unit.source.clone(),
                );
                let child_key = canonical_key(&unit.abs_path);
                let child_origin = if let Some(index) = self.source_indices.get(&child_key) {
                    *index
                } else {
                    let index = self.sources.len();
                    self.sources.push(ModuleSource { path: unit.abs_path, source: unit.source });
                    self.source_indices.insert(child_key.clone(), index);
                    index
                };
                if !self.interfaces.contains_key(&child_key) {
                    let error = if let Some(start) =
                        self.visiting.iter().position(|key| key == &child_key)
                    {
                        let mut chain = self.visiting[start..]
                            .iter()
                            .map(|p| p.display().to_string())
                            .collect::<Vec<_>>();
                        chain.push(child_key.display().to_string());
                        Some(module_error(&child_key, "Import cycle", format!("import cycle detected: {}", chain.join(" -> ")),
                            "remove one dependency edge or move shared declarations into a third module"))
                    } else if self.visiting.len() >= 512 {
                        Some(module_error(
                            &current.key,
                            "Import depth exceeded",
                            "import nesting exceeds the maximum of 512 levels",
                            "reduce the depth of the import chain",
                        ))
                    } else {
                        None
                    };
                    if let Some(error) = error {
                        return Err(import_failure(
                            error,
                            node.span(),
                            &self.sources[current.origin].source,
                            &import.path,
                        ));
                    }
                    current.pending = Some((index, child_key.clone()));
                    stack.push(current);
                    self.visiting.push(child_key.clone());
                    stack.push(frame(child_key, unit.ast, child_origin, false)?);
                } else {
                    current.pending = Some((index, child_key));
                    stack.push(current);
                }
            }
            Ok(self.interfaces[&root].clone())
        })();
        self.visiting.clear();
        operation.map_err(|mut error| {
            // Reconstruct the same related import provenance while unwinding
            // heap frames, without recursive calls or growing the OS stack.
            for parent in stack.iter().rev() {
                if let Some((index, _)) = &parent.pending {
                    let node = &parent.ast[*index];
                    let ASTNode::Statement(StatementNode::Import(import)) = node.unspanned() else {
                        continue;
                    };
                    error = import_failure(
                        error,
                        node.span(),
                        &self.sources[parent.origin].source,
                        &import.path,
                    );
                }
            }
            error
        })
    }
}

fn canonical_key(path: &Path) -> PathBuf {
    path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
}

// Loader failures have no physical dependency span; source failures inside a
// dependency keep their primary range and gain the importing declaration.
fn import_failure(
    mut error: WaveError,
    site: Option<&SourceSpan>,
    source: &str,
    import: &str,
) -> WaveError {
    if let Some(site) = site {
        if error.span.is_some() {
            if error.span.as_ref().is_none_or(|primary| {
                primary.file != site.file || primary.start < site.start || primary.end > site.end
            }) {
                error.related.push(RelatedDiagnostic {
                    message: format!("imported through `{import}`"),
                    span: site.clone(),
                });
            }
        } else {
            let attempted = error.file.clone();
            error = error.with_span(Some(site)).with_source_code(source);
            let context = format!("while resolving `{import}` (dependency: {attempted})");
            error.note =
                Some(error.note.map_or_else(|| context.clone(), |old| format!("{old}; {context}")));
        }
        if error.file == site.file {
            error.source_code = Some(source.into());
        }
    }
    error
}

fn import_symbol_span(
    site: Option<&SourceSpan>,
    source: &str,
    selected: &str,
) -> Option<SourceSpan> {
    let site = site?;
    let raw = source.get(site.start..site.end)?;
    let tokens = lexer::Lexer::new(raw).tokenize().ok()?;
    let mut span = tokens.into_iter().find(|t| matches!(&t.token_type, lexer::token::TokenType::Identifier(name) if name == selected))?.span?;
    span.start += site.start;
    span.end += site.start;
    if span.line == 1 {
        span.column += site.column - 1;
    }
    if span.end_line == 1 {
        span.end_column += site.column - 1;
    }
    span.line += site.line - 1;
    span.end_line += site.line - 1;
    span.file = site.file.clone();
    Some(span)
}

// Recognize only names that the selected std actually exports on native OSes.
// This is a bounded diagnostic for the existing socket provider contract, not
// a new target-capability system or a promise of WebAssembly socket support.
fn unavailable_socket(path: &Path, config: &ImportConfig) -> Option<SocketCapability> {
    if !matches!(config.target.arch.as_deref(), Some("wasm32" | "wasm64")) {
        return None;
    }
    let std = config.std_root.as_ref().ok()?;
    if path != std.path().join("sys/socket.wave") {
        return None;
    }
    let raw = std::fs::read_to_string(path).ok()?;
    let declarations = raw
        .lines()
        .filter(|line| !line.trim_start().starts_with("#[target("))
        .collect::<Vec<_>>()
        .join("\n");
    let tokens = lexer::Lexer::new(&declarations).tokenize().ok()?;
    let nodes = ::parser::parser::parse_syntax_with_spans(&tokens).ok()?;
    let mut exports = HashSet::new();
    for node in nodes {
        if let ASTNode::Statement(StatementNode::Import(import)) = node.unspanned() {
            if import.visibility == Visibility::Public {
                exports.extend(import.selections.iter().cloned());
            }
        }
    }
    Some(SocketCapability {
        target: format!(
            "{}-{}-{}",
            config.target.arch.as_deref().unwrap_or("unknown"),
            config.target.os.as_deref().unwrap_or("unknown"),
            config.target.env.as_deref().unwrap_or("unknown")
        ),
        exports,
    })
}

fn missing_imported_symbol(
    path: &Path,
    interface: &ModuleInterface,
    namespace: &str,
    name: &str,
) -> WaveError {
    if let Some(capability) = &interface.unavailable_socket {
        if capability.exports.contains(name) {
            return module_error(path, "Unavailable target capability",
                format!("native sockets are unavailable for target `{}`: `{namespace}::{name}` required by `{}`", capability.target, path.display()),
                "WASI Preview 1 and browser WebAssembly do not provide this native socket provider; select a supported native target")
                .with_context("target capability");
        }
    }
    module_error(
        path,
        "Unknown imported symbol",
        format!("module '{namespace}' has no symbol '{name}'"),
        "check the public declaration name and selected imports",
    )
}

fn module_error(
    path: &Path,
    title: &str,
    message: impl Into<String>,
    help: impl Into<String>,
) -> WaveError {
    WaveError::new(
        WaveErrorKind::InvalidStatement(title.to_string()),
        message,
        path.display().to_string(),
        1,
        1,
    )
    .with_code("E3001")
    .with_context("module resolution")
    .with_help(help)
}

fn internal_name(path: &Path, name: &str, is_entry: bool) -> String {
    if is_entry {
        return name.to_string();
    }
    let mut hash = 0xcbf29ce484222325_u64;
    for byte in path.to_string_lossy().as_bytes() {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    format!("__wave_{hash:016x}_{name}")
}

fn insert_symbol(
    path: &Path,
    symbols: &mut HashMap<String, ModuleSymbol>,
    source_name: &str,
    visibility: Visibility,
    kind: SymbolKind,
    is_entry: bool,
) -> Result<(), WaveError> {
    if symbols.contains_key(source_name) {
        // The semantic validator owns duplicate-declaration diagnostics because
        // it can point at the repeated source declaration. Both declarations
        // receive the same lowered name, so flattening cannot hide the error.
        return Ok(());
    }
    symbols.insert(
        source_name.to_string(),
        ModuleSymbol { lowered: internal_name(path, source_name, is_entry), visibility, kind },
    );
    Ok(())
}

fn collect_symbols(
    path: &Path,
    ast: &[ASTNode],
    is_entry: bool,
) -> Result<ModuleInterface, WaveError> {
    let mut symbols = HashMap::new();
    for node in ast {
        match node.unspanned() {
            ASTNode::Function(function) => {
                if !is_entry && function.name == "main" {
                    return Err(module_error(
                        path,
                        "Invalid module entry point",
                        "function 'main' may only be declared in the entry module",
                        "remove `main` from the library module or rename it",
                    ));
                }
                insert_symbol(
                    path,
                    &mut symbols,
                    &function.name,
                    function.visibility,
                    SymbolKind::Function,
                    is_entry,
                )?;
            },
            ASTNode::ExternFunction(function) => insert_symbol(
                path,
                &mut symbols,
                &function.name,
                Visibility::Private,
                SymbolKind::Function,
                is_entry,
            )?,
            ASTNode::Struct(structure) => insert_symbol(
                path,
                &mut symbols,
                &structure.name,
                structure.visibility,
                SymbolKind::Struct,
                is_entry,
            )?,
            ASTNode::TypeAlias(alias) => insert_symbol(
                path,
                &mut symbols,
                &alias.name,
                alias.visibility,
                SymbolKind::Type,
                is_entry,
            )?,
            ASTNode::Enum(enumeration) => {
                insert_symbol(
                    path,
                    &mut symbols,
                    &enumeration.name,
                    enumeration.visibility,
                    SymbolKind::Type,
                    is_entry,
                )?;
                for variant in &enumeration.variants {
                    insert_symbol(
                        path,
                        &mut symbols,
                        &variant.name,
                        Visibility::Private,
                        SymbolKind::Value,
                        is_entry,
                    )?;
                    symbols.insert(
                        format!("{}::{}", enumeration.name, variant.name),
                        ModuleSymbol {
                            lowered: internal_name(path, &variant.name, is_entry),
                            visibility: enumeration.visibility,
                            kind: SymbolKind::Value,
                        },
                    );
                }
            },
            ASTNode::Variant(variant) => {
                insert_symbol(
                    path,
                    &mut symbols,
                    &variant.name,
                    variant.visibility,
                    SymbolKind::Type,
                    is_entry,
                )?;
                let lowered_owner = internal_name(path, &variant.name, is_entry);
                for case in &variant.cases {
                    symbols.insert(
                        format!("{}::{}", variant.name, case.name),
                        ModuleSymbol {
                            lowered: format!("{}::{}", lowered_owner, case.name),
                            visibility: variant.visibility,
                            kind: SymbolKind::VariantConstructor,
                        },
                    );
                }
            },
            ASTNode::Variable(variable)
                if matches!(variable.mutability, Mutability::Const | Mutability::Static) =>
            {
                insert_symbol(
                    path,
                    &mut symbols,
                    &variable.name,
                    variable.visibility,
                    SymbolKind::Value,
                    is_entry,
                )?;
            },
            _ => {},
        }
    }
    Ok(ModuleInterface { symbols, unavailable_socket: None })
}

fn default_namespace(path: &str) -> Option<String> {
    if path.starts_with("./") {
        return Path::new(path).file_stem().and_then(|name| name.to_str()).map(str::to_string);
    }
    Some(path.to_string())
}

fn bind_import(
    current_path: &Path,
    import: &ImportNode,
    interface: ModuleInterface,
    names: &mut NameContext,
    site: Option<&SourceSpan>,
    source: &str,
) -> Result<(), WaveError> {
    if !import.selections.is_empty() {
        for selected in &import.selections {
            let focus = import_symbol_span(site, source, selected);
            let fail = |error: WaveError| {
                let mut error = error.with_span(focus.as_ref());
                if let Some(previous) = names.selected_sites.get(selected) {
                    error.related.push(RelatedDiagnostic {
                        message: "previous import binding".into(),
                        span: previous.clone(),
                    });
                }
                error
            };
            let Some(symbol) = interface.symbols.get(selected) else {
                return Err(fail(missing_imported_symbol(
                    current_path,
                    &interface,
                    &import.path,
                    selected,
                )));
            };
            if symbol.visibility != Visibility::Public {
                return Err(fail(module_error(
                    current_path,
                    "Private imported symbol",
                    format!("symbol '{}' is private in module '{}'", selected, import.path),
                    "mark the declaration `pub` or remove it from the import list",
                )));
            }
            if names.own.contains_key(selected) {
                return Err(fail(module_error(
                    current_path,
                    "Ambiguous imported symbol",
                    format!("imported symbol '{}' conflicts in this module", selected),
                    "rename the local declaration or use a qualified module import",
                )));
            }
            if let Some(existing) = names.selected.get(selected) {
                if existing == symbol {
                    continue;
                }
                return Err(fail(module_error(
                    current_path,
                    "Ambiguous imported symbol",
                    format!("imported symbol '{}' conflicts in this module", selected),
                    "use a qualified module import to disambiguate the declarations",
                )));
            }
            if let Some(focus) = focus {
                names.selected_sites.insert(selected.clone(), focus);
            }
            names.selected.insert(selected.clone(), symbol.clone());
            let prefix = format!("{}::", selected);
            for (qualified, constructor) in &interface.symbols {
                if qualified.starts_with(&prefix)
                    && constructor.kind == SymbolKind::VariantConstructor
                {
                    names.selected.insert(qualified.clone(), constructor.clone());
                }
            }
        }
        return Ok(());
    }

    let namespace =
        import.alias.clone().or_else(|| default_namespace(&import.path)).ok_or_else(|| {
            module_error(
                current_path,
                "Invalid import namespace",
                format!("cannot derive a namespace from import '{}'", import.path),
                "provide an explicit alias with `as name`",
            )
        })?;

    if let Some(existing) = names.namespaces.get(&namespace) {
        if existing == &interface {
            return Ok(());
        }
        let mut error = module_error(
            current_path,
            "Duplicate import namespace",
            format!("namespace '{}' is bound to multiple modules", namespace),
            "use a distinct `as` alias for one import",
        );
        if let Some(previous) = names.namespace_sites.get(&namespace) {
            error.related.push(RelatedDiagnostic {
                message: "previous namespace binding".into(),
                span: previous.clone(),
            });
        }
        return Err(error);
    }
    if let Some(site) = site {
        names.namespace_sites.insert(namespace.clone(), site.clone());
    }
    names.namespaces.insert(namespace, interface);
    Ok(())
}

fn resolve_name(
    name: &str,
    names: &NameContext,
    path: &Path,
) -> Result<Option<ModuleSymbol>, WaveError> {
    if name.contains("::") {
        if let Some(symbol) = names.own.get(name).or_else(|| names.selected.get(name)) {
            return Ok(Some(symbol.clone()));
        }
        let mut best: Option<(&str, &ModuleInterface)> = None;
        for (namespace, interface) in &names.namespaces {
            if name.strip_prefix(namespace).is_some_and(|rest| rest.starts_with("::"))
                && best.is_none_or(|(current, _)| namespace.len() > current.len())
            {
                best = Some((namespace, interface));
            }
        }
        let Some((namespace, interface)) = best else {
            return Err(module_error(
                path,
                "Unknown import namespace",
                format!("qualified name '{}' uses an unknown module namespace", name),
                "import the module first or check its alias",
            )
            .with_suggestions(utils::suggestions::nearby(
                name.split("::").next().unwrap_or(name),
                {
                    let mut candidates = names.namespaces.keys().cloned().collect::<Vec<_>>();
                    candidates.sort_by_key(|candidate| {
                        (
                            names
                                .namespace_sites
                                .get(candidate)
                                .map(|span| (span.file.clone(), span.start)),
                            candidate.clone(),
                        )
                    });
                    candidates
                        .into_iter()
                        .enumerate()
                        .map(|(order, candidate)| (candidate, 0, order))
                        .collect()
                },
            )));
        };
        let symbol_name = &name[namespace.len() + 2..];
        let Some(symbol) = interface.symbols.get(symbol_name) else {
            return Err(missing_imported_symbol(path, interface, namespace, symbol_name));
        };
        if symbol.visibility != Visibility::Public {
            return Err(module_error(
                path,
                "Private imported symbol",
                format!("symbol '{}' is private in module '{}'", symbol_name, namespace),
                "only `pub` declarations are accessible outside their module",
            ));
        }
        return Ok(Some(symbol.clone()));
    }

    Ok(names.own.get(name).or_else(|| names.selected.get(name)).cloned())
}

fn rewrite_type(mut ty: WaveType, names: &NameContext, path: &Path) -> Result<WaveType, WaveError> {
    let mut current = &mut ty;
    loop {
        match current {
            WaveType::Future(inner) | WaveType::Pointer(inner) | WaveType::Array(inner, _) => {
                current = inner
            },
            WaveType::Struct(name) | WaveType::Variant(name) => {
                *name = rewrite_type_name(name, names, path)?;
                break;
            },
            _ => break,
        }
    }
    Ok(ty)
}

fn rewrite_type_name(name: &str, names: &NameContext, path: &Path) -> Result<String, WaveError> {
    if let Some((base, arguments)) = split_type_application(name) {
        let rewritten_base = rewrite_type_name(base, names, path)?;
        let rewritten_arguments = split_type_arguments(arguments)
            .into_iter()
            .map(|argument| rewrite_type_name(argument.trim(), names, path))
            .collect::<Result<Vec<_>, _>>()?;
        return Ok(format!("{}<{}>", rewritten_base, rewritten_arguments.join(",")));
    }

    match resolve_name(name, names, path)? {
        Some(symbol) if matches!(symbol.kind, SymbolKind::Struct | SymbolKind::Type) => {
            Ok(symbol.lowered)
        },
        Some(_) => Err(module_error(
            path,
            "Expected imported type",
            format!("symbol '{}' is not a type", name),
            "use a public struct, enum, or type alias in this position",
        )),
        None => Ok(name.to_string()),
    }
}

fn split_type_application(name: &str) -> Option<(&str, &str)> {
    let open = name.find('<')?;
    if !name.ends_with('>') || open == 0 {
        return None;
    }
    Some((&name[..open], &name[open + 1..name.len() - 1]))
}

fn split_type_arguments(arguments: &str) -> Vec<&str> {
    let mut out = Vec::new();
    let mut depth = 0_i32;
    let mut start = 0;
    for (index, ch) in arguments.char_indices() {
        match ch {
            '<' => depth += 1,
            '>' => depth -= 1,
            ',' if depth == 0 => {
                out.push(&arguments[start..index]);
                start = index + 1;
            },
            _ => {},
        }
    }
    out.push(&arguments[start..]);
    out
}

fn rewrite_top_level(
    node: ASTNode,
    names: &NameContext,
    path: &Path,
    is_entry: bool,
) -> Result<ASTNode, WaveError> {
    match node {
        ASTNode::Located { value, span } => {
            Ok(rewrite_top_level(*value, names, path, is_entry)?.with_span(Some(span)))
        },
        ASTNode::Function(mut function) => {
            let original = function.name.clone();
            function.name = names.own[&original].lowered.clone();
            if !is_entry && original == "main" {
                function.name = internal_name(path, "main", false);
            }
            if !is_entry {
                if let Some(export) = &mut function.export {
                    if export.symbol.is_none() {
                        export.symbol = Some(original);
                    }
                }
            }
            rewrite_function(&mut function, names, path)?;
            Ok(ASTNode::Function(function))
        },
        ASTNode::ExternFunction(mut function) => {
            function.name = names.own[&function.name].lowered.clone();
            function.params = function
                .params
                .into_iter()
                .map(|(name, ty)| Ok((name, rewrite_type(ty, names, path)?)))
                .collect::<Result<_, WaveError>>()?;
            function.return_type = rewrite_type(function.return_type, names, path)?;
            Ok(ASTNode::ExternFunction(function))
        },
        ASTNode::Struct(mut structure) => {
            structure.name = names.own[&structure.name].lowered.clone();
            structure.fields = structure
                .fields
                .into_iter()
                .map(|(name, ty)| Ok((name, rewrite_type(ty, names, path)?)))
                .collect::<Result<_, WaveError>>()?;
            for method in &mut structure.methods {
                rewrite_function(method, names, path)?;
            }
            Ok(ASTNode::Struct(structure))
        },
        ASTNode::TypeAlias(mut alias) => {
            alias.name = names.own[&alias.name].lowered.clone();
            alias.target = rewrite_type(alias.target, names, path)?;
            Ok(ASTNode::TypeAlias(alias))
        },
        ASTNode::Enum(mut enumeration) => {
            enumeration.name = names.own[&enumeration.name].lowered.clone();
            enumeration.repr_type = rewrite_type(enumeration.repr_type, names, path)?;
            for variant in &mut enumeration.variants {
                variant.name = names.own[&variant.name].lowered.clone();
            }
            Ok(ASTNode::Enum(enumeration))
        },
        ASTNode::Variant(mut variant) => {
            variant.name = names.own[&variant.name].lowered.clone();
            for case in &mut variant.cases {
                case.payload_types = case
                    .payload_types
                    .drain(..)
                    .map(|ty| rewrite_type(ty, names, path))
                    .collect::<Result<_, _>>()?;
            }
            Ok(ASTNode::Variant(variant))
        },
        ASTNode::Variable(mut variable) => {
            variable.name = names.own[&variable.name].lowered.clone();
            variable.type_name = rewrite_type(variable.type_name, names, path)?;
            if let Some(value) = variable.initial_value.take() {
                variable.initial_value =
                    Some(rewrite_expression(value, names, path, &HashSet::new())?);
            }
            Ok(ASTNode::Variable(variable))
        },
        ASTNode::ProtoImpl(mut implementation) => {
            implementation.target = resolve_name(&implementation.target, names, path)?
                .map_or(implementation.target, |symbol| symbol.lowered);
            for method in &mut implementation.methods {
                rewrite_function(method, names, path)?;
            }
            Ok(ASTNode::ProtoImpl(implementation))
        },
        ASTNode::Statement(statement) => {
            Ok(ASTNode::Statement(rewrite_statement(statement, names, path, &mut HashSet::new())?))
        },
        ASTNode::Expression(expression) => {
            Ok(ASTNode::Expression(rewrite_expression(expression, names, path, &HashSet::new())?))
        },
        other => Ok(other),
    }
}

fn rewrite_function(
    function: &mut FunctionNode,
    names: &NameContext,
    path: &Path,
) -> Result<(), WaveError> {
    for parameter in &mut function.parameters {
        parameter.param_type = rewrite_type(parameter.param_type.clone(), names, path)?;
    }
    function.return_type =
        function.return_type.take().map(|ty| rewrite_type(ty, names, path)).transpose()?;
    let mut locals =
        function.parameters.iter().map(|parameter| parameter.name.clone()).collect::<HashSet<_>>();
    function.body = rewrite_block(std::mem::take(&mut function.body), names, path, &mut locals)?;
    Ok(())
}

fn rewrite_block(
    mut nodes: Vec<ASTNode>,
    names: &NameContext,
    path: &Path,
    locals: &mut HashSet<String>,
) -> Result<Vec<ASTNode>, WaveError> {
    use std::cell::RefCell;
    use std::rc::Rc;
    type Scope = Rc<RefCell<HashSet<String>>>;
    enum Work<'a> {
        Node(&'a mut ASTNode, Scope),
        Expression(&'a mut Expression, Scope),
        MatchArm(&'a mut MatchArm, Scope),
        ForTail(&'a mut Expression, &'a mut Expression, &'a mut Vec<ASTNode>, Scope),
    }
    fn enqueue<'a>(nodes: &'a mut [ASTNode], scope: &Scope, work: &mut Vec<Work<'a>>) {
        for node in nodes.iter_mut().rev() {
            work.push(Work::Node(node, scope.clone()));
        }
    }
    let root = Rc::new(RefCell::new(locals.clone()));
    let mut work = Vec::new();
    enqueue(&mut nodes, &root, &mut work);
    while let Some(next) = work.pop() {
        match next {
            Work::Expression(expression, scope) => {
                rewrite_expression_in_place(expression, names, path, &scope.borrow())?;
            },
            Work::MatchArm(arm, scope) => {
                rewrite_match_pattern(&mut arm.pattern, names, path, &scope.borrow())?;
                let mut locals = scope.borrow().clone();
                collect_pattern_bindings(&arm.pattern, &mut locals);
                enqueue(&mut arm.body, &Rc::new(RefCell::new(locals)), &mut work);
            },
            Work::ForTail(condition, increment, body, scope) => {
                rewrite_expression_in_place(condition, names, path, &scope.borrow())?;
                rewrite_expression_in_place(increment, names, path, &scope.borrow())?;
                enqueue(body, &scope, &mut work);
            },
            Work::Node(node, scope) => match node {
                ASTNode::Located { value, .. } => work.push(Work::Node(value, scope)),
                ASTNode::Variable(variable) => {
                    variable.type_name = rewrite_type(variable.type_name.clone(), names, path)?;
                    if let Some(value) = &mut variable.initial_value {
                        rewrite_expression_in_place(value, names, path, &scope.borrow())?;
                    }
                    scope.borrow_mut().insert(variable.name.clone());
                },
                ASTNode::Expression(expression) => {
                    rewrite_expression_in_place(expression, names, path, &scope.borrow())?
                },
                ASTNode::Statement(statement) => match statement {
                    StatementNode::PrintFormat { args, .. }
                    | StatementNode::PrintlnFormat { args, .. }
                    | StatementNode::Input { args, .. } => {
                        for expression in args {
                            rewrite_expression_in_place(expression, names, path, &scope.borrow())?;
                        }
                    },
                    StatementNode::If { condition, body, else_if_blocks, else_block } => {
                        rewrite_expression_in_place(condition, names, path, &scope.borrow())?;
                        if let Some(body) = else_block {
                            enqueue(
                                body,
                                &Rc::new(RefCell::new(scope.borrow().clone())),
                                &mut work,
                            );
                        }
                        if let Some(blocks) = else_if_blocks {
                            for (condition, body) in blocks.iter_mut().rev() {
                                enqueue(
                                    body,
                                    &Rc::new(RefCell::new(scope.borrow().clone())),
                                    &mut work,
                                );
                                work.push(Work::Expression(condition, scope.clone()));
                            }
                        }
                        enqueue(body, &Rc::new(RefCell::new(scope.borrow().clone())), &mut work);
                    },
                    StatementNode::While { condition, body } => {
                        rewrite_expression_in_place(condition, names, path, &scope.borrow())?;
                        enqueue(body, &Rc::new(RefCell::new(scope.borrow().clone())), &mut work);
                    },
                    StatementNode::For { initialization, condition, increment, body } => {
                        let scope = Rc::new(RefCell::new(scope.borrow().clone()));
                        work.push(Work::ForTail(condition, increment, body, scope.clone()));
                        work.push(Work::Node(initialization, scope));
                    },
                    StatementNode::Match { value, arms } => {
                        rewrite_expression_in_place(value, names, path, &scope.borrow())?;
                        for arm in arms.iter_mut().rev() {
                            work.push(Work::MatchArm(arm, scope.clone()));
                        }
                    },
                    StatementNode::Assign { variable, value } => {
                        if !scope.borrow().contains(variable) {
                            if let Some(symbol) = resolve_name(variable, names, path)? {
                                *variable = symbol.lowered;
                            }
                        }
                        rewrite_expression_in_place(value, names, path, &scope.borrow())?;
                    },
                    StatementNode::AsmBlock { inputs, outputs, .. } => {
                        for (_, expression) in inputs.iter_mut().chain(outputs) {
                            rewrite_expression_in_place(expression, names, path, &scope.borrow())?;
                        }
                    },
                    StatementNode::Return(Some(expression))
                    | StatementNode::Expression(expression) => {
                        rewrite_expression_in_place(expression, names, path, &scope.borrow())?;
                    },
                    _ => {},
                },
                _ => {},
            },
        }
    }
    *locals = root.borrow().clone();
    Ok(nodes)
}

fn rewrite_statement(
    statement: StatementNode,
    names: &NameContext,
    path: &Path,
    locals: &mut HashSet<String>,
) -> Result<StatementNode, WaveError> {
    let node = rewrite_block(vec![ASTNode::Statement(statement)], names, path, locals)?
        .pop()
        .expect("one statement");
    let ASTNode::Statement(statement) = node else { unreachable!() };
    Ok(statement)
}

fn rewrite_match_pattern(
    pattern: &mut MatchPattern,
    names: &NameContext,
    path: &Path,
    locals: &HashSet<String>,
) -> Result<(), WaveError> {
    match pattern {
        MatchPattern::Located { value, span } => rewrite_match_pattern(value, names, path, locals)
            .map_err(|e| e.with_span(Some(span)))?,
        MatchPattern::Ident(name) => {
            if !locals.contains(name) {
                if let Some(symbol) = resolve_name(name, names, path)? {
                    *name = symbol.lowered;
                }
            }
        },
        MatchPattern::Variant { variant_type, case_name, payloads } => {
            let qualified = format!("{}::{}", variant_type, case_name);
            let symbol = resolve_name(&qualified, names, path)?.ok_or_else(|| {
                module_error(
                    path,
                    "Unknown variant case",
                    format!("variant case '{}' is not declared", qualified),
                    "check the variant type and case name",
                )
            })?;
            if symbol.kind != SymbolKind::VariantConstructor {
                return Err(module_error(
                    path,
                    "Invalid variant pattern",
                    format!("'{}' is not a variant case", qualified),
                    "use a qualified case declared by a variant",
                ));
            }
            let (owner, case) = symbol.lowered.rsplit_once("::").unwrap();
            *variant_type = owner.to_string();
            *case_name = case.to_string();
            for payload in payloads {
                rewrite_match_pattern(payload, names, path, locals)?;
            }
        },
        MatchPattern::Int(_) | MatchPattern::Binding(_) | MatchPattern::Wildcard => {},
    }
    Ok(())
}

fn collect_pattern_bindings(pattern: &MatchPattern, locals: &mut HashSet<String>) {
    match pattern {
        MatchPattern::Located { value, .. } => collect_pattern_bindings(value, locals),
        MatchPattern::Binding(name) => {
            locals.insert(name.clone());
        },
        MatchPattern::Variant { payloads, .. } => {
            for payload in payloads {
                collect_pattern_bindings(payload, locals);
            }
        },
        MatchPattern::Int(_) | MatchPattern::Ident(_) | MatchPattern::Wildcard => {},
    }
}

fn rewrite_expression(
    mut expression: Expression,
    names: &NameContext,
    path: &Path,
    locals: &HashSet<String>,
) -> Result<Expression, WaveError> {
    rewrite_expression_in_place(&mut expression, names, path, locals)?;
    Ok(expression)
}

// Keep expression traversal on a heap worklist. Reconstructing owned variants
// in recursive debug frames exhausted the 1 MiB native Windows process stack
// while resolving ordinary expressions imported from std::task.
fn rewrite_expression_in_place(
    expression: &mut Expression,
    names: &NameContext,
    path: &Path,
    locals: &HashSet<String>,
) -> Result<(), WaveError> {
    enum Work<'a> {
        Expression(&'a mut Expression),
        Types(&'a mut [WaveType]),
        Failure(Box<WaveError>),
    }
    let mut pending = vec![(Work::Expression(expression), None)];
    while let Some((work, enclosing_span)) = pending.pop() {
        let result = (|| -> Result<(), WaveError> {
            let expression = match work {
                Work::Types(types) => {
                    for ty in types {
                        *ty = rewrite_type(std::mem::replace(ty, WaveType::Void), names, path)?;
                    }
                    return Ok(());
                },
                Work::Failure(error) => return Err(*error),
                Work::Expression(expression) => expression,
            };
            if let Expression::FunctionCall { name, type_args, args } = expression {
                if type_args.is_empty() && args.is_empty() {
                    if let Some(symbol) = resolve_name(name, names, path)? {
                        if symbol.kind == SymbolKind::Struct {
                            *expression = Expression::StructLiteral {
                                name: symbol.lowered,
                                fields: Vec::new(),
                            };
                            return Ok(());
                        }
                    }
                }
            }
            match expression {
                Expression::Located { value, span } => {
                    // Recursive callers previously applied the outermost span
                    // last. Retain that diagnostic location without recursion.
                    pending.push((Work::Expression(value), enclosing_span.or(Some(&*span))));
                },
                Expression::StructLiteral { name, fields } => {
                    *name = rewrite_type_name(name, names, path)?;
                    for (_, value) in fields.iter_mut().rev() {
                        pending.push((Work::Expression(value), enclosing_span));
                    }
                },
                Expression::FunctionCall { name, type_args, args } => {
                    let symbol = resolve_name(name, names, path)?;
                    for ty in type_args.iter_mut() {
                        *ty = rewrite_type(std::mem::replace(ty, WaveType::Void), names, path)?;
                    }
                    match symbol {
                        Some(symbol)
                            if symbol.kind == SymbolKind::Struct && type_args.is_empty() =>
                        {
                            pending.push((Work::Failure(Box::new(module_error(
                                path,
                                "Invalid struct constructor",
                                format!("struct '{}' must be initialized with named fields", name),
                                "use `Type { field: value }`; `Type()` is only valid for empty structs",
                            ))), enclosing_span));
                        },
                        Some(symbol)
                            if matches!(
                                symbol.kind,
                                SymbolKind::Function | SymbolKind::VariantConstructor
                            ) =>
                        {
                            *name = symbol.lowered;
                        },
                        Some(_) => {
                            pending.push((
                                Work::Failure(Box::new(module_error(
                                    path,
                                    "Symbol is not callable",
                                    format!("symbol '{}' cannot be called", name),
                                    "call a function or construct an empty struct",
                                ))),
                                enclosing_span,
                            ));
                        },
                        None => {},
                    }
                    // Arguments still report errors before a non-callable
                    // symbol/invalid-constructor error, in source order.
                    for arg in args.iter_mut().rev() {
                        pending.push((Work::Expression(arg), enclosing_span));
                    }
                },
                Expression::MethodCall { object, type_args, args, .. } => {
                    for arg in args.iter_mut().rev() {
                        pending.push((Work::Expression(arg), enclosing_span));
                    }
                    pending.push((Work::Types(type_args), enclosing_span));
                    pending.push((Work::Expression(object), enclosing_span));
                },
                Expression::Variable(name) => {
                    if !locals.contains(name) {
                        if let Some(symbol) = resolve_name(name, names, path)? {
                            *name = symbol.lowered;
                        }
                    }
                },
                Expression::Deref(inner)
                | Expression::AddressOf(inner)
                | Expression::Await(inner)
                | Expression::Grouped(inner) => {
                    pending.push((Work::Expression(inner), enclosing_span))
                },
                Expression::BinaryExpression { left, right, .. } => {
                    pending.push((Work::Expression(right), enclosing_span));
                    pending.push((Work::Expression(left), enclosing_span));
                },
                Expression::IndexAccess { target, index } => {
                    pending.push((Work::Expression(index), enclosing_span));
                    pending.push((Work::Expression(target), enclosing_span));
                },
                Expression::ArrayLiteral(values) => {
                    for value in values.iter_mut().rev() {
                        pending.push((Work::Expression(value), enclosing_span));
                    }
                },
                Expression::AssignOperation { target, value, .. }
                | Expression::Assignment { target, value } => {
                    pending.push((Work::Expression(value), enclosing_span));
                    pending.push((Work::Expression(target), enclosing_span));
                },
                Expression::AsmBlock { inputs, outputs, .. } => {
                    for (_, value) in inputs.iter_mut().chain(outputs).rev() {
                        pending.push((Work::Expression(value), enclosing_span));
                    }
                },
                Expression::FieldAccess { object, .. } => {
                    pending.push((Work::Expression(object), enclosing_span));
                },
                Expression::Unary { expr, .. } => {
                    pending.push((Work::Expression(expr), enclosing_span))
                },
                Expression::Cast { expr, target_type } => {
                    pending.push((Work::Types(std::slice::from_mut(target_type)), enclosing_span));
                    pending.push((Work::Expression(expr), enclosing_span));
                },
                Expression::IncDec { target, .. } => {
                    pending.push((Work::Expression(target), enclosing_span))
                },
                Expression::Null | Expression::Literal(_) => {},
            }
            Ok(())
        })();
        result.map_err(|e| e.with_span(enclosing_span))?;
    }
    Ok(())
}

//! Import graph construction, module visibility, and namespace lowering.
//!
//! Wave's backend still consumes one concrete AST. This pass preserves module
//! boundaries while resolving imports, then gives every imported declaration a
//! collision-free internal name before the existing semantic and LLVM phases.

use ::error::{WaveError, WaveErrorKind};
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
}

#[derive(Default)]
struct NameContext {
    own: HashMap<String, ModuleSymbol>,
    selected: HashMap<String, ModuleSymbol>,
    namespaces: HashMap<String, ModuleInterface>,
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

/// Removes the internal module hash prefix from names shown to Wave users.
pub fn demangle_module_names(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut rest = text;
    const PREFIX: &str = "__wave_";
    while let Some(index) = rest.find(PREFIX) {
        out.push_str(&rest[..index]);
        let candidate = &rest[index + PREFIX.len()..];
        if candidate.len() >= 17
            && candidate.as_bytes()[..16]
                .iter()
                .all(|byte| byte.is_ascii_hexdigit())
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
        if let Some(interface) = self.interfaces.get(&key) {
            return Ok(interface.clone());
        }
        if let Some(cycle_start) = self.visiting.iter().position(|path| path == &key) {
            let mut chain = self.visiting[cycle_start..]
                .iter()
                .map(|path| path.display().to_string())
                .collect::<Vec<_>>();
            chain.push(key.display().to_string());
            return Err(module_error(
                &key,
                "Import cycle",
                format!("import cycle detected: {}", chain.join(" -> ")),
                "remove one dependency edge or move shared declarations into a third module",
            ));
        }

        self.visiting.push(key.clone());
        let mut interface = collect_symbols(&key, &ast, is_entry)?;
        let mut names = NameContext {
            own: interface.symbols.clone(),
            ..NameContext::default()
        };

        for node in &ast {
            let ASTNode::Statement(StatementNode::Import(import)) = node.unspanned() else {
                continue;
            };

            let base_dir = key.parent().unwrap_or(Path::new("."));
            let mut loaded = HashSet::new();
            let unit =
                local_import_unit_with_config(&import.path, &mut loaded, base_dir, self.config)?;
            let child_key = canonical_key(&unit.abs_path);
            let child_origin = if let Some(index) = self.source_indices.get(&child_key) {
                *index
            } else {
                let index = self.sources.len();
                self.sources.push(ModuleSource {
                    path: unit.abs_path.clone(),
                    source: unit.source,
                });
                self.source_indices.insert(child_key.clone(), index);
                index
            };
            let child = self.resolve_module(child_key, unit.ast, child_origin, false)?;
            bind_import(&key, import, child.clone(), &mut names)?;
            if import.visibility == Visibility::Public {
                for selected in &import.selections {
                    let symbol = child
                        .symbols
                        .get(selected)
                        .expect("bind_import validated public selections")
                        .clone();
                    if interface.symbols.insert(selected.clone(), symbol).is_some() {
                        return Err(module_error(
                            &key,
                            "Duplicate public symbol",
                            format!("re-exported symbol '{}' conflicts in this module", selected),
                            "remove one re-export or rename the local declaration",
                        ));
                    }
                    let prefix = format!("{}::", selected);
                    for (qualified, constructor) in &child.symbols {
                        if qualified.starts_with(&prefix)
                            && constructor.kind == SymbolKind::VariantConstructor
                        {
                            interface
                                .symbols
                                .insert(qualified.clone(), constructor.clone());
                        }
                    }
                }
            }
        }

        let mut lowered = Vec::new();
        for node in ast {
            if matches!(
                node.unspanned(),
                ASTNode::Statement(StatementNode::Import(_))
            ) {
                continue;
            }
            lowered.push(rewrite_top_level(node, &names, &key, is_entry)?);
        }

        self.visiting.pop();
        self.interfaces.insert(key, interface.clone());
        self.origins
            .extend(std::iter::repeat_n(origin, lowered.len()));
        self.ast.extend(lowered);
        Ok(interface)
    }
}

fn canonical_key(path: &Path) -> PathBuf {
    path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
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
        ModuleSymbol {
            lowered: internal_name(path, source_name, is_entry),
            visibility,
            kind,
        },
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
            }
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
            }
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
            }
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
            }
            _ => {}
        }
    }
    Ok(ModuleInterface { symbols })
}

fn default_namespace(path: &str) -> Option<String> {
    if path.starts_with("./") {
        return Path::new(path)
            .file_stem()
            .and_then(|name| name.to_str())
            .map(str::to_string);
    }
    Some(path.to_string())
}

fn bind_import(
    current_path: &Path,
    import: &ImportNode,
    interface: ModuleInterface,
    names: &mut NameContext,
) -> Result<(), WaveError> {
    if !import.selections.is_empty() {
        for selected in &import.selections {
            let Some(symbol) = interface.symbols.get(selected) else {
                return Err(module_error(
                    current_path,
                    "Unknown imported symbol",
                    format!("module '{}' has no symbol '{}'", import.path, selected),
                    "check the declaration name and the selected import list",
                ));
            };
            if symbol.visibility != Visibility::Public {
                return Err(module_error(
                    current_path,
                    "Private imported symbol",
                    format!(
                        "symbol '{}' is private in module '{}'",
                        selected, import.path
                    ),
                    "mark the declaration `pub` or remove it from the import list",
                ));
            }
            if names.own.contains_key(selected) {
                return Err(module_error(
                    current_path,
                    "Ambiguous imported symbol",
                    format!("imported symbol '{}' conflicts in this module", selected),
                    "rename the local declaration or use a qualified module import",
                ));
            }
            if let Some(existing) = names.selected.get(selected) {
                if existing == symbol {
                    continue;
                }
                return Err(module_error(
                    current_path,
                    "Ambiguous imported symbol",
                    format!("imported symbol '{}' conflicts in this module", selected),
                    "use a qualified module import to disambiguate the declarations",
                ));
            }
            names.selected.insert(selected.clone(), symbol.clone());
            let prefix = format!("{}::", selected);
            for (qualified, constructor) in &interface.symbols {
                if qualified.starts_with(&prefix)
                    && constructor.kind == SymbolKind::VariantConstructor
                {
                    names
                        .selected
                        .insert(qualified.clone(), constructor.clone());
                }
            }
        }
        return Ok(());
    }

    let namespace = import
        .alias
        .clone()
        .or_else(|| default_namespace(&import.path))
        .ok_or_else(|| {
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
        return Err(module_error(
            current_path,
            "Duplicate import namespace",
            format!("namespace '{}' is bound to multiple modules", namespace),
            "use a distinct `as` alias for one import",
        ));
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
            if name
                .strip_prefix(namespace)
                .is_some_and(|rest| rest.starts_with("::"))
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
            ));
        };
        let symbol_name = &name[namespace.len() + 2..];
        let Some(symbol) = interface.symbols.get(symbol_name) else {
            return Err(module_error(
                path,
                "Unknown imported symbol",
                format!("module '{}' has no symbol '{}'", namespace, symbol_name),
                "check the public declaration name",
            ));
        };
        if symbol.visibility != Visibility::Public {
            return Err(module_error(
                path,
                "Private imported symbol",
                format!(
                    "symbol '{}' is private in module '{}'",
                    symbol_name, namespace
                ),
                "only `pub` declarations are accessible outside their module",
            ));
        }
        return Ok(Some(symbol.clone()));
    }

    Ok(names
        .own
        .get(name)
        .or_else(|| names.selected.get(name))
        .cloned())
}

fn rewrite_type(ty: WaveType, names: &NameContext, path: &Path) -> Result<WaveType, WaveError> {
    match ty {
        WaveType::Pointer(inner) => Ok(WaveType::Pointer(Box::new(rewrite_type(
            *inner, names, path,
        )?))),
        WaveType::Array(inner, size) => Ok(WaveType::Array(
            Box::new(rewrite_type(*inner, names, path)?),
            size,
        )),
        WaveType::Struct(name) => Ok(WaveType::Struct(rewrite_type_name(&name, names, path)?)),
        WaveType::Variant(name) => Ok(WaveType::Variant(rewrite_type_name(&name, names, path)?)),
        other => Ok(other),
    }
}

fn rewrite_type_name(name: &str, names: &NameContext, path: &Path) -> Result<String, WaveError> {
    if let Some((base, arguments)) = split_type_application(name) {
        let rewritten_base = rewrite_type_name(base, names, path)?;
        let rewritten_arguments = split_type_arguments(arguments)
            .into_iter()
            .map(|argument| rewrite_type_name(argument.trim(), names, path))
            .collect::<Result<Vec<_>, _>>()?;
        return Ok(format!(
            "{}<{}>",
            rewritten_base,
            rewritten_arguments.join(",")
        ));
    }

    match resolve_name(name, names, path)? {
        Some(symbol) if matches!(symbol.kind, SymbolKind::Struct | SymbolKind::Type) => {
            Ok(symbol.lowered)
        }
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
            }
            _ => {}
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
        }
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
        }
        ASTNode::ExternFunction(mut function) => {
            function.name = names.own[&function.name].lowered.clone();
            function.params = function
                .params
                .into_iter()
                .map(|(name, ty)| Ok((name, rewrite_type(ty, names, path)?)))
                .collect::<Result<_, WaveError>>()?;
            function.return_type = rewrite_type(function.return_type, names, path)?;
            Ok(ASTNode::ExternFunction(function))
        }
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
        }
        ASTNode::TypeAlias(mut alias) => {
            alias.name = names.own[&alias.name].lowered.clone();
            alias.target = rewrite_type(alias.target, names, path)?;
            Ok(ASTNode::TypeAlias(alias))
        }
        ASTNode::Enum(mut enumeration) => {
            enumeration.name = names.own[&enumeration.name].lowered.clone();
            enumeration.repr_type = rewrite_type(enumeration.repr_type, names, path)?;
            for variant in &mut enumeration.variants {
                variant.name = names.own[&variant.name].lowered.clone();
            }
            Ok(ASTNode::Enum(enumeration))
        }
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
        }
        ASTNode::Variable(mut variable) => {
            variable.name = names.own[&variable.name].lowered.clone();
            variable.type_name = rewrite_type(variable.type_name, names, path)?;
            if let Some(value) = variable.initial_value.take() {
                variable.initial_value =
                    Some(rewrite_expression(value, names, path, &HashSet::new())?);
            }
            Ok(ASTNode::Variable(variable))
        }
        ASTNode::ProtoImpl(mut implementation) => {
            implementation.target = resolve_name(&implementation.target, names, path)?
                .map_or(implementation.target, |symbol| symbol.lowered);
            for method in &mut implementation.methods {
                rewrite_function(method, names, path)?;
            }
            Ok(ASTNode::ProtoImpl(implementation))
        }
        ASTNode::Statement(statement) => Ok(ASTNode::Statement(rewrite_statement(
            statement,
            names,
            path,
            &mut HashSet::new(),
        )?)),
        ASTNode::Expression(expression) => Ok(ASTNode::Expression(rewrite_expression(
            expression,
            names,
            path,
            &HashSet::new(),
        )?)),
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
    function.return_type = function
        .return_type
        .take()
        .map(|ty| rewrite_type(ty, names, path))
        .transpose()?;
    let mut locals = function
        .parameters
        .iter()
        .map(|parameter| parameter.name.clone())
        .collect::<HashSet<_>>();
    function.body = rewrite_block(std::mem::take(&mut function.body), names, path, &mut locals)?;
    Ok(())
}

fn rewrite_block(
    nodes: Vec<ASTNode>,
    names: &NameContext,
    path: &Path,
    locals: &mut HashSet<String>,
) -> Result<Vec<ASTNode>, WaveError> {
    let mut out = Vec::with_capacity(nodes.len());
    for node in nodes {
        match node {
            ASTNode::Located { value, span } => {
                let rewritten = rewrite_block(vec![*value], names, path, locals)?;
                out.extend(
                    rewritten
                        .into_iter()
                        .map(|node| node.with_span(Some(span.clone()))),
                );
            }
            ASTNode::Variable(mut variable) => {
                variable.type_name = rewrite_type(variable.type_name, names, path)?;
                if let Some(value) = variable.initial_value.take() {
                    variable.initial_value = Some(rewrite_expression(value, names, path, locals)?);
                }
                locals.insert(variable.name.clone());
                out.push(ASTNode::Variable(variable));
            }
            ASTNode::Statement(statement) => out.push(ASTNode::Statement(rewrite_statement(
                statement, names, path, locals,
            )?)),
            ASTNode::Expression(expression) => out.push(ASTNode::Expression(rewrite_expression(
                expression, names, path, locals,
            )?)),
            other => out.push(other),
        }
    }
    Ok(out)
}

fn rewrite_statement(
    statement: StatementNode,
    names: &NameContext,
    path: &Path,
    locals: &mut HashSet<String>,
) -> Result<StatementNode, WaveError> {
    Ok(match statement {
        StatementNode::PrintFormat { format, args } => StatementNode::PrintFormat {
            format,
            args: rewrite_expressions(args, names, path, locals)?,
        },
        StatementNode::PrintlnFormat { format, args } => StatementNode::PrintlnFormat {
            format,
            args: rewrite_expressions(args, names, path, locals)?,
        },
        StatementNode::Input { format, args } => StatementNode::Input {
            format,
            args: rewrite_expressions(args, names, path, locals)?,
        },
        StatementNode::If {
            condition,
            body,
            else_if_blocks,
            else_block,
        } => {
            let condition = rewrite_expression(condition, names, path, locals)?;
            let mut body_scope = locals.clone();
            let body = rewrite_block(body, names, path, &mut body_scope)?;
            let else_if_blocks = else_if_blocks
                .map(|blocks| {
                    blocks
                        .into_iter()
                        .map(|(condition, body)| {
                            let condition = rewrite_expression(condition, names, path, locals)?;
                            let mut scope = locals.clone();
                            Ok((condition, rewrite_block(body, names, path, &mut scope)?))
                        })
                        .collect::<Result<Vec<_>, WaveError>>()
                        .map(Box::new)
                })
                .transpose()?;
            let else_block = else_block
                .map(|body| {
                    let mut scope = locals.clone();
                    rewrite_block(*body, names, path, &mut scope).map(Box::new)
                })
                .transpose()?;
            StatementNode::If {
                condition,
                body,
                else_if_blocks,
                else_block,
            }
        }
        StatementNode::For {
            initialization,
            condition,
            increment,
            body,
        } => {
            let mut scope = locals.clone();
            let initialization = rewrite_block(vec![*initialization], names, path, &mut scope)?
                .into_iter()
                .next()
                .expect("for initializer is preserved");
            let condition = rewrite_expression(condition, names, path, &scope)?;
            let increment = rewrite_expression(increment, names, path, &scope)?;
            let body = rewrite_block(body, names, path, &mut scope)?;
            StatementNode::For {
                initialization: Box::new(initialization),
                condition,
                increment,
                body,
            }
        }
        StatementNode::While { condition, body } => {
            let condition = rewrite_expression(condition, names, path, locals)?;
            let mut scope = locals.clone();
            let body = rewrite_block(body, names, path, &mut scope)?;
            StatementNode::While { condition, body }
        }
        StatementNode::Match { value, arms } => StatementNode::Match {
            value: rewrite_expression(value, names, path, locals)?,
            arms: arms
                .into_iter()
                .map(|mut arm| {
                    rewrite_match_pattern(&mut arm.pattern, names, path, locals)?;
                    let mut scope = locals.clone();
                    collect_pattern_bindings(&arm.pattern, &mut scope);
                    arm.body = rewrite_block(arm.body, names, path, &mut scope)?;
                    Ok(arm)
                })
                .collect::<Result<_, WaveError>>()?,
        },
        StatementNode::Assign { variable, value } => StatementNode::Assign {
            variable: if locals.contains(&variable) {
                variable
            } else {
                resolve_name(&variable, names, path)?.map_or(variable, |symbol| symbol.lowered)
            },
            value: rewrite_expression(value, names, path, locals)?,
        },
        StatementNode::AsmBlock {
            instructions,
            inputs,
            outputs,
            clobbers,
        } => StatementNode::AsmBlock {
            instructions,
            inputs: inputs
                .into_iter()
                .map(|(constraint, expression)| {
                    Ok((
                        constraint,
                        rewrite_expression(expression, names, path, locals)?,
                    ))
                })
                .collect::<Result<_, WaveError>>()?,
            outputs: outputs
                .into_iter()
                .map(|(constraint, expression)| {
                    Ok((
                        constraint,
                        rewrite_expression(expression, names, path, locals)?,
                    ))
                })
                .collect::<Result<_, WaveError>>()?,
            clobbers,
        },
        StatementNode::Return(value) => StatementNode::Return(
            value
                .map(|expression| rewrite_expression(expression, names, path, locals))
                .transpose()?,
        ),
        StatementNode::Expression(expression) => {
            StatementNode::Expression(rewrite_expression(expression, names, path, locals)?)
        }
        other => other,
    })
}

fn rewrite_expressions(
    expressions: Vec<Expression>,
    names: &NameContext,
    path: &Path,
    locals: &HashSet<String>,
) -> Result<Vec<Expression>, WaveError> {
    expressions
        .into_iter()
        .map(|expression| rewrite_expression(expression, names, path, locals))
        .collect()
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
        }
        MatchPattern::Variant {
            variant_type,
            case_name,
            payloads,
        } => {
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
        }
        MatchPattern::Int(_) | MatchPattern::Binding(_) | MatchPattern::Wildcard => {}
    }
    Ok(())
}

fn collect_pattern_bindings(pattern: &MatchPattern, locals: &mut HashSet<String>) {
    match pattern {
        MatchPattern::Located { value, .. } => collect_pattern_bindings(value, locals),
        MatchPattern::Binding(name) => {
            locals.insert(name.clone());
        }
        MatchPattern::Variant { payloads, .. } => {
            for payload in payloads {
                collect_pattern_bindings(payload, locals);
            }
        }
        MatchPattern::Int(_) | MatchPattern::Ident(_) | MatchPattern::Wildcard => {}
    }
}

fn rewrite_expression(
    expression: Expression,
    names: &NameContext,
    path: &Path,
    locals: &HashSet<String>,
) -> Result<Expression, WaveError> {
    Ok(match expression {
        Expression::Located { value, span } => rewrite_expression(*value, names, path, locals)
            .map_err(|e| e.with_span(Some(&span)))?
            .with_span(Some(span)),
        Expression::StructLiteral { name, fields } => Expression::StructLiteral {
            name: rewrite_type_name(&name, names, path)?,
            fields: fields
                .into_iter()
                .map(|(name, value)| Ok((name, rewrite_expression(value, names, path, locals)?)))
                .collect::<Result<_, WaveError>>()?,
        },
        Expression::FunctionCall {
            name,
            type_args,
            args,
        } => {
            let symbol = resolve_name(&name, names, path)?;
            let type_args = type_args
                .into_iter()
                .map(|ty| rewrite_type(ty, names, path))
                .collect::<Result<Vec<_>, _>>()?;
            let args = rewrite_expressions(args, names, path, locals)?;
            match symbol {
                Some(symbol) if symbol.kind == SymbolKind::Struct && type_args.is_empty() => {
                    if !args.is_empty() {
                        return Err(module_error(
                            path,
                            "Invalid struct constructor",
                            format!("struct '{}' must be initialized with named fields", name),
                            "use `Type { field: value }`; `Type()` is only valid for empty structs",
                        ));
                    }
                    Expression::StructLiteral {
                        name: symbol.lowered,
                        fields: Vec::new(),
                    }
                }
                Some(symbol)
                    if matches!(
                        symbol.kind,
                        SymbolKind::Function | SymbolKind::VariantConstructor
                    ) =>
                {
                    Expression::FunctionCall {
                        name: symbol.lowered,
                        type_args,
                        args,
                    }
                }
                Some(_) => {
                    return Err(module_error(
                        path,
                        "Symbol is not callable",
                        format!("symbol '{}' cannot be called", name),
                        "call a function or construct an empty struct",
                    ))
                }
                None => Expression::FunctionCall {
                    name,
                    type_args,
                    args,
                },
            }
        }
        Expression::MethodCall { object, name, args } => Expression::MethodCall {
            object: Box::new(rewrite_expression(*object, names, path, locals)?),
            name,
            args: rewrite_expressions(args, names, path, locals)?,
        },
        Expression::Variable(name) => Expression::Variable(if locals.contains(&name) {
            name
        } else {
            resolve_name(&name, names, path)?.map_or(name, |symbol| symbol.lowered)
        }),
        Expression::Deref(inner) => {
            Expression::Deref(Box::new(rewrite_expression(*inner, names, path, locals)?))
        }
        Expression::AddressOf(inner) => {
            Expression::AddressOf(Box::new(rewrite_expression(*inner, names, path, locals)?))
        }
        Expression::BinaryExpression {
            left,
            operator,
            right,
        } => Expression::BinaryExpression {
            left: Box::new(rewrite_expression(*left, names, path, locals)?),
            operator,
            right: Box::new(rewrite_expression(*right, names, path, locals)?),
        },
        Expression::IndexAccess { target, index } => Expression::IndexAccess {
            target: Box::new(rewrite_expression(*target, names, path, locals)?),
            index: Box::new(rewrite_expression(*index, names, path, locals)?),
        },
        Expression::ArrayLiteral(values) => {
            Expression::ArrayLiteral(rewrite_expressions(values, names, path, locals)?)
        }
        Expression::Grouped(inner) => {
            Expression::Grouped(Box::new(rewrite_expression(*inner, names, path, locals)?))
        }
        Expression::AssignOperation {
            target,
            operator,
            value,
        } => Expression::AssignOperation {
            target: Box::new(rewrite_expression(*target, names, path, locals)?),
            operator,
            value: Box::new(rewrite_expression(*value, names, path, locals)?),
        },
        Expression::Assignment { target, value } => Expression::Assignment {
            target: Box::new(rewrite_expression(*target, names, path, locals)?),
            value: Box::new(rewrite_expression(*value, names, path, locals)?),
        },
        Expression::AsmBlock {
            instructions,
            inputs,
            outputs,
            clobbers,
        } => Expression::AsmBlock {
            instructions,
            inputs: inputs
                .into_iter()
                .map(|(constraint, expression)| {
                    Ok((
                        constraint,
                        rewrite_expression(expression, names, path, locals)?,
                    ))
                })
                .collect::<Result<_, WaveError>>()?,
            outputs: outputs
                .into_iter()
                .map(|(constraint, expression)| {
                    Ok((
                        constraint,
                        rewrite_expression(expression, names, path, locals)?,
                    ))
                })
                .collect::<Result<_, WaveError>>()?,
            clobbers,
        },
        Expression::FieldAccess { object, field } => Expression::FieldAccess {
            object: Box::new(rewrite_expression(*object, names, path, locals)?),
            field,
        },
        Expression::Unary { operator, expr } => Expression::Unary {
            operator,
            expr: Box::new(rewrite_expression(*expr, names, path, locals)?),
        },
        Expression::Cast { expr, target_type } => Expression::Cast {
            expr: Box::new(rewrite_expression(*expr, names, path, locals)?),
            target_type: rewrite_type(target_type, names, path)?,
        },
        Expression::IncDec { kind, target } => Expression::IncDec {
            kind,
            target: Box::new(rewrite_expression(*target, names, path, locals)?),
        },
        other => other,
    })
}

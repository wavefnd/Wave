use crate::ast::{FunctionSignature, WaveType};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct StdlibModule {
    pub name: String,
    pub functions: HashMap<String, FunctionSignature>,
    pub types: HashMap<String, WaveType>,
    pub constants: HashMap<String, (WaveType, String)>,
}

#[derive(Debug, Default)]
pub struct StdlibRegistry {
    modules: HashMap<String, StdlibModule>,
    used: HashSet<String>,
    strict: bool,
}

impl StdlibRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn mark_used(&mut self, module: &str) {
        self.used.insert(module.to_string());
    }

    pub fn used_modules(&self) -> impl Iterator<Item = &String> {
        self.used.iter()
    }

    pub fn set_strict(&mut self, strict: bool) {
        self.strict = strict;
    }

    pub fn register_module(&mut self, m: StdlibModule) {
        self.modules.insert(m.name.clone(), m);
    }

    pub fn has_module(&self, name: &str) -> bool {
        self.modules.contains_key(name)
    }

    pub fn module(&self, name: &str) -> Option<&StdlibModule> {
        self.modules.get(name)
    }

    pub fn validate_import(&self, name: &str) -> bool {
        if !self.strict {
            return true;
        }
        self.modules.contains_key(name)
    }
}

// codegen/asm/plan.rs
use parser::ast::Expression;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct AsmPlan<'a> {
    pub asm_code: String,

    // outputs are first in constraints
    pub outputs: Vec<AsmOut<'a>>,

    // inputs are call arguments (constraints after outputs)
    pub inputs: Vec<AsmIn<'a>>,

    // clobbers go last
    pub clobbers: Vec<String>,

    pub has_side_effects: bool,
}

#[derive(Debug, Clone)]
pub struct AsmOut<'a> {
    pub reg_raw: String,      // user wrote (e.g. "rax", "%rax", "RAX", "r")
    pub reg_norm: String,     // normalized token (e.g. "rax", "r")
    pub phys_group: Option<String>, // Some("rax") for real regs (al/ax/eax/rax -> rax), None for constraint classes (r/rm/m/..)
    pub target: &'a Expression,
}

#[derive(Debug, Clone)]
pub struct AsmIn<'a> {
    pub constraint: String,   // "{rax}" or "r" or "0" (tied)
    pub phys_group: Option<String>, // Some("rax") if it binds a concrete reg token, None if it is a class constraint
    pub value: &'a Expression,
}

#[derive(Debug, Clone, Copy)]
pub enum AsmSafetyMode {
    ConservativeKernel,
}

#[derive(Debug, Clone)]
struct RegToken {
    raw_norm: String,             // normalized token (no %, no braces, lowercase)
    phys_group: Option<String>,   // physical register group for real regs
}

impl RegToken {
    fn is_real_reg(&self) -> bool {
        self.phys_group.is_some()
    }
}

/// Normalize user reg/constraint token:
/// - trims spaces
/// - strips leading '%'
/// - strips surrounding '{...}' if user wrote them
/// - lowercase
fn normalize_token(s: &str) -> String {
    let s = s.trim();
    let s = s.trim_start_matches('%');

    let s = if let Some(inner) = s.strip_prefix('{').and_then(|x| x.strip_suffix('}')) {
        inner
    } else {
        s
    };

    s.trim().to_ascii_lowercase()
}

/// If token is a real x86_64 GPR/subreg, return its *physical group*:
/// - al/ax/eax/rax -> "rax"
/// - dl/dx/edx/rdx -> "rdx"
/// - r8b/r8w/r8d/r8 -> "r8"
fn reg_phys_group(token: &str) -> Option<&'static str> {
    match token {
        // rax group
        "al" | "ah" | "ax" | "eax" | "rax" => Some("rax"),
        // rbx group
        "bl" | "bh" | "bx" | "ebx" | "rbx" => Some("rbx"),
        // rcx group
        "cl" | "ch" | "cx" | "ecx" | "rcx" => Some("rcx"),
        // rdx group
        "dl" | "dh" | "dx" | "edx" | "rdx" => Some("rdx"),

        // rsi group
        "sil" | "si" | "esi" | "rsi" => Some("rsi"),
        // rdi group
        "dil" | "di" | "edi" | "rdi" => Some("rdi"),

        // rbp group
        "bpl" | "bp" | "ebp" | "rbp" => Some("rbp"),
        // rsp group
        "spl" | "sp" | "esp" | "rsp" => Some("rsp"),

        // r8~r15 groups
        "r8b" | "r8w" | "r8d" | "r8" => Some("r8"),
        "r9b" | "r9w" | "r9d" | "r9" => Some("r9"),
        "r10b" | "r10w" | "r10d" | "r10" => Some("r10"),
        "r11b" | "r11w" | "r11d" | "r11" => Some("r11"),
        "r12b" | "r12w" | "r12d" | "r12" => Some("r12"),
        "r13b" | "r13w" | "r13d" | "r13" => Some("r13"),
        "r14b" | "r14w" | "r14d" | "r14" => Some("r14"),
        "r15b" | "r15w" | "r15d" | "r15" => Some("r15"),

        _ => None,
    }
}

/// Decide whether user string is:
/// - real register (rax/eax/al/r8d/...)
/// - or a constraint class (r/rm/m/i/n/g/...)
fn parse_token(raw: &str) -> RegToken {
    let raw_norm = normalize_token(raw);
    let phys_group = reg_phys_group(&raw_norm).map(|s| s.to_string());
    RegToken { raw_norm, phys_group }
}

/// For conservative kernel mode:
/// - ALWAYS clobber memory + flags-ish
/// - If ANY operand uses a *class constraint* (no concrete phys reg),
///   DO NOT auto-clobber GPRs (otherwise allocator can't satisfy "r"/"rm").
/// - If all operands are concrete regs only, you may clobber the rest GPRs safely.
fn build_default_clobbers(
    mode: AsmSafetyMode,
    inputs: &[(String, Expression)],
    outputs: &[(String, Expression)],
) -> Vec<String> {
    match mode {
        AsmSafetyMode::ConservativeKernel => {
            let mut clobbers = vec![
                "~{memory}".to_string(),
                "~{dirflag}".to_string(),
                "~{fpsr}".to_string(),
                "~{flags}".to_string(),
            ];

            // Collect concrete used physical register groups
            let mut used_phys: HashSet<String> = HashSet::new();
            let mut has_class_constraint = false;

            for (r, _) in inputs {
                let t = parse_token(r);
                if let Some(pg) = t.phys_group {
                    used_phys.insert(pg);
                } else {
                    has_class_constraint = true;
                }
            }
            for (r, _) in outputs {
                let t = parse_token(r);
                if let Some(pg) = t.phys_group {
                    used_phys.insert(pg);
                } else {
                    has_class_constraint = true;
                }
            }

            // If any class constraint exists, don't auto-clobber GPRs.
            if has_class_constraint {
                return clobbers;
            }

            // Otherwise: clobber all GPRs not explicitly used.
            const GPRS: [&str; 16] = [
                "rax","rbx","rcx","rdx","rsi","rdi","rbp","rsp",
                "r8","r9","r10","r11","r12","r13","r14","r15",
            ];

            for r in GPRS {
                if !used_phys.contains(r) {
                    clobbers.push(format!("~{{{}}}", r));
                }
            }

            clobbers
        }
    }
}

fn gcc_percent_to_llvm_dollar(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let bytes = s.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        if bytes[i] == b'%' {
            // "%%" -> literal '%'
            if i + 1 < bytes.len() && bytes[i + 1] == b'%' {
                out.push('%');
                i += 2;
                continue;
            }

            // "%123" -> "$123"
            let mut j = i + 1;
            if j < bytes.len() && bytes[j].is_ascii_digit() {
                out.push('$');
                while j < bytes.len() && bytes[j].is_ascii_digit() {
                    out.push(bytes[j] as char);
                    j += 1;
                }
                i = j;
                continue;
            }
        }

        out.push(bytes[i] as char);
        i += 1;
    }

    out
}

fn normalize_clobber_item(s: &str) -> String {
    let t = s.trim();

    if let Some(inner) = t.strip_prefix("~{").and_then(|x| x.strip_suffix('}')) {
        let n = normalize_token(inner);

        match n.as_str() {
            "memory" => return "~{memory}".to_string(),
            "cc" | "flags" | "eflags" | "rflags" => return "~{flags}".to_string(),
            "dirflag" => return "~{dirflag}".to_string(),
            "fpsr" => return "~{fpsr}".to_string(),
            _ => {}
        }

        if let Some(pg) = reg_phys_group(&n) {
            return format!("~{{{}}}", pg);
        }

        panic!("Invalid clobber token: '{}'", inner);
    }

    // "{...}" 형태
    if let Some(inner) = t.strip_prefix('{').and_then(|x| x.strip_suffix('}')) {
        let n = normalize_token(inner);

        match n.as_str() {
            "memory" => return "~{memory}".to_string(),
            "cc" | "flags" | "eflags" | "rflags" => return "~{flags}".to_string(),
            "dirflag" => return "~{dirflag}".to_string(),
            "fpsr" => return "~{fpsr}".to_string(),
            _ => {}
        }

        if let Some(pg) = reg_phys_group(&n) {
            return format!("~{{{}}}", pg);
        }

        panic!("Invalid clobber token: '{}'", inner);
    }

    // specials (plain)
    let lower = t.to_ascii_lowercase();
    match lower.as_str() {
        "memory" => return "~{memory}".to_string(),
        "cc" | "flags" | "eflags" | "rflags" => return "~{flags}".to_string(),
        "dirflag" => return "~{dirflag}".to_string(),
        "fpsr" => return "~{fpsr}".to_string(),
        _ => {}
    }

    let rt = parse_token(t);
    if let Some(pg) = rt.phys_group {
        return format!("~{{{}}}", pg);
    }

    panic!("Invalid clobber token: '{}'", t);
}


fn merge_clobbers(
    mut base: Vec<String>,
    user: &[String],
    used_phys: &HashSet<String>,
) -> Vec<String> {
    let mut seen: HashSet<String> = base.iter().cloned().collect();

    for raw in user {
        let c = normalize_clobber_item(raw);

        if let Some(inner) = c.strip_prefix("~{").and_then(|x| x.strip_suffix('}')) {
            let inner_norm = normalize_token(inner);
            if used_phys.contains(&inner_norm) {
                panic!(
                    "clobber '{}' conflicts with an input/output operand register",
                    raw
                );
            }
        }

        if seen.insert(c.clone()) {
            base.push(c);
        }
    }

    base
}

impl<'a> AsmPlan<'a> {
    pub fn build(
        instructions: &'a [String],
        inputs_raw: &'a [(String, Expression)],
        outputs_raw: &'a [(String, Expression)],
        user_clobbers_raw: &'a [String],
        mode: AsmSafetyMode,
    ) -> Self {
        let asm_code = instructions.join("\n");
        let asm_code = gcc_percent_to_llvm_dollar(&asm_code);

        // outputs
        let mut used_out_phys: HashSet<String> = HashSet::new();
        let mut out_index_by_exact_reg: HashMap<String, usize> = HashMap::new();
        let mut outputs: Vec<AsmOut<'a>> = Vec::with_capacity(outputs_raw.len());

        for (reg, target) in outputs_raw {
            let t = parse_token(reg);

            // real reg outputs: disallow duplicates by physical group
            if let Some(pg) = &t.phys_group {
                if !used_out_phys.insert(pg.clone()) {
                    panic!("Register '{}' duplicated in asm outputs (same phys group '{}')", reg, pg);
                }
                // enable tied input only when exact same token used (ex: out("rax") + in("rax"))
                out_index_by_exact_reg.insert(t.raw_norm.clone(), outputs.len());
            }
            // class constraints (r/rm/m/...) -> allow duplicates

            outputs.push(AsmOut {
                reg_raw: reg.clone(),
                reg_norm: t.raw_norm,
                phys_group: t.phys_group,
                target,
            });
        }

        // inputs
        let mut used_in_phys: HashSet<String> = HashSet::new();
        let mut inputs: Vec<AsmIn<'a>> = Vec::with_capacity(inputs_raw.len());

        for (reg, expr) in inputs_raw {
            let t = parse_token(reg);

            // real reg inputs: disallow duplicates by physical group
            if let Some(pg) = &t.phys_group {
                if !used_in_phys.insert(pg.clone()) {
                    panic!("Register '{}' duplicated in asm inputs (same phys group '{}')", reg, pg);
                }

                // tied only when exact same reg token matches a real-reg output token
                if let Some(&out_idx) = out_index_by_exact_reg.get(&t.raw_norm) {
                    inputs.push(AsmIn {
                        constraint: out_idx.to_string(), // "0", "1", ...
                        phys_group: Some(pg.clone()),
                        value: expr,
                    });
                    continue;
                }

                inputs.push(AsmIn {
                    constraint: format!("{{{}}}", t.raw_norm), // "{rax}", "{dl}", "{r8d}", ...
                    phys_group: Some(pg.clone()),
                    value: expr,
                });
                continue;
            }

            // class constraint: allow duplicates, pass through as-is
            inputs.push(AsmIn {
                constraint: t.raw_norm, // "r", "rm", "m", "i", ...
                phys_group: None,
                value: expr,
            });
        }

        let mut used_phys: HashSet<String> = HashSet::new();
        for o in &outputs {
            if let Some(pg) = &o.phys_group {
                used_phys.insert(pg.clone());
            }
        }
        for i in &inputs {
            if let Some(pg) = &i.phys_group {
                used_phys.insert(pg.clone());
            }
        }

        let default_clobbers = build_default_clobbers(mode, inputs_raw, outputs_raw);
        let clobbers = merge_clobbers(default_clobbers, user_clobbers_raw, &used_phys);

        Self {
            asm_code,
            outputs,
            inputs,
            clobbers,
            has_side_effects: true,
        }
    }

    pub fn constraints_string(&self) -> String {
        let mut parts: Vec<String> = Vec::new();

        // outputs first
        for o in &self.outputs {
            if o.phys_group.is_some() {
                // concrete register
                parts.push(format!("={{{}}}", o.reg_norm)); // "={rax}", "={dl}", ...
            } else {
                // class constraint
                parts.push(format!("={}", o.reg_norm)); // "=r", "=m", ...
            }
        }

        // inputs next
        for i in &self.inputs {
            parts.push(i.constraint.clone()); // "{rsi}" or "r" or "0" ...
        }

        // clobbers last
        for c in &self.clobbers {
            parts.push(c.clone());
        }

        parts.join(",")
    }
}

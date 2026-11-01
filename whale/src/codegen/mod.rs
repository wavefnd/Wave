// SPDX-License-Identifier: MPL-2.0
//! Experimental Wave typed HIR -> Whale IR adapter. No LLVM fallback or ABI
//! inference belongs here. Unsupported Wave constructs fail before publication.
mod expression;
mod numeric;
#[cfg(test)]
mod tests;
mod walk;

use error::SourceSpan;
use hir::TypedProgram;
use parser::ast::{ASTNode, Expression, FunctionNode, Mutability, StatementNode, WaveType};
use std::collections::HashMap;
use whale_ir::*;

#[derive(Debug)]
pub struct LowerError {
    pub message: String,
    pub span: Option<SourceSpan>,
}
type Result<T> = std::result::Result<T, LowerError>;

fn unsupported(message: impl Into<String>) -> LowerError {
    LowerError {
        message: format!("Whale backend does not yet support {}", message.into()),
        span: None,
    }
}

fn ice(message: impl Into<String>) -> LowerError {
    LowerError {
        message: format!("internal compiler error in Whale lowering: {}", message.into()),
        span: None,
    }
}

fn scalar(ty: &WaveType) -> Result<Type> {
    Ok(match ty {
        WaveType::Int(8) => Type::I8,
        WaveType::Int(16) => Type::I16,
        WaveType::Int(32) => Type::I32,
        WaveType::Int(64) => Type::I64,
        WaveType::Int(128) => Type::I128,
        WaveType::Uint(8) | WaveType::Byte | WaveType::Char => Type::U8,
        WaveType::Uint(16) => Type::U16,
        WaveType::Uint(32) => Type::U32,
        WaveType::Uint(64) => Type::U64,
        WaveType::Uint(128) => Type::U128,
        WaveType::Float(32) => Type::F32,
        WaveType::Float(64) => Type::F64,
        WaveType::Bool => Type::Bool,
        WaveType::Void => Type::Void,
        _ => return Err(unsupported(format!("type {ty:?}"))),
    })
}

pub(crate) fn lower(program: &TypedProgram) -> Result<Module> {
    lower_with_limits(program, IrLimits::default())
}

fn lower_with_limits(program: &TypedProgram, limits: IrLimits) -> Result<Module> {
    program.verify_conversions().map_err(|e| LowerError {
        message: format!("internal compiler error: invalid HIR conversions: {}", e.message),
        span: e.span,
    })?;
    let mut module = Module::new("x86_64-whale-linux", DataLayout::default_64bit_le());
    let mut functions = HashMap::new();
    let mut constants = HashMap::new();
    for node in program.syntax() {
        let result = (|| {
            let f = match node.unspanned() {
                ASTNode::Function(function) => function,
                ASTNode::TypeAlias(alias) => {
                    // Typed HIR already resolved aliases, including chains.
                    // Validate this backend's type subset without rebuilding
                    // an alias environment or emitting a runtime declaration.
                    scalar(&alias.target)?;
                    return Ok(());
                },
                ASTNode::Variable(variable) if variable.mutability == Mutability::Const => {
                    let ty = scalar(&variable.type_name)?;
                    let initializer = variable
                        .initial_value
                        .as_ref()
                        .ok_or_else(|| ice("global constant has no initializer"))?;
                    let value = program
                        .constant_value_of(initializer)
                        .ok_or_else(|| ice("global scalar constant has no verified HIR value"))?;
                    constants.insert(
                        variable.name.clone(),
                        (ty, expression::constant_value(value, &variable.type_name)?),
                    );
                    return Ok(());
                },
                ASTNode::Enum(enumeration) => {
                    let info = program
                        .enum_types()
                        .iter()
                        .find(|ty| ty.name == enumeration.name)
                        .ok_or_else(|| ice("missing frontend enum facts"))?;
                    let ty = scalar(&info.underlying_type)?;
                    for variant in &info.variants {
                        constants.insert(
                            variant.name.clone(),
                            (
                                ty.clone(),
                                expression::constant_value(
                                    &hir::ConstantValue::Int(variant.value.clone()),
                                    &info.underlying_type,
                                )?,
                            ),
                        );
                    }
                    return Ok(());
                },
                ASTNode::Variable(_) => return Err(unsupported("mutable global storage")),
                _ => return Err(unsupported("non-function top-level declarations")),
            };
            if f.is_async || f.export.is_some() {
                return Err(unsupported("async or exported functions"));
            }
            let id = FunctionId(module.declarations.len() as u32);
            let signature = FunctionSignature::whale(
                f.parameters.iter().map(|p| scalar(&p.param_type)).collect::<Result<_>>()?,
                scalar(f.return_type.as_ref().unwrap_or(&WaveType::Void))?,
            );
            functions.insert(f.name.clone(), (id, signature.clone()));
            module.declarations.push(FunctionDecl {
                id,
                name: f.name.clone(),
                signature,
                linkage: Linkage::Internal,
                link_name: None,
            });
            Ok(())
        })();
        result.map_err(|mut e: LowerError| {
            e.span = program
                .node_id(node)
                .and_then(|id| program.node_span(id))
                .cloned()
                .or_else(|| node.span().cloned());
            e
        })?;
    }
    for node in program.syntax() {
        let ASTNode::Function(f) = node.unspanned() else { continue };
        let function = Lowerer::function(program, &functions, &constants, f).map_err(|mut e| {
            if e.span.is_none() {
                e.span = f.span.clone().or_else(|| node.span().cloned());
            }
            e
        })?;
        module.functions.push(function);
    }
    verify_output(&module, limits)?;
    Ok(module)
}

// Budget exhaustion is an ordinary bounded-input diagnostic. Malformed output
// still means the adapter violated its own IR contract and remains an ICE.
fn verify_output(module: &Module, limits: IrLimits) -> Result<()> {
    verify_module_with_limits(module, limits).map_err(|error| match error {
        VerifyError::ResourceLimit(limit) => LowerError {
            message: format!("Whale verification resource limit exceeded: {limit}"),
            span: None,
        },
        other => ice(format!("Whale IR verification failed: {other:?}")),
    })
}

#[derive(Clone)]
struct Slot {
    ptr: ValueId,
    ty: Type,
}
#[derive(Clone)]
struct Value {
    id: ValueId,
    ty: Type,
}
struct Lowerer<'a> {
    program: &'a TypedProgram,
    functions: &'a HashMap<String, (FunctionId, FunctionSignature)>,
    constants: &'a HashMap<String, (Type, ConstValue)>,
    function: Function,
    block: usize,
    scopes: Vec<HashMap<String, Slot>>,
    loops: Vec<(BlockId, BlockId)>,
}
impl<'a> Lowerer<'a> {
    fn function(
        program: &'a TypedProgram,
        functions: &'a HashMap<String, (FunctionId, FunctionSignature)>,
        constants: &'a HashMap<String, (Type, ConstValue)>,
        source: &FunctionNode,
    ) -> Result<Function> {
        let (id, signature) = &functions[&source.name];
        let mut this = Self {
            program,
            functions,
            constants,
            function: Function {
                id: *id,
                name: source.name.clone(),
                params: vec![],
                ret_ty: signature.ret.clone(),
                blocks: vec![BasicBlock::new(BlockId(0), "entry")],
                entry: BlockId(0),
                value_types: vec![],
            },
            block: 0,
            scopes: vec![HashMap::new()],
            loops: vec![],
        };
        for (param, ty) in source.parameters.iter().zip(&signature.params) {
            let value = this.value(ty.clone());
            this.function.params.push(Param {
                name: param.name.clone(),
                id: value.id,
                ty: ty.clone(),
            });
            this.bind(&param.name, ty.clone(), Some(value))?;
        }
        this.body(&source.body)?;
        if !this.terminated() {
            if this.function.ret_ty != Type::Void {
                return Err(unsupported("implicit non-void returns; add an explicit return"));
            }
            this.terminate(Terminator::Ret { ty: Type::Void, value: None });
        }
        Ok(this.function)
    }

    fn value(&mut self, ty: Type) -> Value {
        let id = ValueId(self.function.value_types.len() as u32);
        self.function.value_types.push((id, ty.clone()));
        Value { id, ty }
    }

    fn emit(&mut self, instruction: Instruction) {
        self.function.blocks[self.block].instructions.push(instruction);
    }

    fn terminate(&mut self, terminator: Terminator) {
        self.function.blocks[self.block].terminator = Some(terminator);
    }

    fn terminated(&self) -> bool {
        self.function.blocks[self.block].is_terminated()
    }

    fn new_block(&mut self, name: &str) -> BlockId {
        let id = BlockId(self.function.blocks.len() as u32);
        self.function.blocks.push(BasicBlock::new(id, name));
        id
    }

    fn switch(&mut self, block: BlockId) {
        self.block = block.0 as usize;
    }

    fn branch_if_open(&mut self, target: BlockId) {
        if !self.terminated() {
            self.terminate(Terminator::Br { target });
        }
    }

    fn slot(&self, name: &str) -> Result<Slot> {
        self.local_slot(name).ok_or_else(|| unsupported(format!("binding `{name}`")))
    }

    fn local_slot(&self, name: &str) -> Option<Slot> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name)).cloned()
    }

    fn bind(&mut self, name: &str, ty: Type, init: Option<Value>) -> Result<()> {
        let ptr = self.value(Type::ptr_to(ty.clone())).id;
        // All local storage belongs to the entry block, including loop locals.
        self.function.blocks[0].instructions.push(Instruction::Alloca {
            dst: ptr,
            ty: ty.clone(),
            align: 1,
        });
        let slot = Slot { ptr, ty };
        if let Some(value) = init {
            self.store(&slot, value)?;
        } else {
            // Alloca is hoisted, but lifetime initialization is reset at every
            // declaration execution, including loop re-entry and shadowing.
            self.emit(Instruction::Uninit { ty: slot.ty.clone(), ptr: slot.ptr, align: 1 });
        }
        self.scopes.last_mut().unwrap().insert(name.into(), slot);
        Ok(())
    }

    fn store(&mut self, slot: &Slot, value: Value) -> Result<()> {
        if value.ty != slot.ty {
            return Err(ice("store disagrees with HIR destination type"));
        }
        self.emit(Instruction::Store {
            ty: slot.ty.clone(),
            value: value.id,
            ptr: slot.ptr,
            align: 1,
        });
        Ok(())
    }

    fn body(&mut self, body: &[ASTNode]) -> Result<()> {
        self.scopes.push(HashMap::new());
        for node in body {
            if self.terminated() {
                break;
            }
            self.node(node).map_err(|mut e| {
                if e.span.is_none() {
                    e.span = self
                        .program
                        .node_id(node)
                        .and_then(|id| self.program.node_span(id))
                        .cloned()
                        .or_else(|| node.span().cloned());
                }
                e
            })?;
        }
        self.scopes.pop();
        Ok(())
    }

    fn node(&mut self, node: &ASTNode) -> Result<()> {
        match node.unspanned() {
            ASTNode::Variable(v) => {
                if v.mutability == Mutability::Static {
                    return Err(unsupported("static local storage"));
                }
                let ty = scalar(&v.type_name)?;
                // Evaluate before binding: a shadowing initializer sees the outer scope.
                let init = v.initial_value.as_ref().map(|e| self.expr(e)).transpose()?;
                self.bind(&v.name, ty, init)
            },
            ASTNode::Expression(e) | ASTNode::Statement(StatementNode::Expression(e)) => {
                self.effect(e)
            },
            ASTNode::Statement(StatementNode::Assign { variable, value }) => {
                let value = self.expr(value)?;
                self.store(&self.slot(variable)?, value)
            },
            ASTNode::Statement(StatementNode::Return(expr)) => {
                if self.function.ret_ty == Type::Void {
                    if let Some(expr) = expr {
                        self.effect(expr)?;
                    }
                    self.terminate(Terminator::Ret { ty: Type::Void, value: None });
                    return Ok(());
                }
                let value = expr.as_ref().map(|e| self.expr(e)).transpose()?;
                if value.as_ref().map(|v| &v.ty).unwrap_or(&Type::Void) != &self.function.ret_ty {
                    return Err(ice("return disagrees with HIR signature"));
                }
                self.terminate(Terminator::Ret {
                    ty: self.function.ret_ty.clone(),
                    value: value.map(|v| v.id),
                });
                Ok(())
            },
            ASTNode::Statement(StatementNode::If {
                condition,
                body,
                else_if_blocks,
                else_block,
            }) => {
                let mut branches = vec![(condition, body.as_slice())];
                if let Some(blocks) = else_if_blocks {
                    branches.extend(blocks.iter().map(|(c, b)| (c, b.as_slice())));
                }
                let join = self.new_block("if.end");
                let mut falls_through = false;
                for (condition, body) in branches {
                    let cond = self.condition(condition)?;
                    let then_bb = self.new_block("if.then");
                    let else_bb = self.new_block("if.else");
                    self.terminate(Terminator::CBr { cond, then_bb, else_bb });
                    self.switch(then_bb);
                    self.body(body)?;
                    falls_through |= !self.terminated();
                    self.branch_if_open(join);
                    self.switch(else_bb);
                }
                if let Some(body) = else_block {
                    self.body(body)?;
                }
                falls_through |= !self.terminated();
                self.branch_if_open(join);
                self.switch(join);
                if !falls_through {
                    self.terminate(Terminator::Trap {
                        reason: "unreachable if continuation".into(),
                    });
                }
                Ok(())
            },
            ASTNode::Statement(StatementNode::Match { value, arms }) => {
                let ty = self
                    .program
                    .numeric_expression_of(value)
                    .ok_or_else(|| unsupported("non-integer match scrutinee"))?
                    .result_type
                    .clone();
                if hir::conversions::integer_width(&ty).is_none() {
                    return Err(unsupported("non-integer match scrutinee"));
                }
                let scrutinee = self.expr(value)?;
                let join = self.new_block("match.end");
                let mut falls_through = false;
                let mut wildcard = false;
                for arm in arms {
                    if matches!(arm.pattern, parser::ast::MatchPattern::Wildcard) {
                        self.body(&arm.body)?;
                        falls_through |= !self.terminated();
                        self.branch_if_open(join);
                        wildcard = true;
                        break;
                    }
                    let raw = self
                        .program
                        .integer_pattern_of(&arm.pattern)
                        .ok_or_else(|| unsupported("non-integer match pattern"))?;
                    let literal = parser::ast::Literal::Int(raw.to_owned());
                    let pattern = self.literal(&literal, &ty)?;
                    let cond = self.icmp(ICmpPred::Eq, scrutinee.clone(), pattern)?;
                    let arm_block = self.new_block("match.arm");
                    let next = self.new_block("match.next");
                    self.terminate(Terminator::CBr {
                        cond: cond.id,
                        then_bb: arm_block,
                        else_bb: next,
                    });
                    self.switch(arm_block);
                    self.body(&arm.body)?;
                    falls_through |= !self.terminated();
                    self.branch_if_open(join);
                    self.switch(next);
                }
                if !wildcard {
                    falls_through = true;
                    self.branch_if_open(join);
                }
                self.switch(join);
                if !falls_through {
                    self.terminate(Terminator::Trap {
                        reason: "unreachable match continuation".into(),
                    });
                }
                Ok(())
            },
            ASTNode::Statement(StatementNode::For {
                initialization,
                condition,
                increment,
                body,
            }) => {
                // The initializer is visible to the test, body and increment,
                // but never escapes the loop's lexical scope.
                self.scopes.push(HashMap::new());
                self.node(initialization)?;
                let test = self.new_block("for.test");
                let loop_body = self.new_block("for.body");
                let step = self.new_block("for.step");
                let end = self.new_block("for.end");
                self.branch_if_open(test);
                self.switch(test);
                let cond = self.condition(condition)?;
                self.terminate(Terminator::CBr { cond, then_bb: loop_body, else_bb: end });
                self.switch(loop_body);
                self.loops.push((step, end));
                self.body(body)?;
                self.loops.pop();
                self.branch_if_open(step);
                self.switch(step);
                self.effect(increment)?;
                self.branch_if_open(test);
                self.switch(end);
                self.scopes.pop();
                Ok(())
            },
            ASTNode::Statement(StatementNode::While { condition, body }) => {
                let test = self.new_block("while.test");
                let loop_body = self.new_block("while.body");
                let end = self.new_block("while.end");
                self.terminate(Terminator::Br { target: test });
                self.switch(test);
                let cond = self.condition(condition)?;
                self.terminate(Terminator::CBr { cond, then_bb: loop_body, else_bb: end });
                self.switch(loop_body);
                self.loops.push((test, end));
                self.body(body)?;
                self.loops.pop();
                self.branch_if_open(test);
                self.switch(end);
                Ok(())
            },
            ASTNode::Statement(statement @ (StatementNode::Break | StatementNode::Continue)) => {
                let &(continue_target, end) =
                    self.loops.last().ok_or_else(|| ice("loop control outside loop"))?;
                self.terminate(Terminator::Br {
                    target: if matches!(statement, StatementNode::Break) {
                        end
                    } else {
                        continue_target
                    },
                });
                Ok(())
            },
            ASTNode::Statement(statement) => Err(unsupported(match statement {
                StatementNode::Print(_) | StatementNode::PrintFormat { .. } => "print statement",
                StatementNode::Println(_) | StatementNode::PrintlnFormat { .. } => {
                    "println statement"
                },
                StatementNode::Input { .. } => "input statement",
                StatementNode::AsmBlock { .. } => "inline assembly statement",
                StatementNode::Import(_) => "import statement",
                StatementNode::Variable(_) => "variable statement",
                _ => "statement variant",
            })),
            _ => Err(unsupported("local declaration")),
        }
    }

    fn condition(&mut self, e: &Expression) -> Result<ValueId> {
        let value = self.expr(e)?;
        if value.ty != Type::Bool {
            return Err(ice("condition missing HIR bool conversion"));
        }
        Ok(value.id)
    }
}

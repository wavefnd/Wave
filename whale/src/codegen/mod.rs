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
        WaveType::Uint(8) | WaveType::Byte => Type::U8,
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
    program.verify_conversions().map_err(|e| LowerError {
        message: format!("internal compiler error: invalid HIR conversions: {}", e.message),
        span: e.span,
    })?;
    let mut module = Module::new("x86_64-whale-linux", DataLayout::default_64bit_le());
    let mut functions = HashMap::new();
    for node in program.syntax() {
        let result = (|| {
            let ASTNode::Function(f) = node.unspanned() else {
                return Err(unsupported("non-function top-level declarations"));
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
        let ASTNode::Function(f) = node.unspanned() else { unreachable!() };
        let function = Lowerer::function(program, &functions, f).map_err(|mut e| {
            if e.span.is_none() {
                e.span = f.span.clone().or_else(|| node.span().cloned());
            }
            e
        })?;
        module.functions.push(function);
    }
    verify_module(&module).map_err(|e| ice(format!("Whale IR verification failed: {e:?}")))?;
    Ok(module)
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
    function: Function,
    block: usize,
    scopes: Vec<HashMap<String, Slot>>,
    loops: Vec<(BlockId, BlockId)>,
}
impl<'a> Lowerer<'a> {
    fn function(
        program: &'a TypedProgram,
        functions: &'a HashMap<String, (FunctionId, FunctionSignature)>,
        source: &FunctionNode,
    ) -> Result<Function> {
        let (id, signature) = &functions[&source.name];
        let mut this = Self {
            program,
            functions,
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
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name))
            .cloned()
            .ok_or_else(|| unsupported(format!("binding `{name}`")))
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
                if init.is_none() {
                    return Err(unsupported("uninitialized locals"));
                }
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
                let &(test, end) =
                    self.loops.last().ok_or_else(|| ice("loop control outside loop"))?;
                self.terminate(Terminator::Br {
                    target: if matches!(statement, StatementNode::Break) { end } else { test },
                });
                Ok(())
            },
            _ => Err(unsupported("this statement")),
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

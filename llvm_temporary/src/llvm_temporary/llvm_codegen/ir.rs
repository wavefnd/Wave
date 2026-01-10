use inkwell::context::Context;
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FunctionValue};
use inkwell::OptimizationLevel;

use parser::ast::{ASTNode, FunctionNode, Mutability, VariableNode, WaveType};
use std::collections::HashMap;

use crate::llvm_temporary::statement::generate_statement_ir;

use super::consts::create_llvm_const_value;
use super::types::{wave_type_to_llvm_type, VariableInfo};

pub unsafe fn generate_ir(ast_nodes: &[ASTNode]) -> String {
    let context: &'static Context = Box::leak(Box::new(Context::create()));
    let module: &'static _ = Box::leak(Box::new(context.create_module("main")));
    let builder: &'static _ = Box::leak(Box::new(context.create_builder()));

    let pass_manager_builder = PassManagerBuilder::create();
    pass_manager_builder.set_optimization_level(OptimizationLevel::Aggressive);
    let pass_manager: PassManager<inkwell::module::Module> = PassManager::create(());
    pass_manager_builder.populate_module_pass_manager(&pass_manager);

    let struct_types: HashMap<String, inkwell::types::StructType> = HashMap::new();
    let mut struct_field_indices: HashMap<String, HashMap<String, u32>> = HashMap::new();

    let mut global_consts: HashMap<String, BasicValueEnum> = HashMap::new();

    for ast in ast_nodes {
        if let ASTNode::Variable(VariableNode {
                                     name,
                                     type_name,
                                     initial_value,
                                     mutability,
                                 }) = ast
        {
            if *mutability == Mutability::Const {
                let initial_value = initial_value
                    .as_ref()
                    .expect("Constant must be initialized.");
                let const_val = create_llvm_const_value(context, type_name, initial_value);
                global_consts.insert(name.clone(), const_val);
            }
        }
    }

    let mut struct_types: HashMap<String, inkwell::types::StructType> = HashMap::new();
    for ast in ast_nodes {
        if let ASTNode::Struct(struct_node) = ast {
            let field_types: Vec<BasicTypeEnum> = struct_node
                .fields
                .iter()
                .map(|(_, ty)| wave_type_to_llvm_type(context, ty, &struct_types))
                .collect();
            let struct_ty = context.struct_type(&field_types, false);
            struct_types.insert(struct_node.name.clone(), struct_ty);

            let mut index_map = HashMap::new();
            for (i, (field_name, _)) in struct_node.fields.iter().enumerate() {
                index_map.insert(field_name.clone(), i as u32);
            }
            struct_field_indices.insert(struct_node.name.clone(), index_map);
        }
    }

    let mut proto_functions: Vec<(String, FunctionNode)> = Vec::new();
    for ast in ast_nodes {
        if let ASTNode::ProtoImpl(proto_impl) = ast {
            for method in &proto_impl.methods {
                let new_name = format!("{}_{}", proto_impl.target, method.name);
                let mut new_fn = method.clone();
                new_fn.name = new_name.clone();
                proto_functions.push((new_name, new_fn));
            }
        }
    }

    let mut functions: HashMap<String, FunctionValue> = HashMap::new();

    let function_nodes: Vec<FunctionNode> = ast_nodes
        .iter()
        .filter_map(|ast| {
            if let ASTNode::Function(f) = ast {
                Some(f.clone())
            } else {
                None
            }
        })
        .chain(proto_functions.iter().map(|(_, f)| f.clone()))
        .collect();

    for FunctionNode {
        name,
        parameters,
        return_type,
        ..
    } in &function_nodes
    {
        let param_types: Vec<BasicMetadataTypeEnum> = parameters
            .iter()
            .map(|p| wave_type_to_llvm_type(context, &p.param_type, &struct_types).into())
            .collect();

        let fn_type = match return_type {
            None | Some(WaveType::Void) => context.void_type().fn_type(&param_types, false),
            Some(wave_ret_ty) => {
                let llvm_ret_type = wave_type_to_llvm_type(context, wave_ret_ty, &struct_types);
                llvm_ret_type.fn_type(&param_types, false)
            }
        };

        let function = module.add_function(name, fn_type, None);
        functions.insert(name.clone(), function);
    }

    for func_node in &function_nodes {
        let function = *functions.get(&func_node.name).unwrap();
        let entry_block = context.append_basic_block(function, "entry");
        builder.position_at_end(entry_block);

        let mut variables: HashMap<String, VariableInfo> = HashMap::new();
        let mut string_counter = 0;
        let mut loop_exit_stack = vec![];
        let mut loop_continue_stack = vec![];

        for (i, param) in func_node.parameters.iter().enumerate() {
            let llvm_type = wave_type_to_llvm_type(context, &param.param_type, &struct_types);
            let alloca = builder.build_alloca(llvm_type, &param.name).unwrap();
            let param_val = function.get_nth_param(i as u32).unwrap();
            builder.build_store(alloca, param_val).unwrap();

            variables.insert(
                param.name.clone(),
                VariableInfo {
                    ptr: alloca,
                    mutability: Mutability::Let,
                    ty: param.param_type.clone(),
                },
            );
        }

        for stmt in &func_node.body {
            if let ASTNode::Statement(_) | ASTNode::Variable(_) = stmt {
                generate_statement_ir(
                    context,
                    builder,
                    module,
                    &mut string_counter,
                    stmt,
                    &mut variables,
                    &mut loop_exit_stack,
                    &mut loop_continue_stack,
                    function,
                    &global_consts,
                    &struct_types,
                    &struct_field_indices,
                );
            } else {
                panic!("Unsupported node inside function '{}'", func_node.name);
            }
        }

        let current_block = builder.get_insert_block().unwrap();
        if current_block.get_terminator().is_none() {
            let is_void_like = match &func_node.return_type {
                None => true,
                Some(WaveType::Void) => true,
                _ => false,
            };

            if is_void_like {
                builder.build_return(None).unwrap();
            } else {
                panic!(
                    "Non-void function '{}' is missing a return statement",
                    func_node.name
                );
                // builder.build_unreachable().unwrap();
            }
        }
    }

    pass_manager.run_on(module);
    module.print_to_string().to_string()
}

use inkwell::context::Context;
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::{AddressSpace, OptimizationLevel};
use parser::ast::{ASTNode, Expression, FunctionNode, Literal, Mutability, VariableNode, WaveType};

use crate::llvm_temporary::statement::generate_statement_ir;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use lexer::token::TokenType;
use std::collections::HashMap;
use std::hash::Hash;

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

fn create_llvm_const_value<'ctx>(
    context: &'ctx Context,
    ty: &WaveType,
    expr: &Expression,
) -> BasicValueEnum<'ctx> {
    let struct_types = HashMap::new();
    let llvm_type = wave_type_to_llvm_type(context, ty, &struct_types);
    match (expr, llvm_type) {
        (Expression::Literal(Literal::Number(n)), BasicTypeEnum::IntType(int_ty)) => {
            int_ty.const_int(*n as u64, true).as_basic_value_enum()
        }
        (Expression::Literal(Literal::Float(f)), BasicTypeEnum::FloatType(float_ty)) => {
            float_ty.const_float(*f).as_basic_value_enum()
        }
        // TODO: Other constant expressions (for example, true, false, string constant) can also be added here.
        _ => panic!("Constant expression must be a literal of a compatible type."),
    }
}

pub fn wave_format_to_c(format: &str, arg_types: &[BasicTypeEnum]) -> String {
    let mut result = String::new();
    let mut chars = format.chars().peekable();
    let mut arg_index = 0;

    while let Some(c) = chars.next() {
        if c == '{' {
            if let Some('}') = chars.peek() {
                chars.next(); // consume '}'

                if let Some(arg_type) = arg_types.get(arg_index) {
                    let fmt = match arg_type {
                        BasicTypeEnum::FloatType(_) => "%f",
                        BasicTypeEnum::IntType(_) => "%d",
                        BasicTypeEnum::PointerType(ptr_ty) => {
                            if ptr_ty.get_element_type().is_int_type()
                                && ptr_ty.get_element_type().into_int_type().get_bit_width() == 8
                            {
                                "%s"
                            } else {
                                "%p"
                            }
                        }
                        _ => "%d", // fallback
                    };
                    result.push_str(fmt);
                    arg_index += 1;
                    continue;
                }
            }
        }
        result.push(c);
    }

    result
}

pub fn wave_type_to_llvm_type<'ctx>(
    context: &'ctx Context,
    wave_type: &WaveType,
    struct_types: &HashMap<String, inkwell::types::StructType<'ctx>>,
) -> BasicTypeEnum<'ctx> {
    match wave_type {
        WaveType::Int(bits) => context
            .custom_width_int_type(*bits as u32)
            .as_basic_type_enum(),
        WaveType::Uint(bits) => context
            .custom_width_int_type(*bits as u32)
            .as_basic_type_enum(),
        WaveType::Float(bits) => match bits {
            32 => context.f32_type().as_basic_type_enum(),
            64 => context.f64_type().as_basic_type_enum(),
            _ => panic!("Unsupported float bit width: {}", bits),
        },
        WaveType::Bool => context.bool_type().as_basic_type_enum(),
        WaveType::Char => context.i8_type().as_basic_type_enum(), // assuming 1-byte char
        WaveType::Byte => context.i8_type().as_basic_type_enum(),
        WaveType::String => context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .as_basic_type_enum(),
        WaveType::Pointer(inner) => wave_type_to_llvm_type(context, inner, struct_types)
            .ptr_type(AddressSpace::default())
            .as_basic_type_enum(),
        WaveType::Array(inner, size) => {
            let inner_type = wave_type_to_llvm_type(context, inner, struct_types);
            inner_type.array_type(*size).as_basic_type_enum()
        }
        WaveType::Struct(name) => {
            let struct_ty = struct_types
                .get(name)
                .unwrap_or_else(|| panic!("Struct type '{}' not found in struct_types", name));
            struct_ty.as_basic_type_enum()
        }
        WaveType::Void => {
            panic!("Void type cannot be represented as BasicTypeEnum");
        }
        _ => {
            panic!("Unsupported wave type type");
        }
    }
}

pub fn generate_address_ir<'ctx>(
    context: &'ctx Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    expr: &Expression,
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    module: &'ctx inkwell::module::Module<'ctx>,
) -> PointerValue<'ctx> {
    match expr {
        Expression::Grouped(inner) => {
            generate_address_ir(context, builder, inner, variables, module)
        }

        Expression::Variable(name) => {
            let var_info = variables
                .get(name)
                .unwrap_or_else(|| panic!("Variable {} not found", name));

            var_info.ptr
        }

        Expression::Deref(inner_expr) => {
            let mut inner: &Expression = inner_expr.as_ref();
            while let Expression::Grouped(g) = inner {
                inner = g.as_ref();
            }

            match inner {
                Expression::Variable(var_name) => {
                    let ptr_to_ptr = variables
                        .get(var_name)
                        .unwrap_or_else(|| panic!("Variable {} not found", var_name))
                        .ptr;

                    let actual_ptr = builder.build_load(ptr_to_ptr, "deref_target").unwrap();
                    actual_ptr.into_pointer_value()
                }
                _ => panic!("Cannot take address: deref target is not a variable"),
            }
        }

        _ => panic!("Cannot take address of this expression"),
    }
}

#[derive(Clone)]
pub struct VariableInfo<'ctx> {
    pub ptr: PointerValue<'ctx>,
    pub mutability: Mutability,
    pub ty: WaveType,
}

pub fn get_llvm_type<'a>(context: &'a Context, ty: &TokenType) -> BasicTypeEnum<'a> {
    match ty {
        TokenType::TypeInt(bits) => context
            .custom_width_int_type(*bits as u32)
            .as_basic_type_enum(),
        TokenType::TypeUint(bits) => context
            .custom_width_int_type(*bits as u32)
            .as_basic_type_enum(),
        TokenType::TypeFloat(bits) => match bits {
            32 => context.f32_type().as_basic_type_enum(),
            64 => context.f64_type().as_basic_type_enum(),
            128 => context.f128_type().as_basic_type_enum(),
            _ => panic!("Unsupported float size: {}", bits),
        },
        TokenType::TypeBool => context.bool_type().as_basic_type_enum(),
        TokenType::TypeChar => context.i8_type().as_basic_type_enum(),
        TokenType::TypeByte => context.i8_type().as_basic_type_enum(),
        TokenType::TypePointer(inner_type) => {
            let inner_llvm_type = get_llvm_type(context, inner_type); // Box dereference
            inner_llvm_type
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum()
        }
        TokenType::TypeArray(inner_type, size) => {
            let inner_llvm_type = get_llvm_type(context, inner_type); // Box dereference
            inner_llvm_type
                .array_type(*size as u32)
                .as_basic_type_enum()
        }
        TokenType::TypeString => context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .as_basic_type_enum(),
        _ => panic!("Unsupported type: {:?}", ty),
    }
}

pub unsafe fn create_alloc<'a>(
    context: &'a Context,
    builder: &'a inkwell::builder::Builder<'a>,
    function: FunctionValue<'a>,
    name: &'a str,
) -> PointerValue<'a> {
    let alloca = builder.build_alloca(context.i32_type(), name).unwrap();
    alloca
}

use parser::ast::{ASTNode, FunctionNode, Expression, WaveType, Mutability, Value};
use inkwell::context::Context;
use inkwell::values::{PointerValue, FunctionValue, BasicValue};
use inkwell::{AddressSpace, OptimizationLevel};
use inkwell::passes::{PassManager, PassManagerBuilder};

use std::collections::HashMap;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use lexer::token::TokenType;
use crate::llvm_temporary::statement::generate_statement_ir;

pub unsafe fn generate_ir(ast_nodes: &[ASTNode]) -> String {
    let context: &'static Context = Box::leak(Box::new(Context::create()));
    let module: &'static _ = Box::leak(Box::new(context.create_module("main")));
    let builder: &'static _ = Box::leak(Box::new(context.create_builder()));

    let pass_manager_builder = PassManagerBuilder::create();
    pass_manager_builder.set_optimization_level(OptimizationLevel::Aggressive);
    let pass_manager: PassManager<inkwell::module::Module> = PassManager::create(());
    pass_manager_builder.populate_module_pass_manager(&pass_manager);

    let mut functions: HashMap<String, FunctionValue> = HashMap::new();

    for ast in ast_nodes {
        if let ASTNode::Function(FunctionNode { name, parameters, return_type, .. }) = ast {
            let param_types: Vec<BasicMetadataTypeEnum> = parameters.iter()
                .map(|p| wave_type_to_llvm_type(context, &p.param_type).into())
                .collect();

            let fn_type = match return_type {
                Some(wave_ret_ty) => {
                    let llvm_ret_type = wave_type_to_llvm_type(context, wave_ret_ty);
                    llvm_ret_type.fn_type(&param_types, false)
                }
                None => context.void_type().fn_type(&param_types, false),
            };

            let function = module.add_function(name, fn_type, None);
            functions.insert(name.clone(), function);
        }
    }

    for ast in ast_nodes {
        if let ASTNode::Function(FunctionNode { name, parameters, return_type, body }) = ast {
            let function = *functions.get(name).unwrap();
            let entry_block = context.append_basic_block(function, "entry");
            builder.position_at_end(entry_block);

            let mut variables: HashMap<String, VariableInfo> = HashMap::new();
            let mut string_counter = 0;
            let mut loop_exit_stack = vec![];
            let mut loop_continue_stack = vec![];

            for (i, param) in parameters.iter().enumerate() {
                let llvm_type = wave_type_to_llvm_type(context, &param.param_type);
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

            for stmt in body {
                match stmt {
                    ASTNode::Variable(_) | ASTNode::Statement(_) => {
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
                        );
                    }
                    _ => panic!("Unsupported ASTNode in function body"),
                }
            }

            let current_block = builder.get_insert_block().unwrap();
            if current_block.get_terminator().is_none() {
                if return_type.is_none() {
                    builder.build_return(None).unwrap();
                } else {
                    builder.build_unreachable().unwrap();
                }
            }
        }
    }

    pass_manager.run_on(module);

    module.print_to_string().to_string()
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
                            if ptr_ty.get_element_type().is_int_type() && ptr_ty.get_element_type().into_int_type().get_bit_width() == 8 {
                                "%s"
                            } else {
                                "%ld"
                            }
                        },
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

pub fn wave_type_to_llvm_type<'ctx>(context: &'ctx Context, wave_type: &WaveType) -> BasicTypeEnum<'ctx> {
    match wave_type {
        WaveType::Int(bits) => context.custom_width_int_type(*bits as u32).as_basic_type_enum(),
        WaveType::Uint(bits) => context.custom_width_int_type(*bits as u32).as_basic_type_enum(),
        WaveType::Float(bits) => match bits {
            32 => context.f32_type().as_basic_type_enum(),
            64 => context.f64_type().as_basic_type_enum(),
            _ => panic!("Unsupported float bit width: {}", bits),
        },
        WaveType::Bool => context.bool_type().as_basic_type_enum(),
        WaveType::Char => context.i8_type().as_basic_type_enum(), // assuming 1-byte char
        WaveType::Byte => context.i8_type().as_basic_type_enum(),
        WaveType::String => context.i8_type().ptr_type(AddressSpace::default()).as_basic_type_enum(),
        WaveType::Pointer(inner) => wave_type_to_llvm_type(context, inner).ptr_type(AddressSpace::default()).as_basic_type_enum(),
        WaveType::Array(inner, size) => {
            let inner_type = wave_type_to_llvm_type(context, inner);
            inner_type.array_type(*size).as_basic_type_enum()
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
        Expression::Variable(name) => {
            let var_info = variables.get(name)
                .unwrap_or_else(|| panic!("Variable {} not found", name));

            var_info.ptr
        }

        Expression::Deref(inner_expr) => {
            match &**inner_expr {
                Expression::Variable(var_name) => {
                    let ptr_to_ptr = variables.get(var_name)
                        .unwrap_or_else(|| panic!("Variable {} not found", var_name))
                        .ptr;

                    let actual_ptr = builder.build_load(ptr_to_ptr, "deref_target").unwrap();
                    actual_ptr.into_pointer_value()
                }
                _ => panic!("Nested deref not supported"),
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
        TokenType::TypeInt(bits) => context.custom_width_int_type(*bits as u32).as_basic_type_enum(),
        TokenType::TypeUint(bits) => context.custom_width_int_type(*bits as u32).as_basic_type_enum(),
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
            let inner_llvm_type = get_llvm_type(context, &*inner_type); // Box 역참조
            inner_llvm_type.ptr_type(AddressSpace::default()).as_basic_type_enum()
        }
        TokenType::TypeArray(inner_type, size) => {
            let inner_llvm_type = get_llvm_type(context, &*inner_type); // Box 역참조
            inner_llvm_type.array_type(*size as u32).as_basic_type_enum()
        }
        TokenType::TypeString => context.i8_type().ptr_type(AddressSpace::default()).as_basic_type_enum(),
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
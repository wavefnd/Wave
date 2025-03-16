use crate::parser::ast::{ASTNode, FunctionNode, StatementNode, Expression, VariableNode, Literal};
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::values::{PointerValue, FunctionValue};
use inkwell::AddressSpace;

use std::collections::HashMap;
use inkwell::types::{AnyTypeEnum, BasicType, BasicTypeEnum};
use crate::lexer::TokenType;
use crate::parser::parse_type;

/// Generates LLVM Intermediate Representation (IR) as a string from the provided Abstract Syntax Tree (AST).
///
/// This unsafe function sets up an LLVM context, module, and builder to convert an AST into its corresponding LLVM IR.
/// It handles function definitions with variable declarations (with optional initialization), print statements, conditional
/// branches (if statements), and while loops. Unsupported constructs or nodes (including commented-out for-loop code)
/// are ignored.
///
/// # Safety
///
/// This function is unsafe due to its direct manipulation of low-level LLVM constructs. Callers must ensure that the AST
/// is valid and that any side effects of using LLVM objects are properly managed.
///
/// # Examples
///
/// ```
/// // Assuming ASTNode and FunctionNode are defined appropriately in your project.
/// let main_function = FunctionNode {
///     name: String::from("main"),
///     parameters: vec![],
///     body: vec![], // Populate with desired AST nodes (e.g., Variable, Println, If, While, etc.)
/// };
/// let ast = ASTNode::Function(main_function);
///
/// unsafe {
///     let ir = generate_ir(&ast);
///     println!("{}", ir);
/// }
/// ```
pub unsafe fn generate_ir(ast: &ASTNode) -> String {
    let context = Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();

    // HashMap to store variables
    let mut variables: HashMap<String, PointerValue> = HashMap::new();

    if let ASTNode::Function(FunctionNode { name, parameters: _, body }) = ast {
        // Create function type (void -> void)
        let fn_type = context.void_type().fn_type(&[], false);
        let function = module.add_function(name, fn_type, None);

        // Create entry block
        let entry_block = context.append_basic_block(function, "entry");
        builder.position_at_end(entry_block);

        let mut string_counter = 0;

        for stmt in body {
            match stmt {
                ASTNode::Variable(VariableNode { name, type_name, initial_value }) => {
                    // Parse the type
                    let llvm_type = match parse_type(type_name) {
                        Some(token_type) => get_llvm_type(&context, &token_type),
                        None => panic!("Unsupported type: {}", type_name),
                    };

                    // Create alloca for the variable
                    let alloca = builder.build_alloca(llvm_type, &name).unwrap();
                    variables.insert(name.clone(), alloca);

                    // Initialize the variable if an initial value is provided
                    if let Some(Literal::Number(value)) = initial_value {
                        let init_value = match llvm_type {
                            BasicTypeEnum::IntType(int_type) => {
                                int_type.const_int(*value as u64, false)
                            }
                            _ => panic!("Unsupported type for initialization"),
                        };
                        let _ = builder.build_store(alloca, init_value);
                    }
                }
                ASTNode::Statement(StatementNode::Println { format, args }) |
                ASTNode::Statement(StatementNode::Print { format, args }) => {
                    // Determine the format string based on the type of the first argument
                    let format = if let Some(Expression::Variable(var_name)) = args.get(0) {
                        if let Some(alloca) = variables.get(var_name) {
                            match alloca.get_type().get_element_type() {
                                AnyTypeEnum::IntType(_) => format.replace("{}", "%d"),
                                _ => format.replace("{}", "%d"), // Default to %d
                            }
                        } else {
                            format.replace("{}", "%d") // Default to %d
                        }
                    } else {
                        format.replace("{}", "%d") // Default to %d
                    };

                    // Generate unique global name for the format string
                    let global_name = format!("str_{}_{}", name, string_counter);
                    string_counter += 1;

                    // Create null-terminated string
                    let mut bytes = format.as_bytes().to_vec();
                    bytes.push(0);
                    let const_str = context.const_string(&bytes, false);

                    // Create global variable for the format string
                    let global = module.add_global(
                        context.i8_type().array_type(bytes.len() as u32),
                        None,
                        &global_name,
                    );
                    global.set_initializer(&const_str);
                    global.set_linkage(Linkage::Private);
                    global.set_constant(true);

                    // Get printf function
                    let printf_type = context.i32_type().fn_type(
                        &[context.i8_type().ptr_type(AddressSpace::default()).into()],
                        true
                    );
                    let printf_func = match module.get_function("printf") {
                        Some(func) => func,
                        None => module.add_function("printf", printf_type, None),
                    };

                    // Create GEP to get i8* pointer to the format string
                    let zero = context.i32_type().const_zero();
                    let indices = [zero, zero];
                    let gep = builder.build_gep(
                        global.as_pointer_value(),
                        &indices,
                        "gep",
                    ).unwrap();

                    // Prepare arguments for printf
                    let mut printf_args = vec![gep.into()];

                    // Add additional arguments
                    for arg in args {
                        let value = match arg {
                            Expression::Variable(var_name) => {
                                // Find the alloca of the variable and load the value
                                if let Some(alloca) = variables.get(var_name) {
                                    let loaded_value = builder.build_load(*alloca, var_name).unwrap();
                                    match loaded_value.get_type() {
                                        BasicTypeEnum::IntType(_) => loaded_value.into_int_value(),
                                        _ => panic!("Unsupported type for printf argument"),
                                    }
                                } else {
                                    panic!("Variable {} not found", var_name);
                                }
                            }
                            Expression::Literal(literal) => {
                                match literal {
                                    Literal::Number(value) => {
                                        context.i32_type().const_int(*value as u64, false)
                                    }
                                    _ => unimplemented!("Unsupported literal type"),
                                }
                            }
                            _ => unimplemented!("Unsupported expression type"),
                        };
                        printf_args.push(value.into());
                    }

                    // Call printf
                    let _ = builder.build_call(printf_func, &printf_args, "printf_call");
                }
                ASTNode::Statement(StatementNode::If { condition, body, /* else_if_blocks, */ else_block, ..  }) => {
                    // Generate IR for if statement
                    let condition_value = generate_expression_ir(&context, &builder, condition, &mut variables);
                    let then_block = context.append_basic_block(function, "then");
                    let else_block = context.append_basic_block(function, "else");
                    let merge_block = context.append_basic_block(function, "merge");

                    let _ = builder.build_conditional_branch(condition_value, then_block, else_block);

                    // Generate then block
                    builder.position_at_end(then_block);
                    for stmt in body {
                        generate_statement_ir(&context, &builder, stmt, &mut variables);
                    }
                    let _ = builder.build_unconditional_branch(merge_block);

                    // Generate else block
                    builder.position_at_end(else_block);
                    let _ = builder.build_unconditional_branch(merge_block);

                    // Position builder at merge block
                    builder.position_at_end(merge_block);
                }
                ASTNode::Statement(StatementNode::While { condition, body }) => {
                    // Generate IR for while loop
                    let condition_block = context.append_basic_block(function, "while.cond");
                    let body_block = context.append_basic_block(function, "while.body");
                    let merge_block = context.append_basic_block(function, "while.merge");

                    let _ = builder.build_unconditional_branch(condition_block);

                    // Generate condition block
                    builder.position_at_end(condition_block);
                    let condition_value = generate_expression_ir(&context, &builder, condition, &mut variables);
                    let _ = builder.build_conditional_branch(condition_value, body_block, merge_block);

                    // Generate body block
                    builder.position_at_end(body_block);
                    for stmt in body {
                        generate_statement_ir(&context, &builder, stmt, &mut variables);
                    }
                    let _ = builder.build_unconditional_branch(condition_block);

                    // Position builder at merge block
                    builder.position_at_end(merge_block);
                }
                /*
                ASTNode::Statement(StatementNode::For { initialization, condition, increment, body }) => {
                    // Generate IR for for loop
                    let init_block = context.append_basic_block(function, "for.init");
                    let condition_block = context.append_basic_block(function, "for.cond");
                    let body_block = context.append_basic_block(function, "for.body");
                    let increment_block = context.append_basic_block(function, "for.inc");
                    let merge_block = context.append_basic_block(function, "for.merge");

                    // Generate initialization block
                    builder.position_at_end(init_block);
                    generate_expression_ir(&context, &builder, initialization, &mut variables);
                    builder.build_unconditional_branch(condition_block);

                    // Generate condition block
                    builder.position_at_end(condition_block);
                    let condition_value = generate_expression_ir(&context, &builder, condition, &mut variables);
                    builder.build_conditional_branch(condition_value, body_block, merge_block);

                    // Generate body block
                    builder.position_at_end(body_block);
                    for stmt in body {
                        generate_statement_ir(&context, &builder, stmt, &mut variables);
                    }
                    let _ = builder.build_unconditional_branch(increment_block);

                    // Generate increment block
                    builder.position_at_end(increment_block);
                    generate_expression_ir(&context, &builder, increment, &mut variables);
                    let _ = builder.build_unconditional_branch(condition_block);

                    // Position builder at merge block
                    builder.position_at_end(merge_block);
                }
                 */
                _ => {}
            }
        }

        // Add void return
        let _ = builder.build_return(None);
    }

    module.print_to_string().to_string()
}

fn generate_expression_ir<'a>(
    context: &'a Context,
    builder: &'a inkwell::builder::Builder<'a>,
    expr: &Expression,
    variables: &mut HashMap<String, PointerValue<'a>>,
) -> inkwell::values::IntValue<'a> {
    match expr {
        Expression::Literal(Literal::Number(value)) => {
            context.i32_type().const_int(*value as u64, false)
        }
        Expression::Variable(var_name) => {
            if let Some(alloca) = variables.get(var_name) {
                builder.build_load(*alloca, var_name).unwrap().into_int_value()
            } else {
                panic!("Variable {} not found", var_name);
            }
        }
        _ => unimplemented!("Unsupported expression type"),
    }
}

fn generate_statement_ir<'a>(
    context: &'a Context,
    builder: &'a inkwell::builder::Builder<'a>,
    stmt: &ASTNode,
    variables: &mut HashMap<String, PointerValue<'a>>,
) {
    match stmt {
        ASTNode::Variable(VariableNode { name, type_name, initial_value }) => {
            // Parse the type
            let llvm_type = match parse_type(type_name) {
                Some(token_type) => get_llvm_type(&context, &token_type),
                None => panic!("Unsupported type: {}", type_name),
            };

            // Create alloca for the variable
            let alloca = builder.build_alloca(llvm_type, &name).unwrap();
            variables.insert(name.clone(), alloca);

            // Initialize the variable if an initial value is provided
            if let Some(Literal::Number(value)) = initial_value {
                let init_value = match llvm_type {
                    BasicTypeEnum::IntType(int_type) => {
                        int_type.const_int(*value as u64, false)
                    }
                    _ => panic!("Unsupported type for initialization"),
                };
                let _ = builder.build_store(alloca, init_value);
            }
        }
        _ => {}
    }
}

fn get_llvm_type<'a>(context: &'a Context, ty: &TokenType) -> BasicTypeEnum<'a> {
    match ty {
        TokenType::TypeInt(bits) => context.custom_width_int_type(*bits as u32).as_basic_type_enum(),
        TokenType::TypeUint(bits) => context.custom_width_int_type(*bits as u32).as_basic_type_enum(),
        TokenType::TypeFloat(bits) => match bits {
            32 => context.f32_type().as_basic_type_enum(),
            64 => context.f64_type().as_basic_type_enum(),
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

unsafe fn create_alloc<'a>(
    context: &'a Context,
    builder: &'a inkwell::builder::Builder<'a>,
    function: FunctionValue<'a>,
    name: &'a str,
) -> PointerValue<'a> {
    let alloca = builder.build_alloca(context.i32_type(), name).unwrap();
    alloca
}
use crate::llvm_temporary::expression::lvalue::generate_lvalue_ir;
use crate::llvm_temporary::expression::rvalue::generate_expression_ir;
use crate::llvm_temporary::llvm_codegen::{wave_format_to_c, wave_format_to_scanf, VariableInfo};
use inkwell::module::{Linkage, Module};
use inkwell::types::{StructType};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, BasicValue};
use inkwell::{AddressSpace, IntPredicate};
use parser::ast::Expression;
use std::collections::HashMap;

fn make_global_cstr<'ctx>(
    context: &'ctx inkwell::context::Context,
    module: &'ctx Module<'ctx>,
    string_counter: &mut usize,
    s: &str,
) -> inkwell::values::PointerValue<'ctx> {
    let global_name = format!("str_{}", *string_counter);
    *string_counter += 1;

    let mut bytes = s.as_bytes().to_vec();
    bytes.push(0);

    let const_str = context.const_string(&bytes, false);
    let global = module.add_global(
        context.i8_type().array_type(bytes.len() as u32),
        None,
        &global_name,
    );
    global.set_initializer(&const_str);
    global.set_linkage(Linkage::Private);
    global.set_constant(true);

    let zero = context.i32_type().const_zero();
    let indices = [zero, zero];

    unsafe { module.get_context().create_builder().build_gep(global.as_pointer_value(), &indices, "tmp_gep").unwrap() }
}

pub(super) fn gen_print_literal_ir<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    module: &'ctx Module<'ctx>,
    string_counter: &mut usize,
    message: &str,
) {
    let global_name = format!("str_{}", *string_counter);
    *string_counter += 1;

    let mut bytes = message.as_bytes().to_vec();
    bytes.push(0);
    let const_str = context.const_string(&bytes, false);

    let global = module.add_global(
        context.i8_type().array_type(bytes.len() as u32),
        None,
        &global_name,
    );
    global.set_initializer(&const_str);
    global.set_linkage(Linkage::Private);
    global.set_constant(true);

    let printf_type = context.i32_type().fn_type(
        &[context.i8_type().ptr_type(AddressSpace::default()).into()],
        true,
    );
    let printf_func = match module.get_function("printf") {
        Some(f) => f,
        None => module.add_function("printf", printf_type, None),
    };

    let zero = context.i32_type().const_zero();
    let indices = [zero, zero];
    let gep = unsafe { builder.build_gep(global.as_pointer_value(), &indices, "gep").unwrap() };

    let _ = builder.build_call(printf_func, &[gep.into()], "printf_call");
}

pub(super) fn gen_print_format_ir<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    module: &'ctx Module<'ctx>,
    string_counter: &mut usize,
    format: &str,
    args: &[Expression],
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
) {
    // ⚠️ 원본은 args를 2번 평가했는데(부작용 가능), 여기선 1번만 평가하도록 고쳤어.
    let mut arg_vals: Vec<BasicValueEnum<'ctx>> = Vec::with_capacity(args.len());
    let mut arg_types = Vec::with_capacity(args.len());

    for arg in args {
        let val = generate_expression_ir(
            context,
            builder,
            arg,
            variables,
            module,
            None,
            global_consts,
            struct_types,
            struct_field_indices,
        );
        arg_types.push(val.get_type());
        arg_vals.push(val);
    }

    let c_format_string = wave_format_to_c(format, &arg_types);

    let global_name = format!("str_{}", *string_counter);
    *string_counter += 1;

    let mut bytes = c_format_string.as_bytes().to_vec();
    bytes.push(0);
    let const_str = context.const_string(&bytes, false);

    let global = module.add_global(
        context.i8_type().array_type(bytes.len() as u32),
        None,
        &global_name,
    );
    global.set_initializer(&const_str);
    global.set_linkage(Linkage::Private);
    global.set_constant(true);

    let printf_type = context.i32_type().fn_type(
        &[context.i8_type().ptr_type(AddressSpace::default()).into()],
        true,
    );
    let printf_func = match module.get_function("printf") {
        Some(func) => func,
        None => module.add_function("printf", printf_type, None),
    };

    let zero = context.i32_type().const_zero();
    let indices = [zero, zero];
    let gep = unsafe { builder.build_gep(global.as_pointer_value(), &indices, "gep").unwrap() };

    let mut printf_args: Vec<BasicMetadataValueEnum<'ctx>> = vec![gep.into()];

    for value in arg_vals {
        let casted_value = match value {
            BasicValueEnum::PointerValue(ptr_val) => {
                let element_ty = ptr_val.get_type().get_element_type();
                if element_ty.is_int_type() && element_ty.into_int_type().get_bit_width() == 8 {
                    ptr_val.as_basic_value_enum()
                } else {
                    builder
                        .build_ptr_to_int(ptr_val, context.i64_type(), "ptr_as_int")
                        .unwrap()
                        .as_basic_value_enum()
                }
            }
            BasicValueEnum::FloatValue(fv) => {
                let double_ty = context.f64_type();
                builder
                    .build_float_ext(fv, double_ty, "cast_to_double")
                    .unwrap()
                    .as_basic_value_enum()
            }
            _ => value,
        };

        printf_args.push(casted_value.into());
    }

    let _ = builder.build_call(printf_func, &printf_args, "printf_call");
}

pub(super) fn gen_input_ir<'ctx>(
    context: &'ctx inkwell::context::Context,
    builder: &'ctx inkwell::builder::Builder<'ctx>,
    module: &'ctx Module<'ctx>,
    string_counter: &mut usize,
    format: &str,
    args: &[Expression],
    variables: &mut HashMap<String, VariableInfo<'ctx>>,
    global_consts: &HashMap<String, BasicValueEnum<'ctx>>,
    struct_types: &HashMap<String, StructType<'ctx>>,
    struct_field_indices: &HashMap<String, HashMap<String, u32>>,
) {
    // lvalue도 2번 만들 필요 없어서 1번만 만들도록 정리
    let mut ptrs = Vec::with_capacity(args.len());
    let mut arg_types = Vec::with_capacity(args.len());

    for arg in args {
        let ptr = generate_lvalue_ir(
            context,
            builder,
            arg,
            variables,
            module,
            global_consts,
            struct_types,
            struct_field_indices,
        );
        arg_types.push(ptr.get_type().get_element_type());
        ptrs.push(ptr);
    }

    let c_format_string = wave_format_to_scanf(format, &arg_types);

    let global_name = format!("str_{}", *string_counter);
    *string_counter += 1;

    let mut bytes = c_format_string.as_bytes().to_vec();
    bytes.push(0);
    let const_str = context.const_string(&bytes, false);

    let global = module.add_global(
        context.i8_type().array_type(bytes.len() as u32),
        None,
        &global_name,
    );
    global.set_initializer(&const_str);
    global.set_linkage(Linkage::Private);
    global.set_constant(true);

    let scanf_type = context.i32_type().fn_type(
        &[context.i8_type().ptr_type(AddressSpace::default()).into()],
        true,
    );
    let scanf_func = match module.get_function("scanf") {
        Some(func) => func,
        None => module.add_function("scanf", scanf_type, None),
    };

    let zero = context.i32_type().const_zero();
    let indices = [zero, zero];
    let gep = unsafe { builder.build_gep(global.as_pointer_value(), &indices, "fmt_gep").unwrap() };

    let mut scanf_args: Vec<BasicMetadataValueEnum<'ctx>> = vec![gep.into()];
    for ptr in ptrs {
        scanf_args.push(ptr.into());
    }

    let call = builder
        .build_call(scanf_func, &scanf_args, "scanf_call")
        .unwrap();

    let ret = call
        .try_as_basic_value()
        .left()
        .unwrap()
        .into_int_value();

    let expected = context.i32_type().const_int(args.len() as u64, false);
    let ok = builder
        .build_int_compare(IntPredicate::EQ, ret, expected, "scanf_ok")
        .unwrap();

    let cur_bb = builder.get_insert_block().unwrap();
    let cur_fn = cur_bb.get_parent().unwrap();

    let ok_bb = context.append_basic_block(cur_fn, "input_ok");
    let fail_bb = context.append_basic_block(cur_fn, "input_fail");
    let cont_bb = context.append_basic_block(cur_fn, "input_cont");

    builder.build_conditional_branch(ok, ok_bb, fail_bb).unwrap();

    builder.position_at_end(fail_bb);

    let exit_ty = context.void_type().fn_type(&[context.i32_type().into()], false);
    let exit_fn = match module.get_function("exit") {
        Some(f) => f,
        None => module.add_function("exit", exit_ty, None),
    };

    builder
        .build_call(
            exit_fn,
            &[context.i32_type().const_int(1, false).into()],
            "exit_call",
        )
        .unwrap();
    builder.build_unreachable().unwrap();

    builder.position_at_end(ok_bb);
    builder.build_unconditional_branch(cont_bb).unwrap();

    builder.position_at_end(cont_bb);
}

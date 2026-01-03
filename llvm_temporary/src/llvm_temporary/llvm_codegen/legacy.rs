use inkwell::context::Context;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{FunctionValue, PointerValue};
use inkwell::AddressSpace;

use lexer::token::TokenType;

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
            let inner_llvm_type = get_llvm_type(context, inner_type);
            inner_llvm_type
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum()
        }
        TokenType::TypeArray(inner_type, size) => {
            let inner_llvm_type = get_llvm_type(context, inner_type);
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

use inkwell::context::Context;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::PointerValue;
use inkwell::AddressSpace;

use parser::ast::{Mutability, WaveType};
use std::collections::HashMap;

pub type StructFieldMap = HashMap<String, HashMap<String, u32>>;

pub fn build_field_map(fields: &[(String, parser::ast::WaveType)]) -> HashMap<String, u32> {
    let mut m = HashMap::new();
    for (i, (name, _ty)) in fields.iter().enumerate() {
        m.insert(name.clone(), i as u32);
    }
    m
}

pub fn get_field_index(
    struct_fields: &StructFieldMap,
    struct_name: &str,
    field: &str,
) -> u32 {
    *struct_fields
        .get(struct_name)
        .unwrap_or_else(|| panic!("Struct '{}' field map not found", struct_name))
        .get(field)
        .unwrap_or_else(|| panic!("Field '{}' not found in struct '{}'", field, struct_name))
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
        WaveType::Char => context.i8_type().as_basic_type_enum(),
        WaveType::Byte => context.i8_type().as_basic_type_enum(),
        WaveType::Void => context.i8_type().as_basic_type_enum(), // fallback (shouldn't be used)
        WaveType::Pointer(inner) => wave_type_to_llvm_type(context, inner, struct_types)
            .ptr_type(AddressSpace::default())
            .as_basic_type_enum(),
        WaveType::Array(inner, size) => {
            let inner_ty = wave_type_to_llvm_type(context, inner, struct_types);
            inner_ty.array_type(*size as u32).as_basic_type_enum()
        }
        WaveType::String => context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .as_basic_type_enum(),
        WaveType::Struct(name) => {
            let struct_ty = struct_types
                .get(name)
                .unwrap_or_else(|| panic!("Struct type '{}' not found", name));
            struct_ty.as_basic_type_enum()
        }
    }
}

#[derive(Clone)]
pub struct VariableInfo<'ctx> {
    pub ptr: PointerValue<'ctx>,
    pub mutability: Mutability,
    pub ty: WaveType,
}

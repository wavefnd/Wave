use inkwell::context::Context;
use inkwell::values::PointerValue;

use parser::ast::Expression;
use std::collections::HashMap;

use super::types::VariableInfo;

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

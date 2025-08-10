use crate::ast::{FunctionNode, FunctionSignature, ProtoNode, StructNode, WaveType};

pub fn struct_implements_proto(struct_node: &StructNode, proto_node: &ProtoNode) -> bool {
    proto_node.methods.iter().all(|proto_method| {
        struct_node.methods.iter().any(|struct_method| {
            method_matches(struct_method, proto_method)
        })
    })
}

pub fn check_proto_assignment(proto: &ProtoNode, target_struct: &StructNode) -> bool {
    struct_implements_proto(target_struct, proto)
}

pub fn method_matches(struct_method: &FunctionNode, proto_method: &FunctionSignature) -> bool {
    if struct_method.name != proto_method.name {
        return false;
    }

    if struct_method.parameters.len() != proto_method.params.len() {
        return false;
    }

    for (struct_param, proto_param) in struct_method.parameters.iter().zip(proto_method.params.iter()) {
        let struct_param_type = &struct_param.param_type;
        let proto_param_type = &proto_param.1;

        if struct_param_type != proto_param_type {
            return false;
        }
    }

    let struct_return_type = struct_method.return_type.as_ref().unwrap_or(&WaveType::Void);

    if struct_return_type != &proto_method.return_type {
        return false;
    }

    true
}
use crate::ast::{FunctionSignature, ProtoNode, StructNode, WaveType};

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

pub fn method_matches(struct_method: &FunctionSignature, proto_method: &FunctionSignature) -> bool {
    if struct_method.name != proto_method.name {
        return false;
    }

    if struct_method.params.len() != proto_method.params.len() {
        return false;
    }

    for (i, (_, proto_param_type)) in proto_method.params.iter().enumerate() {
        let (_, struct_param_type) = &struct_method.params[i];
        if struct_param_type != proto_param_type {
            return false;
        }
    }

    struct_method.return_type == proto_method.return_type
}
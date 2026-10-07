// SPDX-License-Identifier: MPL-2.0
//! Version 1 source AST schema. Explicit node mappings are independent of Debug.
use crate::ast::*;
use error::SourceSpan;
use std::cell::RefCell;
use std::collections::HashMap;
use utils::wson::{self, Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum AstFormat {
    #[default]
    Wson,
    Json,
    Sexpr,
}
impl AstFormat {
    pub fn extension(self) -> &'static str {
        match self {
            Self::Wson => "ast.wson",
            Self::Json => "ast.json",
            Self::Sexpr => "ast",
        }
    }
}
struct Context<'a> {
    source: &'a str,
    expressions: Option<&'a RefCell<HashMap<*const Expression, Value>>>,
}
trait Data {
    fn data(&self, cx: &Context) -> Value;
}
fn record(kind: &str, fields: Vec<(&str, Value)>) -> Value {
    let mut result = vec![("kind".into(), Value::String(kind.into()))];
    result.extend(fields.into_iter().map(|(k, v)| (k.into(), v)));
    Value::Object(result)
}

fn put(value: &mut Value, key: &str, item: Value) {
    if let Value::Object(fields) = value {
        if let Some((_, existing)) = fields.iter_mut().find(|(k, _)| k == key) {
            *existing = item;
        } else {
            fields.push((key.into(), item));
        }
    }
}
impl Data for String {
    fn data(&self, _: &Context) -> Value {
        Value::String(self.clone())
    }
}
impl Data for char {
    fn data(&self, _: &Context) -> Value {
        Value::String(self.to_string())
    }
}
impl Data for bool {
    fn data(&self, _: &Context) -> Value {
        Value::Bool(*self)
    }
}
macro_rules! integer { ($($t:ty),*)=>{$(impl Data for $t {fn data(&self,_:&Context)->Value {Value::integer(*self as u64)}})*}; }
integer!(u8, u16, u32, usize);
impl<T: Data> Data for Vec<T> {
    fn data(&self, cx: &Context) -> Value {
        Value::Array(self.iter().map(|v| v.data(cx)).collect())
    }
}
impl<T: Data> Data for Box<T> {
    fn data(&self, cx: &Context) -> Value {
        self.as_ref().data(cx)
    }
}
impl<T: Data> Data for Option<T> {
    fn data(&self, cx: &Context) -> Value {
        self.as_ref().map_or(Value::Null, |v| v.data(cx))
    }
}
impl<A: Data, B: Data> Data for (A, B) {
    fn data(&self, cx: &Context) -> Value {
        Value::Array(vec![self.0.data(cx), self.1.data(cx)])
    }
}
impl Data for SourceSpan {
    fn data(&self, cx: &Context) -> Value {
        Value::Object(vec![
            ("file".into(), self.file.data(cx)),
            ("start".into(), self.start.data(cx)),
            ("end".into(), self.end.data(cx)),
            ("line".into(), self.line.data(cx)),
            ("column".into(), self.column.data(cx)),
            ("end_line".into(), self.end_line.data(cx)),
            ("end_column".into(), self.end_column.data(cx)),
        ])
    }
}

pub fn dump(
    nodes: &[ASTNode],
    source: &str,
    file: &str,
    format: AstFormat,
) -> Result<String, wson::Error> {
    crate::expression_depth::validate(nodes).map_err(|e| wson::Error {
        message: e.message,
        offset: e.span.as_ref().map_or(0, |s| s.start),
        line: e.span.as_ref().map_or(1, |s| s.line),
        column: e.span.as_ref().map_or(1, |s| s.column),
    })?;
    let cx = Context { source, expressions: None };
    let value = Value::Object(vec![
        ("schema_version".into(), Value::integer(1)),
        ("stage".into(), Value::String("parsed".into())),
        (
            "sources".into(),
            Value::Array(vec![Value::object([
                ("id", Value::integer(0)),
                ("path", Value::string(file)),
            ])]),
        ),
        ("nodes".into(), Value::Array(nodes.iter().map(|node| node.data(&cx)).collect())),
    ]);
    let text = match format {
        AstFormat::Wson => wson::dumps_with_depth_limit(&value, wson::Format::Wson, true, 512)?,
        AstFormat::Json => wson::dumps_with_depth_limit(&value, wson::Format::Json, true, 512)?,
        AstFormat::Sexpr => {
            wson::dumps_with_depth_limit(&value, wson::Format::Json, false, 512)?;
            sexpr(&value, 0)
        },
    };
    Ok(text + "\n")
}

fn sexpr(value: &Value, depth: usize) -> String {
    let indent = "  ".repeat(depth + 1);
    match value {
        Value::Object(fields) => {
            let tag = value.get_str("kind").unwrap_or(if depth == 0 { "ast" } else { "record" });
            let mut out = format!("({tag}");
            for (key, v) in fields {
                if key != "kind" {
                    out.push_str(&format!("\n{indent}({key} {})", sexpr(v, depth + 1)));
                }
            }
            out.push(')');
            out
        },
        Value::Array(values) => {
            let mut out = String::from("(list");
            for item in values {
                out.push_str(&format!("\n{indent}{}", sexpr(item, depth + 1)));
            }
            out.push(')');
            out
        },
        Value::String(s) => wson::quote(s),
        Value::Number(n) => n.as_str().into(),
        Value::Bool(b) => b.to_string(),
        Value::Null => "nil".into(),
        _ => unreachable!("AST schema only constructs JSON-compatible values"),
    }
}

impl Data for WaveType {
    fn data(&self, cx: &Context) -> Value {
        match self {
            Self::Isz => record("isz_type", vec![]),
            Self::Usz => record("usz_type", vec![]),
            Self::Int(p0) => record("int_type", vec![("bits", p0.data(cx))]),
            Self::Uint(p0) => record("uint_type", vec![("bits", p0.data(cx))]),
            Self::Float(p0) => record("float_type", vec![("bits", p0.data(cx))]),
            Self::Bool => record("bool_type", vec![]),
            Self::Char => record("char_type", vec![]),
            Self::Byte => record("byte_type", vec![]),
            Self::String => record("string_type", vec![]),
            Self::Pointer(p0) => record("pointer_type", vec![("pointee", p0.data(cx))]),
            Self::Future(p0) => record("future_type", vec![("result", p0.data(cx))]),
            Self::Array(p0, p1) => {
                record("array_type", vec![("element", p0.data(cx)), ("length", p1.data(cx))])
            },
            Self::Void => record("void_type", vec![]),
            Self::Never => record("never_type", vec![]),
            Self::Struct(p0) => record("struct_type", vec![("name", p0.data(cx))]),
            Self::Variant(p0) => record("variant_type", vec![("name", p0.data(cx))]),
        }
    }
}
impl Data for ASTNode {
    fn data(&self, cx: &Context) -> Value {
        match self {
            Self::Located { value, span } => {
                let mut node = value.data(cx);
                put(&mut node, "span", span.data(cx));
                node
            },
            Self::Function(p0) => p0.data(cx),
            Self::ExternFunction(p0) => p0.data(cx),
            Self::Program(p0) => p0.data(cx),
            Self::Statement(p0) => p0.data(cx),
            Self::Variable(p0) => p0.data(cx),
            Self::Expression(p0) => p0.data(cx),
            Self::Struct(p0) => p0.data(cx),
            Self::ProtoImpl(p0) => p0.data(cx),
            Self::TypeAlias(p0) => p0.data(cx),
            Self::Enum(p0) => p0.data(cx),
            Self::Variant(p0) => p0.data(cx),
        }
    }
}
impl Data for Visibility {
    fn data(&self, _cx: &Context) -> Value {
        match self {
            Self::Private => record("private", vec![]),
            Self::Public => record("public", vec![]),
        }
    }
}
impl Data for TypeAliasNode {
    fn data(&self, cx: &Context) -> Value {
        record(
            "type_alias",
            vec![
                ("name", self.name.data(cx)),
                ("target", self.target.data(cx)),
                ("visibility", self.visibility.data(cx)),
            ],
        )
    }
}
impl Data for EnumNode {
    fn data(&self, cx: &Context) -> Value {
        record(
            "enum",
            vec![
                ("name", self.name.data(cx)),
                ("repr_type", self.repr_type.data(cx)),
                ("variants", self.variants.data(cx)),
                ("visibility", self.visibility.data(cx)),
            ],
        )
    }
}
impl Data for EnumVariantNode {
    fn data(&self, cx: &Context) -> Value {
        record(
            "enum_variant",
            vec![
                ("span", self.span.data(cx)),
                ("name", self.name.data(cx)),
                ("explicit_value", self.explicit_value.data(cx)),
            ],
        )
    }
}
impl Data for VariantNode {
    fn data(&self, cx: &Context) -> Value {
        record(
            "variant",
            vec![
                ("name", self.name.data(cx)),
                ("generic_params", self.generic_params.data(cx)),
                ("cases", self.cases.data(cx)),
                ("visibility", self.visibility.data(cx)),
            ],
        )
    }
}
impl Data for VariantCaseNode {
    fn data(&self, cx: &Context) -> Value {
        record(
            "variant_case",
            vec![
                ("span", self.span.data(cx)),
                ("name", self.name.data(cx)),
                ("payload_types", self.payload_types.data(cx)),
            ],
        )
    }
}
impl Data for FunctionNode {
    fn data(&self, cx: &Context) -> Value {
        record(
            "function",
            vec![
                ("is_async", self.is_async.data(cx)),
                ("span", self.span.data(cx)),
                ("name", self.name.data(cx)),
                ("generic_params", self.generic_params.data(cx)),
                ("parameters", self.parameters.data(cx)),
                ("return_type", self.return_type.data(cx)),
                ("return_type_span", self.return_type_span.data(cx)),
                ("body", self.body.data(cx)),
                ("export", self.export.data(cx)),
                ("visibility", self.visibility.data(cx)),
            ],
        )
    }
}
impl Data for ExportAttribute {
    fn data(&self, cx: &Context) -> Value {
        record(
            "export_attribute",
            vec![("abi", self.abi.data(cx)), ("symbol", self.symbol.data(cx))],
        )
    }
}
impl Data for StructNode {
    fn data(&self, cx: &Context) -> Value {
        record(
            "struct",
            vec![
                ("name", self.name.data(cx)),
                ("generic_params", self.generic_params.data(cx)),
                ("fields", self.fields.data(cx)),
                ("field_spans", self.field_spans.data(cx)),
                ("methods", self.methods.data(cx)),
                ("visibility", self.visibility.data(cx)),
            ],
        )
    }
}
impl Data for ProtoImplNode {
    fn data(&self, cx: &Context) -> Value {
        record(
            "proto_impl",
            vec![("target", self.target.data(cx)), ("methods", self.methods.data(cx))],
        )
    }
}
impl Data for FunctionSignature {
    fn data(&self, cx: &Context) -> Value {
        record(
            "function_signature",
            vec![
                ("name", self.name.data(cx)),
                ("params", self.params.data(cx)),
                ("return_type", self.return_type.data(cx)),
            ],
        )
    }
}
impl Data for ParameterNode {
    fn data(&self, cx: &Context) -> Value {
        record(
            "parameter",
            vec![
                ("span", self.span.data(cx)),
                ("name", self.name.data(cx)),
                ("param_type", self.param_type.data(cx)),
                ("initial_value", self.initial_value.data(cx)),
            ],
        )
    }
}
impl Data for ExternFunctionNode {
    fn data(&self, cx: &Context) -> Value {
        record(
            "extern_function",
            vec![
                ("name", self.name.data(cx)),
                ("abi", self.abi.data(cx)),
                ("symbol", self.symbol.data(cx)),
                ("params", self.params.data(cx)),
                ("variadic", self.variadic.data(cx)),
                ("return_type", self.return_type.data(cx)),
            ],
        )
    }
}
impl Data for FormatPart {
    fn data(&self, cx: &Context) -> Value {
        match self {
            Self::Literal(p0) => record("literal", vec![("value", p0.data(cx))]),
            Self::Placeholder => record("placeholder", vec![]),
        }
    }
}
impl Data for IncDecKind {
    fn data(&self, _cx: &Context) -> Value {
        match self {
            Self::PreInc => record("pre_inc", vec![]),
            Self::PreDec => record("pre_dec", vec![]),
            Self::PostInc => record("post_inc", vec![]),
            Self::PostDec => record("post_dec", vec![]),
        }
    }
}
impl Data for Expression {
    fn data(&self, cx: &Context) -> Value {
        if let Some(values) = cx.expressions {
            return values
                .borrow_mut()
                .remove(&(self as *const Expression))
                .expect("postorder child value");
        }
        // Convert bottom-up: a bounded source tree must not multiply native
        // stack frames for location wrappers and wire-format containers.
        let values = RefCell::new(HashMap::new());
        let nested = Context { source: cx.source, expressions: Some(&values) };
        let mut pending = vec![(self, false)];
        while let Some((expression, ready)) = pending.pop() {
            if ready {
                let value = expression_record(expression, &nested);
                values.borrow_mut().insert(expression as *const Expression, value);
            } else {
                pending.push((expression, true));
                crate::ast::visit::walk_expression_children(expression, &mut |child| {
                    pending.push((child, false))
                });
            }
        }
        let result = values
            .borrow_mut()
            .remove(&(self as *const Expression))
            .expect("root expression value");
        result
    }
}

fn expression_record(expression: &Expression, cx: &Context) -> Value {
    match expression {
        Expression::Located { value, span } => {
            let mut node = value.data(cx);
            put(&mut node, "span", span.data(cx));
            if matches!(value.unspanned(), Expression::Literal(_)) {
                if let Some(raw) = cx.source.get(span.start..span.end) {
                    put(&mut node, "raw", Value::String(raw.into()));
                }
            }
            node
        },
        Expression::StructLiteral { name, fields } => {
            record("struct_literal", vec![("name", name.data(cx)), ("fields", fields.data(cx))])
        },
        Expression::FunctionCall { name, type_args, args } => record(
            "function_call",
            vec![
                ("name", name.data(cx)),
                ("type_args", type_args.data(cx)),
                ("args", args.data(cx)),
            ],
        ),
        Expression::MethodCall { object, name, type_args, args } => record(
            "method_call",
            vec![
                ("object", object.data(cx)),
                ("name", name.data(cx)),
                ("type_args", type_args.data(cx)),
                ("args", args.data(cx)),
            ],
        ),
        Expression::Await(p0) => record("await", vec![("operand", p0.data(cx))]),
        Expression::Null => record("null", vec![]),
        Expression::Literal(p0) => p0.data(cx),
        Expression::Variable(p0) => record("variable", vec![("name", p0.data(cx))]),
        Expression::Deref(p0) => record("deref", vec![("operand", p0.data(cx))]),
        Expression::AddressOf(p0) => record("address_of", vec![("operand", p0.data(cx))]),
        Expression::BinaryExpression { left, operator, right } => record(
            "binary_expression",
            vec![
                ("left", left.data(cx)),
                ("operator", operator.data(cx)),
                ("right", right.data(cx)),
            ],
        ),
        Expression::IndexAccess { target, index } => {
            record("index_access", vec![("target", target.data(cx)), ("index", index.data(cx))])
        },
        Expression::ArrayLiteral(p0) => record("array_literal", vec![("elements", p0.data(cx))]),
        Expression::Grouped(p0) => record("grouped", vec![("expression", p0.data(cx))]),
        Expression::AssignOperation { target, operator, value } => record(
            "assign_operation",
            vec![
                ("target", target.data(cx)),
                ("operator", operator.data(cx)),
                ("value", value.data(cx)),
            ],
        ),
        Expression::Assignment { target, value } => {
            record("assignment", vec![("target", target.data(cx)), ("value", value.data(cx))])
        },
        Expression::AsmBlock { instructions, inputs, outputs, clobbers } => record(
            "asm_block",
            vec![
                ("instructions", instructions.data(cx)),
                ("inputs", inputs.data(cx)),
                ("outputs", outputs.data(cx)),
                ("clobbers", clobbers.data(cx)),
            ],
        ),
        Expression::FieldAccess { object, field } => {
            record("field_access", vec![("object", object.data(cx)), ("field", field.data(cx))])
        },
        Expression::Unary { operator, expr } => {
            record("unary", vec![("operator", operator.data(cx)), ("expr", expr.data(cx))])
        },
        Expression::Cast { expr, target_type } => {
            record("cast", vec![("expr", expr.data(cx)), ("target_type", target_type.data(cx))])
        },
        Expression::IncDec { kind, target } => {
            record("inc_dec", vec![("kind", kind.data(cx)), ("target", target.data(cx))])
        },
    }
}

impl Data for Literal {
    fn data(&self, cx: &Context) -> Value {
        match self {
            Self::Int(p0) => record("int_literal", vec![("value", p0.data(cx))]),
            Self::Float(_) => record("float_literal", vec![]),
            Self::String(p0) => record("string_literal", vec![("bytes", p0.data(cx))]),
            Self::Bool(p0) => record("bool_literal", vec![("value", p0.data(cx))]),
            Self::Char(p0) => record("char_literal", vec![("value", p0.data(cx))]),
            Self::Byte(p0) => record("byte_literal", vec![("value", p0.data(cx))]),
        }
    }
}
impl Data for Operator {
    fn data(&self, _cx: &Context) -> Value {
        match self {
            Self::Add => record("add", vec![]),
            Self::Subtract => record("subtract", vec![]),
            Self::Multiply => record("multiply", vec![]),
            Self::Divide => record("divide", vec![]),
            Self::Remainder => record("remainder", vec![]),
            Self::GreaterEqual => record("greater_equal", vec![]),
            Self::LessEqual => record("less_equal", vec![]),
            Self::Greater => record("greater", vec![]),
            Self::Less => record("less", vec![]),
            Self::Equal => record("equal", vec![]),
            Self::NotEqual => record("not_equal", vec![]),
            Self::LogicalAnd => record("logical_and", vec![]),
            Self::BitwiseAnd => record("bitwise_and", vec![]),
            Self::LogicalOr => record("logical_or", vec![]),
            Self::BitwiseOr => record("bitwise_or", vec![]),
            Self::Assign => record("assign", vec![]),
            Self::ShiftLeft => record("shift_left", vec![]),
            Self::ShiftRight => record("shift_right", vec![]),
            Self::BitwiseXor => record("bitwise_xor", vec![]),
            Self::LogicalNot => record("logical_not", vec![]),
            Self::BitwiseNot => record("bitwise_not", vec![]),
            Self::Not => record("not", vec![]),
            Self::Neg => record("neg", vec![]),
        }
    }
}
impl Data for AssignOperator {
    fn data(&self, _cx: &Context) -> Value {
        match self {
            Self::Assign => record("assign", vec![]),
            Self::AddAssign => record("add_assign", vec![]),
            Self::SubAssign => record("sub_assign", vec![]),
            Self::MulAssign => record("mul_assign", vec![]),
            Self::DivAssign => record("div_assign", vec![]),
            Self::RemAssign => record("rem_assign", vec![]),
        }
    }
}
impl Data for MatchPattern {
    fn data(&self, cx: &Context) -> Value {
        match self {
            Self::Located { value, span } => {
                let mut node = value.data(cx);
                put(&mut node, "span", span.data(cx));
                node
            },
            Self::Int(p0) => record("int", vec![("value", p0.data(cx))]),
            Self::Ident(p0) => record("ident", vec![("name", p0.data(cx))]),
            Self::Binding(p0) => record("binding", vec![("name", p0.data(cx))]),
            Self::Wildcard => record("wildcard", vec![]),
            Self::Variant { variant_type, case_name, payloads } => record(
                "variant",
                vec![
                    ("variant_type", variant_type.data(cx)),
                    ("case_name", case_name.data(cx)),
                    ("payloads", payloads.data(cx)),
                ],
            ),
        }
    }
}
impl Data for MatchArm {
    fn data(&self, cx: &Context) -> Value {
        record(
            "match_arm",
            vec![
                ("span", self.span.data(cx)),
                ("pattern", self.pattern.data(cx)),
                ("body", self.body.data(cx)),
            ],
        )
    }
}
impl Data for StatementNode {
    fn data(&self, cx: &Context) -> Value {
        match self {
            Self::Print(p0) => record("print", vec![("bytes", p0.data(cx))]),
            Self::PrintFormat { format, args } => {
                record("print_format", vec![("format", format.data(cx)), ("args", args.data(cx))])
            },
            Self::Println(p0) => record("println", vec![("bytes", p0.data(cx))]),
            Self::PrintlnFormat { format, args } => {
                record("println_format", vec![("format", format.data(cx)), ("args", args.data(cx))])
            },
            Self::Input { format, args } => {
                record("input", vec![("format", format.data(cx)), ("args", args.data(cx))])
            },
            Self::Variable(p0) => record("variable", vec![("name", p0.data(cx))]),
            Self::If { condition, body, else_if_blocks, else_block } => record(
                "if",
                vec![
                    ("condition", condition.data(cx)),
                    ("body", body.data(cx)),
                    ("else_if_blocks", else_if_blocks.data(cx)),
                    ("else_block", else_block.data(cx)),
                ],
            ),
            Self::For { initialization, condition, increment, body } => record(
                "for",
                vec![
                    ("initialization", initialization.data(cx)),
                    ("condition", condition.data(cx)),
                    ("increment", increment.data(cx)),
                    ("body", body.data(cx)),
                ],
            ),
            Self::While { condition, body } => {
                record("while", vec![("condition", condition.data(cx)), ("body", body.data(cx))])
            },
            Self::Match { value, arms } => {
                record("match", vec![("value", value.data(cx)), ("arms", arms.data(cx))])
            },
            Self::Import(p0) => p0.data(cx),
            Self::Assign { variable, value } => {
                record("assign", vec![("variable", variable.data(cx)), ("value", value.data(cx))])
            },
            Self::AsmBlock { instructions, inputs, outputs, clobbers } => record(
                "asm_block",
                vec![
                    ("instructions", instructions.data(cx)),
                    ("inputs", inputs.data(cx)),
                    ("outputs", outputs.data(cx)),
                    ("clobbers", clobbers.data(cx)),
                ],
            ),
            Self::Break => record("break", vec![]),
            Self::Continue => record("continue", vec![]),
            Self::Return(p0) => record("return", vec![("value", p0.data(cx))]),
            Self::Expression(p0) => p0.data(cx),
        }
    }
}
impl Data for ImportNode {
    fn data(&self, cx: &Context) -> Value {
        record(
            "import",
            vec![
                ("path", self.path.data(cx)),
                ("alias", self.alias.data(cx)),
                ("selections", self.selections.data(cx)),
                ("visibility", self.visibility.data(cx)),
            ],
        )
    }
}
impl Data for Mutability {
    fn data(&self, _cx: &Context) -> Value {
        match self {
            Self::Static => record("static", vec![]),
            Self::Var => record("var", vec![]),
            Self::Const => record("const", vec![]),
        }
    }
}
impl Data for VariableNode {
    fn data(&self, cx: &Context) -> Value {
        record(
            "variable",
            vec![
                ("name", self.name.data(cx)),
                ("type_name", self.type_name.data(cx)),
                ("initial_value", self.initial_value.data(cx)),
                ("mutability", self.mutability.data(cx)),
                ("visibility", self.visibility.data(cx)),
            ],
        )
    }
}

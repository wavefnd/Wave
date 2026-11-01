// SPDX-License-Identifier: MPL-2.0
//! Frontend-owned scalar enum identities and resolved values.
use super::*;
use conversions::ConversionMode;
use utils::const_int::ConstInt;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

#[derive(Clone, Debug)]
pub struct EnumVariant {
    pub name: String,
    pub value: ConstInt,
    pub span: Option<error::SourceSpan>,
}
#[derive(Clone, Debug)]
pub struct EnumType {
    pub id: TypeId,
    pub name: String,
    pub underlying_type: WaveType,
    pub variants: Vec<EnumVariant>,
}
#[derive(Clone, Debug)]
pub struct EnumConversion {
    pub source: Option<TypeId>,
    pub target: Option<TypeId>,
    pub mode: ConversionMode,
    pub span: Option<error::SourceSpan>,
}

pub(super) fn collect(nodes: &[ASTNode]) -> Vec<EnumType> {
    let mut types = Vec::new();
    for node in nodes {
        let ASTNode::Enum(enumeration) = node.unspanned() else { continue };
        let mut next = ConstInt::zero();
        let variants = enumeration
            .variants
            .iter()
            .map(|variant| {
                if let Some(raw) = &variant.explicit_value {
                    let parsed =
                        lexer::number::IntegerLiteral::parse(raw).expect("validated enum token");
                    next = ConstInt::from_digits(&parsed.digits, parsed.radix)
                        .expect("validated enum value");
                    if parsed.negative {
                        next = next.clone().negated();
                    }
                }
                let value = next.clone();
                next = next.checked_add(&ConstInt::from_u64(1)).expect("validated enum sequence");
                EnumVariant { name: variant.name.clone(), value, span: variant.span.clone() }
            })
            .collect();
        types.push(EnumType {
            id: TypeId(types.len()),
            name: enumeration.name.clone(),
            underlying_type: enumeration.repr_type.clone(),
            variants,
        });
    }
    types
}

pub(super) fn conversions(program: &TypedProgram) -> HashMap<ExpressionId, EnumConversion> {
    let mut conversions = HashMap::new();
    walk_nodes(program.syntax(), &mut |expression| {
        let id = program.expression_id(expression).unwrap();
        let (source, target, mode) = match expression {
            Expression::Cast { expr, .. } => (
                program.enum_type_of(expr),
                program.enum_type_of(expression),
                ConversionMode::Explicit,
            ),
            _ => (
                program.enum_sources.get(&id).copied(),
                program.enum_targets.get(&id).copied(),
                ConversionMode::Implicit,
            ),
        };
        if source != target && (source.is_some() || target.is_some()) {
            conversions.insert(
                id,
                EnumConversion { source, target, mode, span: program.expression_span(id).cloned() },
            );
        }
    });
    conversions
}

#[cfg(test)]
mod tests {
    use super::*;
    fn program(source: &str) -> Result<TypedProgram, String> {
        let tokens = lexer::Lexer::new_with_file(source, "enums.wave").tokenize().unwrap();
        let syntax = parser::parse_syntax_with_spans(&tokens).unwrap();
        TypedProgram::lower(syntax).map_err(|error| error.diagnostic().message.clone())
    }

    #[test]
    fn identity_values_and_conversion_nodes_survive_materialization() {
        let p = program("enum E -> i8 { A = -2, B, C = 127 } fun probe(x: E) -> i32 { var y: E = B; return y as i32; }").unwrap();
        let ty = &p.enum_types()[0];
        assert_eq!(ty.underlying_type, WaveType::Int(8));
        assert_eq!(ty.variants[0].value.to_literal(), "-0x2");
        assert_eq!(ty.variants[1].value.to_literal(), "-0x1");
        assert!(ty.variants.iter().all(|variant| variant.span.is_some()));
        let mut typed = false;
        let mut converted = false;
        walk_nodes(p.syntax(), &mut |expr| {
            typed |= p.enum_type_of(expr) == Some(ty.id);
            converted |= p.enum_conversion_of(expr).is_some();
        });
        assert!(typed && converted);
    }

    #[test]
    fn enum_range_and_last_maximum_are_checked_before_lowering() {
        assert!(program("enum E -> i8 { A = 127 } fun main() {}").is_ok());
        assert!(program(
            "enum E -> u128 { A = 340282366920938463463374607431768211455 } fun main() {}"
        )
        .is_ok());
        for source in [
            "enum E -> i8 { A = 128 } fun main() {}",
            "enum E -> i8 { A = 127, B } fun main() {}",
            "enum E -> u8 { A = -1 } fun main() {}",
        ] {
            assert!(program(source).unwrap_err().contains("out of range"));
        }
    }
}

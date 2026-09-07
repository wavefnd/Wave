//! Conversion of validated frontend numbers to LLVM radix constants.
use inkwell::types::StringRadix;

pub(crate) fn parse_integer(raw: &str) -> Option<(bool, StringRadix, String)> {
    let value = lexer::number::IntegerLiteral::parse(raw)?;
    let radix = match value.radix {
        2 => StringRadix::Binary,
        8 => StringRadix::Octal,
        16 => StringRadix::Hexadecimal,
        _ => StringRadix::Decimal,
    };
    Some((value.negative, radix, value.digits))
}

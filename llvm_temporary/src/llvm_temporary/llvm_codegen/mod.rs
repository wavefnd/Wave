pub mod ir;
pub mod consts;
pub mod format;
pub mod types;
pub mod address;
pub mod legacy;

pub use address::generate_address_ir;
pub use format::{wave_format_to_c, wave_format_to_scanf};
pub use ir::generate_ir;
pub use types::{wave_type_to_llvm_type, VariableInfo};

// legacy (TokenType 기반) - 아직 참조하는 곳 있으면 쓰고, 없으면 지워도 됨
pub use legacy::{create_alloc, get_llvm_type};

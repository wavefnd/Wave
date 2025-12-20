pub mod llvm_temporary;

pub fn backend() -> Option<String> {
    option_env!("WAVE_LLVM_MAJOR").map(|v| format!("LLVM {}", v))
}

//! Exercise C API allocations and their matching LLVM disposal functions.
//! Native Windows SDKs with an integrated allocator must use a compatible CRT.
use inkwell::context::Context;
use inkwell::targets::TargetData;
use llvm_sys::core::{LLVMCreateMessage, LLVMDisposeMessage};
use std::ffi::{CStr, CString};

#[test]
fn llvm_messages_and_target_layouts_share_allocator_ownership() {
    #[cfg(all(target_os = "windows", target_env = "msvc"))]
    assert!(
        cfg!(target_feature = "crt-static"),
        "the pinned Windows MSVC LLVM SDK requires the static CRT, including when RUSTFLAGS is set"
    );
    // Multiple sizes exercise short strings as well as heap-backed LLVM text.
    for length in [0, 1, 15, 16, 127, 4096] {
        let text = CString::new("x".repeat(length)).unwrap();
        for _ in 0..32 {
            // SAFETY: LLVM copies a live NUL-terminated string. The returned
            // allocation is read while live and disposed exactly once by LLVM.
            unsafe {
                let message = LLVMCreateMessage(text.as_ptr());
                assert!(!message.is_null());
                assert_eq!(CStr::from_ptr(message), text.as_c_str());
                LLVMDisposeMessage(message);
            }
        }
    }

    let context = Context::create();
    let expected = "e-m:w-p:64:64-i64:64-i128:128-n32:64-S128";
    for _ in 0..32 {
        let module = context.create_module("allocator-contract");
        let target_data = TargetData::create(expected);
        let layout = target_data.get_data_layout();
        module.set_data_layout(&layout);
        // This explicit destruction is where #493 crashed on native ARM64.
        drop(layout);
        drop(target_data);
        assert_eq!(module.get_data_layout().as_str().to_str().unwrap(), expected);
        let ir = module.print_to_string();
        assert!(ir.to_str().unwrap().contains(expected));
        drop(ir);
        module.verify().unwrap();
    }
}

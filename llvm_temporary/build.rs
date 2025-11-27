use std::env;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let target = env::var("TARGET").unwrap();

    if target.contains("apple-darwin") {
        try_macos_paths();
    } else if target.contains("linux") {
        linux_original();
    } else if target.contains("windows") {
        try_windows_paths();
    } else {
        panic!("Unsupported OS target: {}", target);
    }
}

fn linux_original() {
    println!("cargo:rustc-link-lib=llvm-14");
    println!("cargo:rustc-link-search=native=/usr/lib/llvm-14/lib");

    println!("cargo:rustc-link-lib=stdc++");
    println!("cargo:rustc-link-lib=ffi");
    println!("cargo:rustc-link-lib=z");
    println!("cargo:rustc-link-lib=xml2");

    println!("Using Linux LLVM: /usr/lib/llvm-14");
}

fn try_macos_paths() {
    let candidates = [
        "/opt/homebrew/opt/llvm@14",   // Apple Silicon
        "/usr/local/opt/llvm@14",      // Intel
    ];

    for prefix in candidates {
        let path = PathBuf::from(prefix);
        if path.exists() {
            link_macos(path);
            return;
        }
    }

    panic!("LLVM@14 not found. Install with: brew install llvm@14");
}

fn link_macos(prefix: PathBuf) {
    let lib = prefix.join("lib");

    println!("cargo:rustc-link-search=native={}", lib.display());
    println!("cargo:rustc-link-lib=dylib=LLVM");
    println!("cargo:rustc-link-lib=c++");
    println!("cargo:rustc-link-lib=ffi");
    println!("cargo:rustc-link-lib=z");
    println!("cargo:rustc-link-lib=xml2");

    println!("Using macOS LLVM: {}", prefix.display());
}

fn try_windows_paths() {
    let candidates = [
        r"C:\Program Files\LLVM",
        r"C:\Program Files (x86)\LLVM",
    ];

    for prefix in candidates {
        let path = PathBuf::from(prefix);
        if path.exists() {
            link_windows(path);
            return;
        }
    }

    panic!("LLVM not found. Install from: https://llvm.org/releases/");
}

fn link_windows(prefix: PathBuf) {
    let lib = prefix.join("lib");

    println!("cargo:rustc-link-search=native={}", lib.display());
    println!("cargo:rustc-link-lib=dylib=LLVM");
    println!("cargo:rustc-link-lib=stdc++");
    println!("cargo:rustc-link-lib=ffi");
    println!("cargo:rustc-link-lib=z");
    println!("cargo:rustc-link-lib=xml2");

    println!("Using Windows LLVM: {}", prefix.display());
}

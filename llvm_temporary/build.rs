use std::env;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    // 1) 사용자가 환경변수로 LLVM 경로를 지정한 경우 → 최우선
    if let Ok(prefix) = env::var("LLVM_PREFIX") {
        link_with_prefix(PathBuf::from(prefix));
        return;
    }

    // 2) OS 별 기본 탐색
    let target = env::var("TARGET").unwrap();

    if target.contains("apple-darwin") {
        try_macos_paths();
    } else if target.contains("linux") {
        try_linux_paths();
    } else if target.contains("windows") {
        try_windows_paths();
    } else {
        panic!("Unsupported OS: {}", target);
    }
}

// ------------------------------
// macOS
// ------------------------------
fn try_macos_paths() {
    // Homebrew 설치 경로 (ARM / Intel 모두 지원)
    let brew_paths = [
        "/opt/homebrew/opt/llvm@14",     // Apple Silicon
        "/usr/local/opt/llvm@14",        // Intel
    ];

    for prefix in brew_paths {
        let path = PathBuf::from(prefix);
        if path.exists() {
            link_with_prefix(path);
            return;
        }
    }

    panic!("LLVM@14 not found. Install with: brew install llvm@14");
}

// ------------------------------
// Linux
// ------------------------------
fn try_linux_paths() {
    let candidates = [
        "/usr/lib/llvm-14",
        "/usr/local/lib/llvm-14",
        "/usr/lib/llvm",
    ];

    for prefix in candidates {
        let path = PathBuf::from(prefix);
        if path.exists() {
            link_with_prefix(path);
            return;
        }
    }

    panic!("LLVM 14 not found. Install with your package manager.");
}

// ------------------------------
// Windows (MSYS / LLVM installer)
// ------------------------------
fn try_windows_paths() {
    let candidates = [
        r"C:\Program Files\LLVM",
        r"C:\Program Files (x86)\LLVM",
    ];

    for prefix in candidates {
        let path = PathBuf::from(prefix);
        if path.exists() {
            link_with_prefix(path);
            return;
        }
    }

    panic!("LLVM not found. Install from https://llvm.org/releases/");
}


// ------------------------------
// Linker 설정
// ------------------------------
fn link_with_prefix(prefix: PathBuf) {
    let lib = prefix.join("lib");

    println!("cargo:rustc-link-search=native={}", lib.display());

    // macOS는 llvm-14 대신 LLVM.dylib로 배포됨
    if cfg!(target_os = "macos") {
        println!("cargo:rustc-link-lib=dylib=LLVM");
        println!("cargo:rustc-link-lib=c++");
    } else {
        println!("cargo:rustc-link-lib=static=LLVM");
        println!("cargo:rustc-link-lib=stdc++");
    }

    println!("cargo:rustc-link-lib=ffi");
    println!("cargo:rustc-link-lib=z");
    println!("cargo:rustc-link-lib=xml2");

    println!("Using LLVM prefix: {}", prefix.display());
}

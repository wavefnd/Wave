<div align="center">
<a href="https://www.wave-lang.dev">
<img src="https://wave-lang.dev/img/favicon.ico" alt="Wave Programming Language Logo" width="120" />
</a>
<br/>
<h1>Wave</h1>
<p><strong>Pure Systems Programming Language</strong></p>
<p>
<a href="https://www.wave-lang.dev"><strong>Website</strong></a> ·
<a href="https://www.wave-lang.dev/docs/intro/"><strong>Docs</strong></a> ·
<a href="https://discord.gg/3nev5nHqq9"><strong>Community</strong></a>
</p>
<div>
<a href="https://github.com/LunaStev/Wave/releases">
<img src="https://img.shields.io/github/v/release/LunaStev/Wave?style=for-the-badge&include_prereleases&logo=github&color=5865F2" alt="Latest version"/>
</a>
<a href="https://github.com/LunaStev/Wave/actions/workflows/rust.yml">
<img src="https://img.shields.io/github/actions/workflow/status/LunaStev/Wave/rust.yml?logo=rust&style=for-the-badge&branch=master&label=build" alt="Build Status"/>
</a>
<a href="https://discord.gg/3nev5nHqq9">
<img src="https://img.shields.io/badge/Discord-Join%20Us-5865F2?style=for-the-badge&logo=discord&logoColor=white" alt="Discord" />
</a>
<a href="https://github.com/LunaStev/Wave/blob/master/LICENSE">
<img src="https://img.shields.io/badge/License-LSD License-blue?style=for-the-badge" alt="License"/>
</a>
</div>
</div>
<br/>
<table>
<tr>
<td valign="top" width="60%">
<h3>What is Wave?</h3>
<p>
<strong>Wave</strong> is a pure systems programming language designed for ultimate low-level control. Unlike other languages, Wave has <strong>zero builtin functions</strong> - giving you complete control over your program's behavior.
</p>
<p>
Wave operates in two distinct modes:
</p>
<ul>
<li><strong>Low-level Mode (Default):</strong> Pure compiler with no standard library - perfect for kernel development, embedded systems, and bare-metal programming</li>
<li><strong>High-level Mode (with Vex):</strong> Full standard library ecosystem via the Vex package manager</li>
</ul>
</td>
<td valign="top" width="40%">
<pre>
<code class="language-wave">
// Pure Wave: No builtin functions
fun main() {
    // Direct system calls only
    asm {
        "mov rax, 1"
        "mov rdi, 1" 
        "syscall"
    }
}
</code>
</pre>
</td>
</tr>
</table>

---

> **⚠️ Development Status**  
> Wave is in active development. The compiler is functional but many features are still being implemented. See our [roadmap](https://github.com/LunaStev/Wave/projects) for current progress.

---

## 🚀 Quick Start

### Prerequisites
- LLVM 14+ 
- Linux(Debian etc.) (Windows/macOS support coming soon)

### Install Command (curl)
```bash
curl -fsSL https://wave-lang.dev/install.sh | bash -s -- latest
```

### Your First Wave Program
```bash
# Create a simple program
echo 'fun main() { }' > hello.wave

# Compile in low-level mode (no standard library)
wavec build hello.wave

# Or compile with Vex integration (when available)
wavec build hello.wave --with-vex
```

---

## ✨ Design Philosophy

<table width="100%">
<tr align="center">
<td width="33%">
<h3>🔥 Zero Overhead</h3>
<p>Absolutely no builtin functions or runtime. What you write is what you get - pure machine code with no hidden costs or surprises.</p>
</td>
<td width="33%">
<h3>🎯 Dual Mode</h3>
<p>Choose your abstraction level: raw system calls for maximum control, or rich standard library through Vex package manager.</p>
</td>
<td width="33%">
<h3>⚡ Modern Tooling</h3>
<p>Rust-style error messages, advanced CLI, and seamless integration with the upcoming Vex ecosystem.</p>
</td>
</tr>
</table>

---

## 📚 Documentation & CLI

### Command Reference
```bash
# Display help
wavec help

# Compile a Wave program
wavec build <file> [options]

# Run a Wave program directly
wavec run <file>

# Build options
wavec build program.wave -o output    # Specify output file
wavec build program.wave --debug      # Enable debug mode  
wavec build program.wave -O2          # Optimization level
wavec build program.wave --with-vex   # Enable Vex integration
```

### Error Messages
Wave provides Rust-style error messages to help you debug quickly:

```
error: standard library module 'std::iosys' requires Vex package manager
  --> program.wave:1:8
   |
1  | import("std::iosys");
   |        ^^^^^^^^^^^^
   |
   = help: Wave compiler in standalone mode only supports low-level system programming
   = suggestion: Use 'vex build' or 'vex run' to access standard library modules
```

---

## 🤝 Contributing

Wave is an open-source project and we welcome contributions! Whether you're fixing bugs, adding features, improving documentation, or sharing feedback, every contribution matters.

<table width="100%">
<tr align="center">
<td width="50%">
<h3>🛠️ Development</h3>
<p>Ready to contribute code? Check out our contributing guidelines and development setup.</p>
<a href="https://github.com/LunaStev/Wave/blob/master/CONTRIBUTING.md">
<img src="https://img.shields.io/badge/Contributing%20Guide-333?style=for-the-badge&logo=github" alt="Contributing Guide"/>
</a>
</td>
<td width="50%">
<h3>💬 Community</h3>
<p>Join our Discord community to discuss features, get help, and connect with other Wave developers.</p>
<a href="https://discord.gg/3nev5nHqq9">
<img src="https://img.shields.io/badge/Discord-Join%20Us-5865F2?style=for-the-badge&logo=discord&logoColor=white" alt="Discord" />
</a>
</td>
</tr>
</table>

---

## 💡 Code Examples

### Low-level System Programming
```wave
// Bare-metal: Direct system calls only
fun main() {
    asm {
        "mov ah, 0x0e"
        "mov al, 0x48"
        "int 0x10"
    }
}
```

### High-level with Vex (Future)
```wave
import("std::iosys");

fun fibonacci(n: i32) -> i32 {
    if (n <= 1) {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

fun main() {
    var i: i32 = 0;
    while (i <= 10) {
        println("fibonacci({}) = {}", i, fibonacci(i));
        i += 1;
    }
}
```

### Memory Management
```wave
fun main() {
    var a: i32 = 42;
    var b: i32 = 84;
    
    // Direct pointer manipulation
    var ptr_a: ptr<i32> = &a;
    var ptr_b: ptr<i32> = &b;
    
    // Swap values through pointers
    var temp: i32 = deref ptr_a;
    deref ptr_a = deref ptr_b;
    deref ptr_b = temp;
    
    // a is now 84, b is now 42
}
```

<details>
<summary><strong>📁 More examples in the repository</strong></summary>

Explore the [`test/`](./test/) directory for more examples:
- **Basic syntax**: Variables, functions, control flow
- **Pointers & Memory**: Direct memory manipulation
- **System calls**: Low-level OS interaction  
- **Module system**: Import and organization
- **Inline assembly**: Hardware-level programming

</details>

## 🗺️ Roadmap

Wave is actively developed with a clear roadmap:

- [x] **Core Compiler** - Basic Wave language compilation to LLVM
- [x] **Dual Mode Architecture** - Low-level and high-level compilation modes
- [x] **Advanced Error Handling** - Rust-style diagnostic messages
- [x] **CLI Interface** - Modern command-line experience
- [ ] **Vex Package Manager** - Standard library and package ecosystem
- [ ] **For Loop Implementation** - Complete control flow structures
- [ ] **Advanced Type System** - Enhanced safety and ergonomics
- [ ] **Cross-platform Support** - Windows, macOS, Linux
- [ ] **IDE Integration** - Language server and tooling
- [ ] **Standard Library** - Core functionality via Vex

See our [GitHub Projects](https://github.com/LunaStev/Wave/projects) for detailed progress tracking.

---

## 📄 License & Attribution

Wave is released under the [LSD License](LICENSE).

### Core Contributors
- [@LunaStev](https://github.com/LunaStev) - Creator and Lead Developer

### Acknowledgments
- LLVM Project(Temporary) - Code generation backend
- Rust Community - Inspiration for tooling and error messages
- Systems Programming Community - Feedback and requirements

---

<p align="center">
<a href="https://star-history.com/#LunaStev/Wave&Date">
<img src="https://api.star-history.com/svg?repos=LunaStev/Wave&type=Date" alt="Star History Chart" width="80%">
</a>
</p>

<p align="center">
<strong>Built with ❤️ by the Wave community</strong><br/>
<sub>© 2024 Wave Programming Language • <a href="LICENSE">LSD License</a></sub>
</p>

# Contributing to Wave

<div align="center">
<img src="https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=for-the-badge" alt="Contributions Welcome"/>
<img src="https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=for-the-badge" alt="PRs Welcome"/>
<img src="https://img.shields.io/badge/first--timers--only-friendly-blue.svg?style=for-the-badge" alt="First Timers Only"/>
</div>

Thank you for your interest in contributing to **Wave**! We welcome contributions from developers of all experience levels. Whether you're fixing bugs, adding features, improving documentation, or sharing feedback, every contribution helps make Wave better.

---

## 🚀 Quick Start for Contributors

### Prerequisites
- Rust toolchain (1.70+)
- LLVM 14+
- Git
- Familiarity with systems programming concepts (helpful but not required)

### Setting Up Your Development Environment

1. **Fork and Clone**
   ```bash
   git clone https://github.com/YOUR_USERNAME/Wave.git
   cd Wave
   ```

2. **Build and Test**
   ```bash
   cargo build
   cargo test
   ```

3. **Run Wave Examples**
   ```bash
   ./target/debug/wavec build test/test.wave
   ./target/debug/wavec run test/test.wave
   ```

---

## 📋 Types of Contributions

We welcome various types of contributions:

<table>
<tr>
<td width="50%">

### 🐛 Bug Fixes
- Fix compiler errors or crashes
- Improve error messages
- Resolve parsing issues
- Performance optimizations

### ✨ Features
- Language features (syntax, semantics)
- Compiler improvements
- CLI enhancements
- Tool integrations

</td>
<td width="50%">

### 📚 Documentation
- API documentation
- Tutorial improvements
- Code examples
- Architecture guides

### 🧪 Testing
- Unit tests
- Integration tests
- Language test cases
- Performance benchmarks

</td>
</tr>
</table>

---

## 🎯 Development Guidelines

### Code Style & Standards

Wave follows **Rust standard conventions** with these additional guidelines:

- **Function Naming**: Use `snake_case` for functions and variables
- **Type Naming**: Use `PascalCase` for types and enums  
- **Constants**: Use `SCREAMING_SNAKE_CASE`
- **Bracing**: K&R style (opening brace on same line)

**Example:**
```rust
// ✅ Correct
fn parse_expression(tokens: &[Token]) -> Result<ASTNode, WaveError> {
    if tokens.is_empty() {
        return Err(WaveError::empty_expression());
    }
    // ...
}

// ❌ Incorrect  
fn parse_expression(tokens: &[Token]) -> Result<ASTNode, WaveError>
{
    if tokens.is_empty()
    {
        return Err(WaveError::empty_expression());
    }
    // ...
}
```

### Language Philosophy

Wave is a **pure systems programming language** with these core principles:

- **Zero builtin functions** - Wave compiler provides only syntax support
- **Dual-mode architecture** - Low-level (standalone) vs High-level (with Vex)
- **No runtime overhead** - Direct compilation to machine code
- **Explicit is better than implicit** - No hidden behavior or magic

⚠️ **Important**: Never add builtin functions to the Wave compiler. All standard library functionality must come through external package managers like Vex.

---

## 🏗️ Project Architecture

Understanding Wave's structure will help you contribute effectively:

```
Wave/
├── src/                    # Main compiler CLI
│   ├── main.rs            # CLI interface and commands
│   └── compiler_config.rs # Compiler configuration management
├── front/                 # Frontend (lexer, parser, AST)
│   ├── lexer/            # Tokenization
│   ├── parser/           # Syntax analysis & AST generation
│   └── error/            # Error handling and diagnostics
├── llvm_temporary/       # Backend (LLVM code generation)
├── test/                # Wave language test files
└── docs/               # Documentation
```

### Key Components

- **Lexer** (`front/lexer/`): Tokenizes Wave source code
- **Parser** (`front/parser/`): Builds Abstract Syntax Tree (AST)
- **Error System** (`front/error/`): Provides Rust-style diagnostics
- **Compiler Config** (`src/compiler_config.rs`): Manages compilation modes
- **Standard Library Integration** (`front/parser/src/parser/stdlib.rs`): Handles Vex communication

---

## 🔄 Contribution Workflow

### 1. Choose an Issue

- Browse [open issues](https://github.com/LunaStev/Wave/issues)
- Look for `good first issue` or `help wanted` labels
- Comment on the issue to let others know you're working on it

### 2. Create a Feature Branch

```bash
git checkout -b feature/your-feature-name
# or
git checkout -b fix/bug-description
```

### 3. Make Your Changes

- Write clear, focused commits
- Add tests for new functionality
- Update documentation if needed
- Ensure all tests pass

### 4. Test Your Changes

```bash
# Run the full test suite
cargo test

# Test specific Wave programs
./target/debug/wavec build test/test.wave
./target/debug/wavec run test/test.wave

# Test error handling
./target/debug/wavec build test/invalid.wave  # Should show helpful errors
```

### 5. Submit a Pull Request

- Target the `master` branch (we use trunk-based development)
- Write a clear PR description
- Include screenshots/examples if applicable
- Link related issues

**PR Template:**
```markdown
## Summary
Brief description of your changes.

## Type of Change
- [ ] Bug fix
- [ ] New feature  
- [ ] Documentation update
- [ ] Performance improvement

## Testing
- [ ] Added/updated tests
- [ ] All tests pass
- [ ] Manual testing performed

## Related Issues
Closes #123
```

---

## 🧪 Testing Guidelines

### Test Types

1. **Unit Tests**: Test individual functions/modules
   ```bash
   cargo test -p parser
   cargo test -p error
   ```

2. **Integration Tests**: Test Wave language features
   ```bash
   ./target/debug/wavec build test/test*.wave
   ```

3. **Error Message Tests**: Verify helpful diagnostics
   ```bash
   ./target/debug/wavec build test/invalid_syntax.wave
   ```

### Adding New Tests

- **Language Tests**: Add `.wave` files to `test/` directory
- **Unit Tests**: Add `#[cfg(test)]` modules to relevant source files
- **Error Tests**: Test both valid and invalid Wave programs

---

## 📝 Documentation Standards

### Code Documentation

- **Public APIs**: Must have rustdoc comments
- **Complex Logic**: Add inline comments explaining the "why"
- **Error Messages**: Should be helpful and actionable

**Example:**
```rust
/// Parses a Wave import statement and resolves the module path.
/// 
/// # Arguments
/// * `path` - The import path (e.g., "std::iosys" or "mymodule")
/// * `stdlib_manager` - Optional standard library manager for Vex integration
/// 
/// # Returns
/// * `Ok(Vec<ASTNode>)` - Parsed AST nodes from the imported module
/// * `Err(WaveError)` - Import resolution or parsing error
pub fn local_import(
    path: &str,
    already_imported: &mut HashSet<String>,
    base_dir: &Path,
    stdlib_manager: Option<&StdlibManager>,
) -> Result<Vec<ASTNode>, WaveError> {
    // Implementation...
}
```

### Wave Language Examples

When adding new language features, include examples in multiple contexts:
- Minimal working example
- Real-world use case
- Error cases (what doesn't work)

---

## 🚧 Common Contribution Areas

### High-Priority Areas

1. **Error Messages**: Improve compiler diagnostics
2. **Language Features**: Implement missing syntax (for loops, etc.)
3. **Standard Library Interface**: Vex integration improvements
4. **Documentation**: Tutorial content and API docs
5. **Testing**: More comprehensive test coverage

### Beginner-Friendly Tasks

- Fix typos in documentation
- Add more Wave language examples
- Improve error message text
- Add unit tests for existing functionality
- Update outdated comments

### Advanced Tasks

- LLVM backend improvements
- New language feature implementation
- Vex package manager integration
- Cross-platform support (Windows)
- Performance optimizations

---

## 💬 Getting Help

### Community Resources

- **Discord**: [Join our community](https://discord.gg/Kuk2qXFjc5)
- **GitHub Issues**: Ask questions with the `question` label
- **Discussions**: [GitHub Discussions](https://github.com/LunaStev/Wave/discussions)

### Finding Mentorship

- Look for issues labeled `mentorship available`
- Ask in Discord for guidance on larger contributions
- Review existing PRs to understand contribution patterns

---

## 🏆 Recognition

Contributors are recognized in several ways:

- Listed in the project's contributors list
- Mentioned in release notes for significant contributions
- Recognition in our Discord community
- Special contributor badges for ongoing contributions

---

## ⚖️ Code of Conduct

We follow the [Contributor Covenant](https://www.contributor-covenant.org/) to ensure a welcoming environment for all contributors. Please:

- Be respectful and inclusive
- Focus on constructive feedback
- Help newcomers learn and contribute
- Assume good intentions

---

## 📄 License

By contributing to Wave, you agree that your contributions will be licensed under the [Mozilla Public License 2.0](LICENSE).

---

<p align="center">
<strong>Thank you for contributing to Wave! 🌊</strong><br/>
<sub>Every contribution, no matter how small, helps build the future of systems programming.</sub>
</p>
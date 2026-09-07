# Wave Alpha language contract, revision 0

[alpha-0.ebnf](alpha-0.ebnf) defines the source grammar implemented by this
frontend. [tokens.tsv](tokens.tsv) inventories **every** `TokenType` variant,
including internal compatibility variants, reserved syntax and removed syntax.
This is a language contract revision, not a compiler release announcement.
Changes to accepted syntax must update the grammar, inventory and conformance
fixtures together. Correctness fixes to existing productions do not require a
new language version.

## Reading the grammar

Quotes denote source terminals; commas concatenate; `|` chooses; brackets are
optional; braces repeat zero or more times. `? ... ?` is a lexical condition
specified here. Longest-token matching applies before parsing. For example,
`--x` is decrement, while `- -x` is two unary negations. A token cannot be split
by whitespace or comments. Types such as `i64` are single tokens, despite their
spelling being factored in `integer-type`. Generic closing chevrons are split
contextually from `>>` by the generic parser.

Whitespace, CRLF/LF newlines, `//` comments and `/* ... */` comments separate
tokens and do not terminate statements. Block comments do not nest. All local
variables, expression statements, returns, breaks, continues and I/O statements
require `;`, even immediately before `}`. Block constructs do not require `;`;
standalone `asm` accepts one optional trailing `;`. Empty statements are not
part of this grammar. A failed statement never permits the parser to skip its
remaining tokens and resume silently.

Assignment is right associative. Other binary operators and repeated casts
associate left to right. Precedence from low to high is assignment, `||`, `&&`,
`|`, `^`, binary `&`, equality, comparison, shift, addition, multiplication,
`as`, prefix operators, postfix operators and primary expressions. Address-of
uses the same `&` token as bitwise AND. `deref` is the dereference operator;
prefix `*` is not supported. Conditions reject assignment and mutation in the
semantic pass, even when their expression syntax is otherwise valid.

An unparenthesized match subject ends at the first `{`; parenthesize subjects
containing a record literal. Match arm bodies are blocks. Integer and enum
patterns match scalar subjects. Variant patterns may recursively bind payloads;
a bare name in a payload position binds a variable, while a qualified name
selects a case. `_` is a wildcard. Arms permit a single optional `,` or `;`
separator. Duplicate arms, duplicate bindings and nonexhaustive variant matches
are semantic errors.

## Names and declarations

Identifiers start with a Unicode Alphabetic scalar or `_` and continue with
Unicode Alphabetic or Numeric scalars or `_` (Rust's Unicode predicates).
No normalization is performed: different scalar sequences are different names.
Combining marks outside these predicates are not identifier characters.
Keywords in the inventory cannot be identifiers. `bool`, `void`, `ptr` and
`array` are contextual type spellings represented by identifier tokens.
Reserved syntax (`module`, `class`, `is`, `xnand`, `~^`, `!&`, `!|`, `?`, `??`,
`?:`) has no accepted production. `let` and `mut` declarations were removed;
use `var`. Ranges, for-in loops, propagation operators, unsafe blocks, function
pointer types, slices, destructuring, expression-valued if/match and match
guards are not added by this revision.

`pub` controls module visibility and is separate from the C ABI export
attribute. `main` must remain private and nongeneric. Imports use
`import("path" as alias);` or `import("path")::{name, other};`; aliases and
selections cannot be combined. A public import requires explicit selections.
`extern(c)` and `extern(system)` declare foreign functions; `export(c)` and
`export(system)` define foreign entry points. These headers accept an optional
string symbol. Extern parameters may omit their names, and a variadic marker
must be last. Exported functions cannot be generic.

The `#[target(...)]` attribute occupies its own source line and applies to one
following declaration. Keys are `arch`, `os`, `env`, and `abi`, without repeated
keys. The same filtering applies to imported sources and to variant declarations.
Inactive declarations and attributes are replaced by spaces that preserve byte
lengths and newline positions. Stacked target attributes and attributes inside
bodies are outside this contract.

Required parameters precede default parameters. Defaults are literal values
(including signed numbers and null), not arbitrary constant expressions. Their
values are checked against the declared type even if the function is never
called. Parameter, payload and enum lists allow a trailing comma; calls, array
literals and explicit type-argument lists do not. Generic parameter lists must
be nonempty and contain unique names. `void` and `!` are restricted to return positions. `!` declares a function that
cannot return: it must end in a provably endless loop or a call to another
never-returning function (or an asm block declaring `clobber("noreturn")`). Explicit return statements are rejected in such functions.
A never-returning call is allowed as a statement and terminates that control-flow
path; this revision does not introduce bottom-type coercions in value expressions.
`const` and `static` belong at top level. Field names, bindings, signatures,
return coverage, visibility and ABI compatibility have additional semantic
checks; syntactic acceptance is not a promise that a program is well typed.

## Numbers and text

Integers accept decimal, binary (`0b`/`0B`), octal (`0o`/`0O`) and hexadecimal
(`0x`/`0X`). Every prefix requires digits. Exactly one underscore may appear
between digits of the same numeric component. Decimal floats require a
fractional part with digits on both sides of the point or a decimal exponent:
`1.0`, `1e3`, `1_000.5e-2`. Exponent signs are allowed; number suffixes and
hexadecimal floats are not. Thus `1.`, `.5`, `1__2`, `0x_1`, `0b102`, `1e+` and
`1u32` are rejected. Invalid adjacent digit/identifier text is diagnosed as one
malformed number, not separate valid tokens. A decimal point directly after a
number belongs to that numeric token; use parentheses for postfix access on a
numeric primary.

Integer spelling and sign are preserved until a target type is known. Supported
signed/unsigned widths are 8, 16, 32, 64, 128, 256, 512 and 1024. Decimal signed
literals must fit the signed range; nondecimal positive literals may spell the
full-width bit pattern. Negative signed minimum values are supported. Unsigned
initializers cannot be negative. Explicit casts retain the existing conversion
rules. Array lengths accept integer literals fitting `u32`. `isz` and `usz`
remain symbolic in the AST, then resolve to the selected target's pointer
width **before** semantic analysis and generic specialization: wasm32 uses 32;
the supported native 64-bit targets and wasm64 use 64. Unsupported numeric
width spellings are errors, not user-defined types.

Floating literals are converted once to finite IEEE binary64 values; a literal
with an expected f32 type must also fit finite binary32. Overflow is diagnosed;
underflow may round to zero. Integer-to-float constant conversion accumulates
exact integer digits before one rounded conversion. The shared implementation
is `front/lexer/src/number.rs`; semantic checking and LLVM constants use it too.

Strings contain Unicode scalar values and use UTF-8 when emitted. Supported
string escapes are `\\`, `\"`, `\n`, `\t`, `\r`, and `\xNN`; the last denotes
the scalar U+00NN. Character literals contain exactly one value in 0..255,
matching Wave's 8-bit `char`; their escapes additionally include `\'`.
`\0` is not an escape: use `\x00`. Physical newlines and unknown or incomplete
escapes are rejected. Escaped strings retain their original source spelling
and byte span separately from the decoded value.

## Source provenance and conformance

Locations use half-open UTF-8 byte ranges and one-based line/column coordinates;
columns count Unicode scalars, not terminal cells or UTF-8 bytes. Parser-selected
name spans focus declaration diagnostics. Imports retain their original file
paths. Generic instances and inserted defaults retain definition locations and
record their expansion reason. Synthetic nodes without a physical origin have
no span, represented as null, rather than an invented location on line 1.

`parse_syntax_with_spans` is the compiler's source-preserving entry point.
`parse_syntax_only` is a compatibility API for consumers that explicitly discard
locations. Typed HIR exposes stable declaration/statement, expression and pattern
IDs with source-span accessors. The diagnostic renderer uses these spans directly;
it does not search for matching text in the source again.

[fixtures.tsv](fixtures.tsv) maps production families to positive and negative
source files in [fixtures](fixtures/). `grammar_contract.rs` executes both sides
and verifies that every token kind has exactly one inventory row referring to a
real production. Lexical examples must produce the documented token kind.
`numeric_contract.rs`, `alpha_frontend.rs` and `source_spans.rs` add detailed
numeric, block, target-filter and provenance regressions. Linux CI runs the
workspace tests, so frontend conformance runs independently of LLVM test filters.

Run locally with `cargo test --locked -p lexer -p parser --jobs 2`; run compiler
and backend integration with `cargo test --locked --workspace --all-targets --jobs 2`.

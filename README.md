> [!WARNING]
> This README is heavily AI-generated and will need manual work later on.

# FHIA Programming Language

<!--toc:start-->
- [FHIA Programming Language](#fhia-programming-language)
  - [Goals](#goals)
  - [Status](#status)
  - [Building](#building)
  - [Usage](#usage)
  - [Language Reference](#language-reference)
    - [Declarations](#declarations)
    - [Types](#types)
    - [Expressions](#expressions)
    - [Operators](#operators)
    - [Comments](#comments)
  - [Diagnostics](#diagnostics)
<!--toc:end-->

## Goals

- [x] Compile to native code
- [ ] Compile to WASM
- [x] Statically typed
- [ ] Type inference
- [ ] Turing complete
- [ ] Self-hosted

## Status

Early-stage compiler that translates a small, statically-typed language to native binaries via LLVM.
The pipeline is fully wired end-to-end: lex → parse → type-check → const-eval → codegen → link.

Currently implemented:

- Global declarations (`let`, `let mut`, `const`)
- All numeric types (`i8`–`i128`, `u8`–`u128`, `isize`, `usize`, `f32`, `f64`)
- Integer literals (decimal, binary, octal, hex) and float literals (decimal, scientific notation)
- Unary negation (`-`)
- Type cast expressions (`as`)
- Forward references between declarations (resolved via topological sort)
- Constant folding for `const` declarations and const-evaluable `let` declarations
- Cross-compilation via `--target`
- Rich error diagnostics (ariadne) with source spans

Not yet implemented: functions with arguments, binary operators, control flow, `bool`, `char`, `str`, arrays, pointers.

## Building

Requires a Rust nightly toolchain and LLVM 22.

```sh
cargo build --release
```

## Usage

```sh
fhia [OPTIONS] [INPUT]
```

| Option | Default | Description |
|---|---|---|
| `[INPUT]` | `test.fhia` | Source file to compile |
| `-o`, `--output <FILE>` | `a.out` | Output binary path |
| `--target <TRIPLE>` | host triple | Cross-compilation target |
| `--parser` | off | Print the parsed AST |
| `--typer` | off | Print the typed AST |
| `--llvm-ir` | off | Print the generated LLVM IR |

Every program must declare a `main` of an integer type. Its value becomes the process exit code.

```fhia
let x: i64 = 42
let main: i32 = x as i32
```

## Language Reference

### Declarations

```fhia
let <name>: <type> = <expr>
let mut <name>: <type> = <expr>
const <name>: <type> = <expr>
```

All declarations are at top level. The type annotation is mandatory. `const` requires a
compile-time-evaluable expression; `let` and `let mut` may reference non-const values.

Forward references are allowed: declarations are evaluated in dependency order, not source order.

### Types

| Type | Description |
|---|---|
| `iN` (N = 1–128) | Signed integer of exactly N bits (e.g. `i8`, `i24`, `i128`) |
| `uN` (N = 1–128) | Unsigned integer of exactly N bits (e.g. `u1`, `u32`, `u128`) |
| `isize`, `usize` | Pointer-sized signed/unsigned integers |
| `f32`, `f64` | Floating-point numbers |

### Expressions

- **Integer literals**: `42`, `0b1010`, `0o17`, `0xFF` — type is inferred from the declaration
- **Float literals**: `3.14`, `1.`, `.5`, `1.0e3`, `1.0e-3`
- **Identifiers**: reference to a previously declared name
- **Grouping**: `(expr)` or `{expr}` — both are semantically equivalent
- **Negation**: `-expr`
- **Cast**: `expr as <type>` — left-associative, chains as `42 as u32 as i64`

### Operators

#### Arithmetic

| Operator | Description | Precedence | Associativity |
|---|---|---|---|
| `-` | unary minus | 2 | right |

#### Cast

| Operator | Description | Precedence | Associativity |
|---|---|---|---|
| `as` | type cast | 2 | left |

Cast semantics follow Rust: truncation, sign extension/zero extension, and float↔integer
conversions are all well-defined and explicit.

### Comments

```fhia
// line comment

/* block
   comment */
```

## Diagnostics

Errors are reported with source spans using [ariadne](https://github.com/zesterer/ariadne).
Multiple errors are collected and reported together rather than stopping at the first failure.

Detected error classes:

- Invalid token
- Malformed declaration (missing name, colon, type, or `=`)
- Unclosed delimiter (`(`, `{`)
- Duplicate declaration
- Undefined variable
- Type mismatch between declared type and expression type
- Integer literal out of range for declared type
- Assigning a signed (negated) literal to an unsigned type
- Missing `main` declaration
- `main` declared with a non-integer type
- Non-const expression in a `const` declaration
- Invalid cast operand (e.g. casting `()`)
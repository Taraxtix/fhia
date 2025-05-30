# FHIA Programming language

## Goals

- [ ] Compile to native code
- [ ] Compile to WASM
- [ ] Statically typed
- [ ] Type inference
- [ ] Turing complete
- [ ] Self-hosted

## Targeted

### Syntax

Everything can be either:

- An expression `<expr>` that evaluates to a value

  An expression followed by a semicolon is a statement evaluates to `()`.

  A block is also an expression, it is a sequence of expressions between `{` and `}`, every expression in a block except the last expression must evaluate to `()` or `!`.

- A function definition :

```fhia
let <name> [pattern*] [: <return_type>] = <expr>
```

Where:

- `<name>` is the name of the function (must not be a reserved keyword or function)
- `[pattern*]` is 0 or more patterns that matches the arguments of the function. There are 3 types of patterns:
  - `<ident> [: <type>]` is a variable name of type `<type>` (if not specified, the type is inferred if possible)
  - `literal` A literal value
  - `_` is a wildcard pattern
  If no patterns are specified, the function takes no arguments and is essentially a constant variable
- `<return_type>` is the type of the return value of the function (if not specified, the type is inferred if possible)
- `<expr>` is the expression that evaluates to the return value of the function

A function can have multiple definitions, that matches different patterns.

Additionally, you can define a mutable variable with `let mut <name> = <expr>`.

### Types

- `i8`, `i16`, `i32`, `i64`, `i128` (signed integers)
- `u8`, `u16`, `u32`, `u64`, `u128`
- `size` (unsigned integers)
- `f32`, `f64`, `f128` (floating point numbers)
- `bool` (boolean)
- `char` (character)
- `str` (strings)
- `()` (unit)
- `!` (never)
- `[T, size]` (array of elements of type `T` and size `size`)
- `&const T` (constant pointer/reference to an element of type `T`)
- `&mut T` (mutable pointer/reference to an element of type `T`)

### Operators

All binary operators will use infix notation (`<expr> <op> <expr>`) but can be used as prefix by surrounding the operator with parentheses (`(<op>) <expr> <expr>`). Furthermore, all binary operators will have a assignment variant (`OP=`) (`x OP= y` is equivalent to `x = x OP y`) which all have a precedence of 14 and associativity to the left.

Operators with lower precedence are evaluated first. If two operators have the same precedence.

precedences and associativity are based on : <https://en.cppreference.com/w/c/language/operator_precedence>

#### Arithmetic

- `X++` postfix increment (precedence: 1 | associativity: left)
- `X--` postfix decrement (precedence: 1 | associativity: left)
- `++X` prefix increment  (precedence: 2 | associativity: right)
- `--X` prefix decrement  (precedence: 2 | associativity: right)
- `-` unary minus         (precedence: 2 | associativity: right)
- `*` multiplication      (precedence: 3 | associativity: left)
- `/` division            (precedence: 3 | associativity: left)
- `%` modulo              (precedence: 3 | associativity: left)
- `+` addition            (precedence: 4 | associativity: left)
- `-` subtraction         (precedence: 4 | associativity: left)

#### Logical

- `!` logical not   (precedence: 2  | associativity: right)
- `&&` logical and  (precedence: 11 | associativity: left)
- `||` logical or   (precedence: 12 | associativity: left)

#### Comparison

- `<` less than               (precedence: 6 | associativity: left)
- `>` greater than            (precedence: 6 | associativity: left)
- `<=` less or equal than     (precedence: 6 | associativity: left)
- `>=` greater or equal than  (precedence: 6 | associativity: left)
- `==` equals to              (precedence: 7 | associativity: left)
- `!=` not equals to          (precedence: 7 | associativity: left)

#### Bitwise

- `~` bitwise not   (precedence: 2 | associativity: left)
- `<<` left shift   (precedence: 5 | associativity: left)
- `>>` right shift  (precedence: 5 | associativity: left)
- `&` bitwise and   (precedence: 8 | associativity: left)
- `^` bitwise xor   (precedence: 9 | associativity: left)
- `|` bitwise or    (precedence: 10 | associativity: left)

#### Other

- `=` assignment (precedence: 14 | associativity: left)
- `*` constant dereference (precedence: 2 | associativity: left)
- `.` member access/function call (precedence: 1 | associativity: left)

### Reserved functions

#### Debug printing

```fhia
dbg <expr>
```

Prints the value of the expression to standard output with a newline.

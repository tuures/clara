## Clara
Clara is an experimental/WIP statically-typed FP/OO programming language that compiles to JavaScript.

**Features currently implemented:**
- Static, polymorphic, single-rooted, class-based (nominal) type system with type inference
- Literals for strings, integers, functions, and unit type.
- Functions are (monomorphic) first-class values. Function application is binary operation (all functions are unary).
- `sum(1, 2)` is a shorthand for `sum.apply(1,2)`
- Classes have values and methods as members. Classes and methods can take type parameters.
- Prefix for keywords to avoid conflicts with identifier names

**Planned features:**
- ES6 Emitter that generates readable code
- Mutually recursive definitions
- "case classes" / ADTs
- Pattern matching and destructuring
- Standard library
- Little syntactic sugar such as definition shorthand for curried functions (multiple parameters)

### Philosophy and Goals
- Syntax: Do not make me think
  - Provide small orthogonal language features, do not provide to many ways to do a same thing.
  - Refactoring should require minimal amount of changes on the syntax level, keep related language features syntactically similar.
  - Keep syntax to the minimal, do not reserve words from being used as identifier.

- Semantics: Be safe but practical
  - Prefer immutability but provide unsafe escapes with warning labels.
  - When unsure, default to safety.

## Clara

DOCUMENT OUTDATED

Clara is an experimental/WIP statically-typed FP/OO programming language that compiles to JavaScript.

**Features currently implemented/WIP:**
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
- Tail call optimization

### Philosophy and Goals

Simplicity, Safety, Batteries included.

Features should be orthogonal. There should be only one obvious way to do something.

- Syntax: Do not make me think
  - Syntax should be easy-to-read and also as easy-to-write as possible without compromising the first.
  - Provide small orthogonal language features, do not provide to many ways to do a same thing.
  - Refactoring should require minimal amount of changes on the syntax level, keep related language features syntactically similar.
  - Keep syntax to the minimal, do not reserve words from being used as identifier.

- Semantics: Be safe but practical
  - Prefer immutability
  - Provide unsafe escapes with warning labels.
  - When unsure, default to safety.

## Design decisions

- Functions have (only) one parameter. Return another function or take tuples if multiple parameters are needed.
- No operators/precedence. All values are objects which have members. `1 + 2 * 3` stuff is common in tutorials but not on real application code. Just use parenthesis.
- No multiple inheritance. Can sum types be used for `Mouse extends Animal with Device` -> `Animal = Mouse | ...; Device = Mouse | ...` ?

## Influencers

- Scala: syntax, FP/OOP unification
- Typescript
- OCaml: all functions are unary, call without parens
- Reason
- Smalltalk: no operator precedence, operators are methods, all values are objects

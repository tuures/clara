# Vision

Approachable functional programming language for the JS generation.

## Strategy – how it should look like
- syntax similar to JS (i.e. not like Haskell, ML, Lisp)
- combines functional and object oriented (think Scala)
- statically typed with good developer experience
- minimal runtime, compiles fast, outputs readable code that works with JS ecosystem
- practicality over purity: a well designed tool instead of a piece of art
- compile implementation which is easy to read and refactor, ultimately bootstrapped

## Philosophy – how it should feel like

Simplicity, Safety, Batteries included.

Features should be orthogonal (non-overlapping, and composable).
In case of clear dichotomies, provide both and let user decide on the trade-off.

Syntax: Do not make me think
  - Syntax should be easy-to-read and also as easy-to-write as possible without compromising the first.
  - Refactoring should require minimal amount of changes on the syntax level. (Keep related language features syntactically similar.)
  - Keep syntax to the minimal, do not reserve words from being used as identifier.

Semantics: Be safe but practical
  - Prefer immutability
  - Provide unsafe escapes with warning labels.
  - When unsure, default to safety.

# Vision

Approachable functional programming language for the JS generation.

## Principles

Simplicity, Safety, Batteries included.

Features should be orthogonal (non-overlapping, and composable).
In case of clear dichotomies, provide both options and let user decide on the trade-off.

Syntax: Do not make me think
  - Avoid using same special characters/keywords for different things so that minimal amount of visual context is needed to determine semantics.
  - However, keep the syntax to minimal, do not reserve words from being used as identifier. Put visual focus to the content instead of filler words.
  - Keep related language features syntactically similar so that refactoring requires minimal amount of changes on the syntax level.

Semantics: Be safe but practical
  - Prefer immutability
  - Provide unsafe escapes with warning labels.
  - When unsure, default to safety.

## How it should look like in practice
- provide a compilete language (instead of just nice syntax for JS)
  - syntax type system similar to JS/TS (i.e. not like Haskell, ML, Lisp), but rethought ground up for functional programming
  - own standard library
- combines functional and object oriented (think Scala)
  - statically typed with good developer experience
  - practicality over purity: a well designed tool instead of esoteric piece of art
- minimal runtime, compiles fast, outputs readable code that works with JS ecosystem
- compiler implementation which is easy to read and refactor, ultimately bootstrapped
- should feel like functional programming in JS without fighting

## Details

More [details in PLAN.md](PLAN.md).

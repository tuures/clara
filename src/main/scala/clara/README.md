## Goal
Simplicity. Features should be orthogonal. There should be only one obvious way to do something. Syntax should be easy-to-read and also as easy-to-write as possible without compromising the first.


## Design decisions

- Functions have (only) one parameter. Use return another function or take tuples if multiple parameters are needed.
- No operators/precedence. All values are objects which have members. `1 + 2 * 3` stuff is common in tutorials but not on real application code. Just use parenthesis.
- No multiple inheritance. Can sum types be used for `Mouse extends Animal with Device` -> `Animal = Mouse | ...; Device = Mouse | ...`


## Compared to other languages

Scala: syntax, FP/OOP unification
OCaml: all functions are unary, call without parens
Smalltalk: no operator precedence, operators are methods, all values are objects

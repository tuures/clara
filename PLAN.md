# Design decisions and planned features

should feel like functional programming in JS without fighting

- unified type system
  - all types are located in single type lattics which has top and bottom types
  - "magical types" like integers and strings are injected as opaque types which do not have any inspectable structure
  - user can define more opaque types to bridge platform types into language

- provide both structural and nominal typing
  - structural subtyping for records etc
  - nominal typing via nominally wrapping structured types
  - support both tagged (runtime variants) and untagged unions (ad-hoc union types)

- define methods separately from the structure of the type
  - provides clear distinction between data and function
  - no "classes with inheritance" — only data is subject to subtyping
  - methods can only be defined for nominal types
  - helps to avoid recursion in definitions
  - ? offer extension ad-hoc methods (on arbitrary types) ?

- provide local type inference
  - similar to Scala, and TypeScript
  - keeps typechecker implementation straightforward and fast

- prefix for keywords to avoid conflicts with identifier names

- all functions take just one (value) parameter
  - return another function or use tuples to mimic multiple parameters
  - do not provide any shorthand syntax for multiple parameters
  - avoids much hassle in typechecker, but also helps keep syntax more explicit
  - argument can be passed without parenthesis when desirable
  - ? automatics adaptation when emitting JS ?

- polymorphism, but not higher-kinded
  - higher-kinded type parameters complicate typechecker without significant practical benefit
  - offer simple parametric polymorphism like in TypeScript

- pattern matching and destructuring
  - pattern matching is actually piecewise-defined function
  - destructuring (patterns) should work the same everywhere (function paramter, assignment, pattern matching)
  - no other built-in control flow syntax (e.g. if-else)

- syntax should read left to right:
  1. primarily via using methods `object.method arg`
    - some methods take functions: `[1,2,3].map(v => ... )`. in this case we can either:
      -  use predefined methods of the value v: `foos.map(_.bar)`, OR
      -  use ad-hoc functions of data-last style: `foos.map(zot)`, where zot: ... => M[Foo] => M[B],
        for example: [1,2,3].map(pow(2)), where pow: Int => Seq[Int] => Seq[Int]
  2. use ad-hoc functions of data-last style: `object..func arg`

- equality should be structural and built-in (behaves well and cannot be overridden)

- shorthand syntax for defining enumerations/ADTs
  - desugars into nominal + union types

- syntax for sequence literals (like array in JS), or use vararg/tuples (like Scala)?

- operator syntax for methods
  - no operator precedence, or should/could it be user configurable? (e.g. + vs *)

- ? provide `sum(1, 2)` as shorthand for `sum.apply(1,2)` like in Scala ?

- ? recursive defintions like LinkedList<N> = {item: N, next: LinkedList<N>} ?

- ? tail call optimization ?

- provide module system

- annotate source code with compiler instructions for controlling emitted code

- emit JS code, later maybe TS

- provide rich standard library (instead of just nice syntax for JS)

- documentation
  - syntax comparisons with Scala, TypeScript, ...

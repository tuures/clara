# Ideas to consider

https://2022.stateofjs.com/en-US/opinions/#top_currently_missing_from_js

## module support

start from the "entry point" file
source -> AST -> dependencyAnalyzer -> list of filenames
starting from first dependency repeat parsing + dependency analysis recursively, on every step append parsed AST
as node into a dependency tree
use post-order DFS to analyze the tree

module can either be a regular clara module (.clara) or a native JS module (.js + .d.clara)

all value and type declarations from the top level block of a module are "exported"

imports can appear any level of the code

::import foo/math // imports everything from math module as math.*
::import foo/math pi // imports pi from math as pi


## operators

operators are methods with non-letter name
// a b c
// if b = foo (letters)
// (a foo) c <=> a.apply(foo).apply(c)
// if b = * (non-letters)
// (a.*(c))

examples:
`foo.bar` -> member (foo, member `bar`)
`foo .bar` -> member (foo, member `bar`)
`foo . bar` -> operator (foo, member `.`, apply `bar`)
`foo asd bar` -> apply (foo, apply asd, apply bar)


## std lib

- list.soleHead return the head if list has just one item (combined head + assert length == 1)

Option https://github.com/scala/scala/blob/2.13.x/src/library/scala/Option.scala

`??` as shorthand for getOrElse: returnsOpt() ?? fallbackValue


## pattern matching, control structures, equality

if-else, ::match

x < 5 then 1 else 0

x < 5 ::match { true => 1, false => 0}

--

pattern matching block
https://github.com/topshell-language/topshell#sum-types-and-pattern-matching

{| <pat1> => expr1 | <pat2> => expr2 | ...}

--

piecewise-defined function:
  (| true => 1 | false => 2)

error if match is not exhaustive
warn if the match is too wide when directly assgining the piecewise func to something where narrower type is expected


--

if (admin) 1 else 0

admin ? 1 : 0

admin .ifElse(() => 1, () => 0)
admin @ #(True => 1, False => 0)
admin @ #(?true => , 0)

if admin printWarning

?(
  isAdmin & isRoot => 0
  isDiscounted => 1
  2
)
?(isAdmin & isRoot => 0, isDiscounted => 1, 2)

### reverse call / pipe syntax
dir @ sort @ uniq

users @(
  | Bar =>
  | Foo =>
)

1 @ Euro


pipe syntax

1 | repeat 3 | _.length


### no-space call precedence?

a b(c)  <=>  a (b c)  not: (a b) c
a(b) c  <=>  (a b) c


### equality and friends
=~ (equivalence, for example rational numbers 2/4 =~ 1/2)
== equality (structural equality)
=_= identity (reference equality)

- Do not define equality for floats
- How to define equality for structural types if not all values have equality?


## Repl

https://60devs.com/executing-js-code-with-nodes-vm-module.html


## Linting / compiler errors

Warn about unused values (and types?)

## Constructor calls

Constructor is a first class function

- `::new T` expression returns the function

### implicit ::new

`A` means

1) value `A`
2) if value `A` is not found but there is type `A`, then `::new A`

this is better than:

`f x` means
1) if there's a function value named f in scope -> call f with x
2) else if there's value f in scope with member (property or method) `apply` -> call f.apply with x
3) else if there's type f -> construct new f from x

because if the logic is bound to call, then it would not simply work in something like `.map(A)`

but the apply rule is also useful (points 1, 2, but not 3)


## Defining types

replace ::declare ::type / ::declare ::methods with ::trait (determines how methods should be implement elsewhere)

::trait Seq<T> {
  headOption: Maybe<T>
  length: Int
  // should we allow also method implementations?:
  headUnsafe: T = headOption.unwrapUnsafe
}

::type FooSeq<T> = Any // TODO should it be : or := instead of =

::methods FooSeq<T> : Seq<T>, SomeOtherTrait {
  // must implement head here
}

when value is assigned to a trait type, the methods of the actual type is injected into the runtime value for dynamic dispatch

how to define static methods? how to declare them in traits

Rust's supertraits seem useful

::comp Boolean {
  parse = (s: String) => ::match {
    "True" => True
  }
}

how it should look like for user:

foo: Foo = Foo.from(bar) // Foo is type, accessing static method

Bar = {
  ... // akin to Scala's object
}

local type variables (reads "where"):
(Option[(A, A)]; A = SomeLongComplexType)


## Tuples and records

- warn if record literal has extraneous fields when directly assigning to something where narrower type is expected

### tuples as nested pairs

a <=> (a, ())
(a, b) => (a, (b, ()))
(a, b, c) <=> (a, (b, (c, ())))
etc.

this should simply implementation as analyzer only has to deal with pairs/2-tuples


### {} == () ?

mathemathically it would be fair to consider empty record and 0-tuple the same but in order to prevent accidents,
they are not assignable to each other


## Records deep copy
how to make this composable? function returning keyword?

::type Address = {
  country: String
  zipCode: String
}
::type User = {
  email: String
  address: Address
}
user = ::new User {
  email = ''
  address = ::new Address {
    country = 'FI'
    zipCode = '00100'
  }
}
updatedUser = ::copy user {
  address.zipCode = 12345
  email = 'foo@bar.invalid'
}
// property list notation:
// {
//   address.zipCode = 12345
//   email = 'foo@bar.invalid'
// }


## TypeScript output

interface Foo {
  x: number
  y: number
}

const Foo$Methods = {
  area: (a: Foo) => a.x * a.y
}

const foo1: Foo = {
  x: 1,
  y: 2,
}

// const $claraType = Symbol('$claraType')

// interface $ClaraObject {
//     [$claraType]: Symbol
// }

// interface Foo extends $ClaraObject {
//   x: number
//   y: number
// }

// const Foo$Type = Symbol('Foo')

// const Foo$Methods = {
//   area: (a: Foo) => a.x * a.y
// }

// const foo1: Foo = {
//   [$claraType]: Foo$Type,
//   x: 1,
//   y: 2,
// }


## Tail call elimination

tail call elimination using @[tailCall]


## Nullary methods:

::methods Boolean {
  not: => Boolean
}


## defaults in patterns (`??`)

`=` in patterns is reserved for renaming/capturing/constants:
- `{foo = bar}` — capture field `foo` as `bar` (rename)
- `{foo = FooConstant}` — match field `foo` against constant
- `{foo = 5}` — match field `foo` against literal
- `{foo}` — shorthand for `{foo = foo}` (capture as same name)

So `=` cannot also mean "default value". Use `?? valueExpression` instead:

```clara
setup = {foo: String, bar: String ?? "default"} => ...
// accepts {foo = "hi"}, bar defaults to "default"

(firstname, lastname, score ?? 0) = someValue
```

The corresponding type could be presented as:
```clara
{foo: String, optional: String ??, anotherOptional: String ??}
```

`??` is justified to avoid the explosion of union types with multiple optional fields and to avoid the overuse of option monad in APIs

```clara
{foo: String, optional1: String ??, optional2: String ?? }
// vs
{foo: String, optional1: String, optional2: String} | {foo: String, optional1: String} | {foo: String, optional2: String} | {foo: String}
```


## Potentially useful resources

List of languages to look at for inspiration: Scala, Dotty, TypeScript, Reason/Ocaml, Rust, Swift, Nim, Python, PureScript, JS++


https://www.cl.cam.ac.uk/~jdy22/papers/lightweight-higher-kinded-polymorphism.pdf
Jeremy Yallop and Leo White University of Cambridge

https://www.cl.cam.ac.uk/~sd601/thesis.pdf
Algebraic subtyping

http://nischalshrestha.me/docs/cross_language_interference.pdf

http://learnyouahaskell.com/making-our-own-types-and-typeclasses

https://www.reddit.com/r/rust/comments/4jh8hv/question_about_rust_vs_haskell_type_systems/

http://www.inquisition.ca/en/info/gepsypl/rules.htm

https://proglangdesign.net/


### documentation approach

https://diataxis.fr/

https://passo.uno/seven-action-model/

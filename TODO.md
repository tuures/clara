# Done

- construct patterns (`Some a`, `Option.Some s`) on boxed types (a tag test, `$value?.$type === 'Option.Some'`) and tagged types (no tag: in a case the argument pattern's runtime test has to tell the type apart from what the earlier cases left, Check 1 of plans/enum2.md); in parameters and value defs they unwrap (`(Name n) => ...`); the userland Option fixture (`option-desugared`) passes
- a case is checked for runtime type tests against what the earlier cases left, not the whole matched type
- a type arg a construct pattern's matched type doesn't give is an unknown type (a fresh type param), not `!`; a constructor's type has type params of its own, not its con's
- parser: a line comment no longer consumes its newline, which made the next line a space call argument
- methods are typed exactly (PLAN.md): a method's type args are matched from the value's type, once for all alternatives of a union (`Some<String> | None` gives `A = String`), `!` where nothing gives them; the alias is transparent, its methods are only on the types it expands to
- member lookup falls back to what a tagged or boxed type wraps (`.$value` of a boxed one, `Terms.MemberSelection.unboxCount`), after its own methods
- type args of a call are inferred through aliases (their expansion), unions (alternative by alternative) and tagged, boxed and opaque types (their type args); a param found as several types gets their union, or their intersection where it's a function's parameter (`TypeInterpreterImpl.findSubstitutions`)
- `::methods` of an alias are methods of each type it expands to (`Option`'s on `Some` and `None`)
- member lookup through an alias (on its expansion), a union (the same member in every alternative, typed with the union of their types) and an intersection (the member of one part)
- `::methods<A> receiver { ... }` and `::declare ::methods<A> { ... }` inside `::extend`, selected from values of tagged, boxed and opaque types; the receiver is any parameter pattern; each method is its own const (`Pair$$methods$left`); the standalone `::methods` sections are gone
- `::new Bar` is the raw constructor of a tagged or boxed type; a companion's own `apply` (a smart constructor) replaces the generated one
- error type (`Types.ErrorType`): what fails to analyze is the error type, so each pass runs also after errors in the earlier ones and the errors of a whole block are reported at once, without follow-up errors
- each pass of a scope is its own object (`NamesPass`, `TypesPass`, `ValuesPass`); every declaration of a scope gets its ids once, before the passes (`IndexedBlock.of`)
- several `::extend X` in one scope are one companion
- messages of a program are in source order, warnings and errors together
- parser: nested calls and parens no longer take exponential time (left-factored rules)
- parser: the empty string `""` parses as a literal and as a pattern
- parser: a guard pattern always has a pattern on its left (`_ ? isValid`, not `? isValid`)
- parser: a block can have just one item (`(foo;)`, `(;foo)`, `(foo = 1)`)

# Roadmap

Each step is a vertical slice (parser -> analyzer -> emitter) validated by an e2e fixture; keep existing tests green.

0. acceptance fixtures first: write the target `.clara` files with expected `.messages`/`.output`; they fail until done
   - userland Option: `option-desugared` (`::alias` + `::extend` by hand) is in `e2e/`, `option` (the `::enum` sugar, step 12) in `e2e-pending/`
   - still to write: Color
1. ~~groundwork~~ done; `InstanceId` comes with typeclasses (14)
2. ~~scope passes (names, types, values), forward references, recursive functions and types~~ done; the follow-ups are under Analyzer follow-ups
3. namespaces, as vertical slices (parser -> analyzer -> emitter, each with an e2e fixture):
   - ~~3a. `::namespace` and qualified names~~ done. Follow-ups:
     - show type names qualified in messages (`Color.Red`, from the con's `definedInNs`), and both `definedAt`s when two names in one message display the same (`Red` vs `Red`, or the same qualified name from two modules); later DX: also the name as written (e.g. an import alias) when it differs from the con's
     - union rule: two different types with the same qualified name can't be members of one union (`Red | Red` from two definitions); compares qualified names, which also makes `$type` unambiguous within a union. Probably needs `isAssignable`-style checks to move to `TypeInterpreter`
     - a dotted name in a pattern is a constant whatever its case (`math.Pi`, plans/namespacing.md), today a lowercase head parses as a capture
     - `Foo = {}` should define a value `Foo` (binding context), today it parses as a named constant pattern ("Unknown value `Foo`"); decide nested cases like `(True, x) = pair`
   - ~~3b. `::extend`~~ done. Follow-ups:
     - emitter: a call of a tagged type's constructor (`Id$$new(x)`) can be just `x`; the const is still needed where the constructor is used as a function value (`xs.map(Id)`)
     - extending a singleton: its value and its companion namespace share the name (see the `Foo.x` lookup tier under Open decisions)
   - ~~3c. `::methods` inside `::extend` with the method lookup on tagged, boxed and opaque types~~ done. Follow-ups:
     - a method is known from its definition on (a `::declare`d one in its whole scope): methods can't refer to each other before their definitions or to themselves. A method body is only evaluated when the method is called, so inside method bodies any method of the scope (all its sections) with a signature can be used, like a function run; needs the names pass to declare methods ("needs a type annotation to be used before its definition") and their signatures in the types pass. Outside method bodies a method stays usable only after its section (its const isn't initialized before)
     - unused methods aren't reported
     - the methods of a type defined in a nested block are lost outside it (they're in the `Env`), a value of the type can still get out of the block
4. member lookup, as vertical slices:
   - ~~4a. alias -> expansion, union per alternative (record fields and methods), intersection~~ done. Follow-ups:
     - different members of one name in the alternatives of a union (e.g. a field and a method, or two types' own methods) need a `$type` dispatch, isn't supported yet
     - a member of several parts of an intersection isn't supported yet (see the open decision)
   - ~~4b. fan-out of `::methods` from an alias to its members~~ done. Follow-ups:
     - the types an alias expands to don't need to be defined in the same scope as it yet (no ad-hoc extension of foreign types, plans/namespacing.md), comes with modules
     - `::methods` of an alias that applies only to some instantiations of a type (`::alias UnitBox: Box<()>`, `::alias Twin<A>: Pair<A, A>`) isn't supported yet, methods apply to every instantiation of a type; behaviour for some instantiations is expected from typeclass instances (14), a separate mechanism (Rust's `impl Point<f32>`, Swift's constrained extensions) would overlap with them
     - `::declare ::methods` of an alias isn't supported yet: fanning out in the types pass would expand aliases whose structures aren't registered yet, or alias cycles not rejected yet. Fan out in a step after the types pass and `rejectUnsupportedRecursion` (a list of pending fan-outs collected in the `Env`), so every `::declare` stays usable anywhere in its scope
   - ~~4b+. methods typed exactly, a union's type args found once, the alias transparent~~ done
   - ~~4c. tagged/boxed fallback to wrapped~~ done
5. ~~patterns: construct patterns + pattern validity (Check 1)~~ done. Follow-ups:
   - a tagged construct pattern that can't tell its type apart gets the general "needs to test the type of the value at runtime" message; it could name the alternatives it can't tell apart and say a tagged type has no tag
   - a construct pattern of a generic type needs the type it matches (`(Some a) => a` alone is "Could not infer type")
   - the unknown type of a type arg a construct pattern's matched type doesn't give is shown with the con's param name, so `<A>(x: A) => x @ #(Option.Some a => (a: A), _ => x)` reports "Type `A` is not assignable to type `A`"; show it distinctly (`?A`, or "the unknown type arg `A` of `Some`"); the same for any two type params of one name in a message (see the 3a follow-up on `definedAt`s)
   - Check 1 compares what the argument pattern's runtime test accepts (`TypeInterpreterImpl.testedType`) with the other alternatives by `disjoint`, so `testedType` must never be smaller than what the test really accepts: a boxed construct pattern tests only the con, not the type args (its tested type wraps what the argument pattern tests); a tuple pattern also accepts a sequence of that length once sequences exist (11); a literal counts as its whole type, which only matters once there are literal types
6. modules and imports (plans/module-system.md): AST support for a module, import syntax in the parser, multi-file compilation, output as ES modules (E2eSpec runs them as modules), the runtime helper module (Emitter follow-up)
   - `::new` only inside the defining module, today it's allowed wherever the type is visible
   - no ad-hoc extension of foreign types: the types an alias expands to are defined in its module (4b follow-up)
7. prelude, a module imported implicitly: `Boolean`, `Option` (the desugared form until `::enum`), the opaque `Int`, `Float` and `String`, so programs stop declaring `::opaque String` themselves
8. runtime type tests: opaque types declare their test (`typeof x === 'string'`); type patterns in cases (`n: Int` on `Int | String`) and tagged construct patterns use them when the type the earlier cases left needs it, so a tagged `String` and a tagged `Int` can be told apart (Check 1 then counts the test); only when the type is testable (not functions or type params). Until then such cases are an error (`TypeInterpreterImpl.needsTypeTest`)
   - what can reach a case is computed by dropping the alternatives the earlier cases cover (`casesCheck.remainingTypes`), an approximation: an alternative covered only in part stays. Subtracting step by step instead (`matched -- covered1 -- covered2 ...`, a `--` difference type) gives the exact remaining type per case, reachability becomes "case overlaps the remaining type" (cheaper than checking against the growing union of earlier cases when the remaining type simplifies, unions of singletons/constructors), and what remains after the last case names the missing cases in the non-exhaustive error (e.g. `Some<Blue>`); the alternative is a union expansion whose results can be shown (`expandUnions` results must not be shown)
9. structural equality: matching a value against another value, so equality is pattern matching; `==` can be an operator on top of it (with 10)
   - a runtime helper compares tuples, records, tagged and boxed values (`$type` included) by structure, singletons and primitives with `===`; opaque types declare their equality like their runtime test (`===` by default)
   - functions don't have equality: comparing them, or values containing them, is an error
   - a named constant pattern compares by structure (today `===`, so a constant tuple or record never matches)
10. numbers and operators: negative literals, operator syntax for methods (`a + b`, `::declare ::methods` with `binaryOperator` on the prelude's opaque types), Int, Float and String operations; negative literals and binary `-` are designed together (`a -1` vs `a - 1`)
11. sequences: array literals and a sequence type with a few methods (`contains` uses 9) in the prelude
   - an immutable sequence is covariant (`Seq<String>` is a `Seq<String | Int>`), opaque types need declared variance (Analyzer follow-ups on variance)
   - tuples vs arrays: a tuple pattern means "an array of length n" (Pattern matching follow-up)
12. ::enum as sugar (start with always boxed, tagged when the alternatives can be told apart, using 8) -> userland Option fixture passes
   - an enum stays an alias of a union; generic calls need re-sugaring of unions to aliases (plans/namespacing.md)
13. JS interop: calling and exposing JS functions of several parameters (Clara functions take one)
14. typeclasses: first-order with member fan-out, then constraints (dictionary passing), static members (`Int.empty`), HKT with re-sugaring
   - question: should inference place an argument type in a union parameter type by checking which alternatives it fits (`isAssignable` with the solved type params as the error type), like TypeScript? Today `findSubstitutions` uses the alternatives that find candidates and falls back to the bare params, so `A | Int` against `Int` gives `A = Int` instead of leaving `A` free. Instance resolution will lean on inference more, decide with it

# Follow-ups

## Pattern matching

- record patterns: record rules in `disjoint` (exhaustive `isDisjointFromRecords`); runtime test checks own properties (`Object.hasOwn`), plus object/array/`$type` checks only when the remaining matched type needs them
- or-patterns, guard patterns, `??` default patterns (`{foo: String ?? "x"}` accepts `{} | {foo: String}`)
- Option without boxing (JS interop): `None` as tagged host undefined/null, opaque types declare their runtime test, `A -- B` difference types for the `Some` payload
- tuples vs arrays (with array literals): both are JS arrays, so a tuple pattern means "an array of length n" (matches `Array<T>` too) and `disjoint` mustn't separate them (`isDisjointFromTuples`); "testable at runtime" needs its own check, separate from `disjoint` (also records vs boxed); consider `(A, B) <: ReadonlyArray<A | B>`
- duplicate literal cases (`#(1 => ..., 1 => ...)`) aren't reported as unreachable since literals cover nothing (would need literal types)
- emitter: skip the `Array.isArray`/length checks of tuple cases when the matched type is known to be a tuple of that length (needs the matched type in `Terms.Piecewise`)
- emitter: string patterns mixing captures and other parts (`"$(a)-$(b)"`) are `???` in `JsEmitter.emitCaseMatch`; need a runtime matcher where each capture takes the shortest text before the next literal part (not needed for Option/enums)

## Analyzer

- error type follow-ups:
  - an unsupported recursive type stops the values of its block (`rejectUnsupportedRecursion`), as `TypeInterpreterImpl` wouldn't terminate on it: could be replaced with the error type in the structures
  - the values of an `::extend` without a type aren't analyzed, nothing in it has been declared
  - a lambda in a record field gets no parameter type from an expected error type (`x: Bad = {f: a => a}` is also "Could not infer type"), as the expected field types come from `Types.Record`
  - a value expression stops at its first error (`a(nope1)(nope2)` reports one), only value defs and block contents go on
- recursion follow-ups:
  - a run of function defs is broken by anything else, also a type def or a `::declare` in between, which evaluate nothing at their position (type defs come first in the block term): could be let through if it turns out to matter
  - a function used only by itself isn't reported unused (its self-reference counts as a use)
  - recursive aliases (`::alias L: () | (Int, L)`): the alias unwraps in `TypeInterpreterImpl` (`isAssignable`, `disjoint`, `expandUnions`) would need to be remembered like the tagged and boxed ones
- a lambda with a tuple parameter doesn't get its type from the expected type (`g: (String, String) => String = (_, b) => b` is "Could not infer type")
- bidirectional checking, steps in this order (Dunfield-Krishnaswami's rules without their inference variables):
  1. a lambda's body doesn't get the expected result type, only its parameter gets the expected parameter type (`lambdaTerm`), so `add: Int => Int => Int = a => b => a` is "Could not infer type" at `b`: check the body against the result type of the expected `Func`, also the case bodies of `#()`; then a block passes its expected type to its last expression
  2. a polymorphic function isn't assignable to a monomorphic one (`f: Int => Int = id` with `id = <A>(a: A) => a` is "Type `<A>A => A` is not assignable to type `Int => Int`"), as `isAssignable` compares functions only with the same number of type params: instantiate the type params of the source with `findSubstitutions` before comparing; also lets an intersection callee keep its polymorphic members (`callResultType` drops them)
  3. polymorphic callee: the expected argument type contains the callee's type params, e.g. a lambda argument's parameter is typed `A` (`map(xs, p => p.name)` is "`name` is not a member of type `A`"): type the argument in two phases, like TypeScript:
     - synthesize the parts of the argument that aren't literal lambdas or `#()`, and find the type args they give
     - check the lambdas against the parameter type with those type args substituted
     - infer the type args from the whole argument, as today

     a lambda whose parameter type comes only from another lambda's result (`pipe(x => 1, y => y)`) needs a round per lambda
  4. `valueExprTermExpecting` treats the expected type as a hint and each caller runs `expectAssignable`, so an error points at the whole expression (`p: (Int, Int) = (1, "x")` quotes the tuple, not `"x"`): split it into checking, which ends in `expectAssignable` at the position of the part that doesn't fit, and synthesis. Needs step 3 first, as a polymorphic callee's parameter type and the matched type that `argumentFirstCallTerm` passes to `#()` are hints, not types to check against
- type arg inference treats every type arg of a tagged, boxed or opaque type as covariant (`TypeInterpreterImpl.findSubstitutions`), so `Sink<()> | Sink<((), ())>` with `::tagged Sink<A>: A => ()` is "no type arguments fit" where Scala infers `A = () & ((), ())`; needs each con's variance, probed from its structure (like TypeScript) or from variance annotations
- type args aren't inferred from the expected type (`xs: List<String> = List(())`), only from the argument; a param the argument doesn't give is `!`, which fits only where the con is covariant in it
- "`m` can't be selected from `X`, no type arguments of `::methods<A>` fit all of its alternatives" doesn't name the types that conflict
- phantom type params of tagged/boxed: error until variance annotations exist, then infer variance per type con (see TODO in Types.AssignableImpl)
- merge identical alternatives in Types.Union (keeps alias nodes, e.g. `Option<Int> | Option<Int>`)
- a boxed type of an empty type (`Some<!>`) has no values but isn't treated as empty, so `Option<!>` isn't assignable to `None` (the other direction is): drop the empty alternatives of a union before checking it (like `!` in `Types.Union`, but it needs the structures, so in `TypeInterpreterImpl`'s normalizing), and treat a boxed type of an empty type as empty like `hasEmptyMember` does for records and tuples; only when the wrapped type itself is empty, so recursive boxed types aren't unwrapped
- a `::declare` has a term (`Terms.ValueDecl`) but a `::declare ::methods` section has none: align them, either drop `Terms.ValueDecl` (only its name, the emitter ignores it) or give declare sections a term too
- `An.traverse(as)(f)` for the `An.seq(as.map(f))` of the analyzer (about ten places)
- `typeParams` as a member of `TypeCon` (`Nil` for singletons and params, until HKT params have an arity), so that `TypeInterpreter.instantiate`, `TypeCons.toSource` and `pseudoInstantiate` don't match on the con kind just to get them

## Emitter

- a block that needs an IIFE as a statement after another statement crashes at runtime (`console.log("a")` followed by `(_ = "x"; console.log("b"))` emits `console.log("a")\n(() => {...})()`, a call of the previous statement's result, as statements have no semicolons)
- `::declare` inside a namespace keeps its `definedInNs` like any member, but the emitter then names it `Host$log`, which nothing defines (the host's name is `log`): the emitter should emit a declared value under its declared name, or an annotation overrides the emitted name (see the next item)
- `::declare` of a reserved name (e.g. `::declare undefined: ...`) is broken: references get the `$` suffix but the JS global has none; later: an annotation declaring the JS name explicitly
- runtime module with tree-shakeable helpers (`import * as $clara from ...`), not duplicated per module; with modules (Roadmap 6): where it lives, output as ES modules, E2eSpec needs `--input-type=module`. Until then emitted code reaches globals through `globalThis`

## Other

- message levels (`Message` has a TODO for it): when a compile fails, the CLI prints its warnings as `ERROR:` too

# Open decisions

- operator precedence: none, fixed (`*` before `+`), or declared per operator (10)
- equality typing: matching a value against one of a disjoint type is already an error ("can never match"), and so is `Some(1).contains("a")` (methods are typed exactly), but a generic `==<A>(a: A, b: A)` would infer `A = Int | String` for `1 == "a"`; `==` should keep the pattern's disjointness error (like Scala 3's multiversal equality, TypeScript's "no overlap") (9)
- singleton `Foo.x` merges companion members, inherent methods and instance members into one tier (duplicates within `::extend Foo` are definition-time errors, others lookup-time ambiguity)
- are instances imported only via companions or from the whole module top level
- revisit: instance derivation for records/tuples/unions (no explicit `::impl` for structural types)
- revisit: intersection member lookup when several parts have the member

# Laters

- `e :< T` checks that `e` is assignable to `T` but keeps `e`'s own type (like TypeScript's `satisfies`)
- performance: `TypeInterpreterImpl.wrappedType` substitutes the type args on every unwrap of an alias/tagged/boxed type (`isAssignable`, `disjoint`, `expandUnions`, ...) and `isAssignableWrapper` rescans with `mentionsParam` on each comparison; cache per `(con, typeArgs)` if it shows up, the expansion stays lazy (recursive types)
- performance: `TypeRecursionCheck` is quadratic in a scope's wrapper type defs (`reachable` from each con, then `filter`/`distinct` for the groups, `findCycle`'s `path.exists`); Tarjan's strongly connected components (one depth-first walk) and a `Set` for the path would make it linear. Kept as is while it's simpler to read and scopes are small

# Done

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
   - userland Option is in `e2e-pending/`: `option-desugared` (`::alias` + `::extend` by hand) and `option` (the `::enum` sugar, step 6)
   - still to write: Color
1. ~~groundwork~~ done; `InstanceId` comes with typeclasses (9)
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
     - a method is known from its definition on (a `::declare`d one in its whole scope): the methods of a section can't refer to each other before their definitions or to themselves; like a function run, needs their signatures in the types pass
     - unused methods aren't reported
     - the methods of a type defined in a nested block are lost outside it (they're in the `Env`), a value of the type can still get out of the block
4. member lookup, as vertical slices:
   - ~~4a. alias -> expansion, union per alternative (record fields and methods), intersection~~ done. Follow-ups:
     - different members of one name in the alternatives of a union (e.g. a field and a method, or two types' own methods) need a `$type` dispatch, isn't supported yet
     - a member of several parts of an intersection isn't supported yet (see the open decision)
   - 4b. fan-out of `::methods` from an alias to its members (one method, so no runtime dispatch; `smoketest.clara`'s `Boolean.not` waits for it); removes `aliasWrappedMethodTargets` (non-exhaustive match warning)
   - 4c. tagged/boxed fallback to wrapped (emit `.value` for boxed)
5. patterns: construct pattern (`Some a`, the `???` in `PatternAnalyzer`) + pattern validity (Check 1 in plans/enum2.md: the runtime test must tell constructors apart, a tag test like `$value.$type === 'Some'`)
   - `Y Z` in pattern position is always a construct pattern
   - qualified construct patterns (`Option.Some s`)
   - a boxed recursive type can't be taken apart until construct patterns exist
6. ::enum as sugar (start with always boxed, disjointness-based tagged later) -> userland Option fixture passes
   - an enum stays an alias of a union; generic calls need re-sugaring of unions to aliases (plans/namespacing.md)
7. small real programs: what an everyday program needs, each with an e2e fixture
   - Int, Float and String operations as methods on opaque types, with operator syntax (`a + b`); fix parsing negative numbers, revisit operator precedence (+/*)
   - array literals and a sequence type with a few methods
   - a minimal prelude (`Boolean`, `Option`, the opaque base types), so programs stop declaring `::opaque String` themselves
   - structural equality
   - JS interop: calling and exposing JS functions of several parameters (Clara functions take one)
8. modules and imports: AST support for a module, import syntax in the parser
   - `::new` only inside the defining module, today it's allowed wherever the type is visible
9. typeclasses: first-order with member fan-out, then constraints (dictionary passing), static members (`Int.empty`), HKT with re-sugaring

# Follow-ups

## Pattern matching

- record patterns: record rules in `disjoint` (exhaustive `isDisjointFromRecords`); runtime test checks own properties (`Object.hasOwn`), plus object/array/`$type` checks only when the remaining matched type needs them
- or-patterns, guard patterns, `??` default patterns (`{foo: String ?? "x"}` accepts `{} | {foo: String}`)
- name missing cases in non-exhaustive errors: needs a union expansion whose results can be shown (`expandUnions` results must not be shown); alternatively compute the remaining type step by step (`matched -- covered1 -- covered2 ...`, needs `--` difference types): what remains after the last case is what's missing (e.g. `Some<Blue>`), and reachability becomes "case overlaps the remaining type", cheaper than checking against the growing union of earlier cases when the remaining type simplifies (unions of singletons/constructors)
- runtime type tests for type patterns in cases (`n: Int` on `Int | String`), only when the matched type isn't already a `T` and `T` is testable (not functions or type params); opaque types declare their test (`typeof`). Until then such cases are an error (`PatternAnalyzer.needsTypeTest`)
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
  - a generic recursive list: a constructor call's type args aren't inferred from a union parameter (`List(("a", rest))` with `::tagged List<A>: () | (A, List<A>)` infers `A` as `!`, `Types.findSubstitutions` doesn't look into unions)
- a lambda with a tuple parameter doesn't get its type from the expected type (`g: (String, String) => String = (_, b) => b` is "Could not infer type")
- polymorphic callee: the expected argument type contains the callee's type params, e.g. a lambda argument's parameter is typed `A` (see FIXME in Types.findSubstitutions)
- phantom type params of tagged/boxed: error until variance annotations exist, then infer variance per type con (see TODO in Types.AssignableImpl)
- merge identical alternatives in Types.Union (keeps alias nodes, e.g. `Option<Int> | Option<Int>`)
- a `::declare` has a term (`Terms.ValueDecl`) but a `::declare ::methods` section has none: align them, either drop `Terms.ValueDecl` (only its name, the emitter ignores it) or give declare sections a term too
- `An.traverse(as)(f)` for the `An.seq(as.map(f))` of the analyzer (about ten places)
- `typeParams` as a member of `TypeCon` (`Nil` for singletons and params, until HKT params have an arity), so that `TypeInterpreter.instantiate`, `TypeCons.toSource` and `pseudoInstantiate` don't match on the con kind just to get them

## Emitter

- a block that needs an IIFE as a statement after another statement crashes at runtime (`console.log("a")` followed by `(_ = "x"; console.log("b"))` emits `console.log("a")\n(() => {...})()`, a call of the previous statement's result, as statements have no semicolons)
- `::declare` inside a namespace keeps its `definedInNs` like any member, but the emitter then names it `Host$log`, which nothing defines (the host's name is `log`): the emitter should emit a declared value under its declared name, or an annotation overrides the emitted name (see the next item)
- `::declare` of a reserved name (e.g. `::declare undefined: ...`) is broken: references get the `$` suffix but the JS global has none; later: an annotation declaring the JS name explicitly
- runtime module with tree-shakeable helpers (`import * as $clara from ...`), not duplicated per module; with modules (Roadmap 8): where it lives, output as ES modules, E2eSpec needs `--input-type=module`. Until then emitted code reaches globals through `globalThis`

## Other

- message levels (`Message` has a TODO for it): when a compile fails, the CLI prints its warnings as `ERROR:` too

# Open decisions

- singleton `Foo.x` merges companion members, inherent methods and instance members into one tier (duplicates within `::extend Foo` are definition-time errors, others lookup-time ambiguity)
- are instances imported only via companions or from the whole module top level
- revisit: instance derivation for records/tuples/unions (no explicit `::impl` for structural types)
- revisit: intersection member lookup when several parts have the member

# Laters

- `e :< T` checks that `e` is assignable to `T` but keeps `e`'s own type (like TypeScript's `satisfies`)
- performance: `TypeInterpreterImpl.wrappedType` substitutes the type args on every unwrap of an alias/tagged/boxed type (`isAssignable`, `disjoint`, `expandUnions`, ...) and `isAssignableWrapper` rescans with `mentionsParam` on each comparison; cache per `(con, typeArgs)` if it shows up, the expansion stays lazy (recursive types)
- performance: `TypeRecursionCheck` is quadratic in a scope's wrapper type defs (`reachable` from each con, then `filter`/`distinct` for the groups, `findCycle`'s `path.exists`); Tarjan's strongly connected components (one depth-first walk) and a `Set` for the path would make it linear. Kept as is while it's simpler to read and scopes are small

# Done

- warn on unused value definitions
- Fix isAssignable for polymorphic funcs — `<A>A => A` should be assignable to `<B>B => B`
- union/intersection assignability: rule order (`A & B` wasn't assignable to itself), lazy head normalization (distribute over unions, unwrap aliases, merge records, disjoint members to Bottom), `disjoint`, expanding nested unions against union targets (tuples, records, tagged/boxed wrapped types; max 25 combinations), function intersections (`#()`) against functions, records/tuples with empty members, invariant phantom type params of tagged/boxed
- constructors collapse single member unions/intersections (`#()` with one case is a plain function)
- patterns: refutable `#()` cases (`walkCase`): must be able to match some value and narrow the matched type, assignments/parameters stay irrefutable (`walkAssignment`); tuple patterns on unions of tuples
- matched type flows into `#()`/lambda callees (`x @ #(...)`) and into `#()`/lambdas nested in tuple/record arguments and annotated record fields
- "can never match" errors quote the pattern's source (`Pos.sourceText`)
- literal patterns (`1`, `1.5`, `"a"`) are errors in assignments and function parameters since they can fail to match (no literal types, revisit if added); a string pattern of a single capture stays allowed
- `#()` exhaustiveness (error) and reachability (warning) using the type each pattern covers (`PatternAnalyzer.coveredType`: literals cover nothing, captures/wildcards/type patterns their type, singletons themselves, tuples only if every element covers); without an expected type the matched type is what the cases can match together
- emitter: piecewise cases with capture and tuple patterns (each case compiles to conditions and bindings, `JsEmitter.emitCaseMatch`); type patterns are kept as `Terms.PatternAs`
- emitter: Clara names colliding with JS reserved words or `undefined`/`globalThis`/`arguments`/`eval` get a `$` suffix (`class$`, `NameMangler.valueName`); compiler names start with `$` (Clara names can't contain `$`); emitted code reaches JS globals through `globalThis`
- emitter: wildcards and named constants in assignments/parameters bind nothing (`JsAst.IgnoredPattern`: array hole, `()` parameter, bare expression for `_ = e`), so several wildcards no longer collide and `_ => ...` no longer crashes; one `emitBindingPattern` for assignments and parameters
- groundwork (Roadmap 1): `Uniq` split into `TypeConId`/`DefId` (asg/Uniq.scala), `UniqInfo` keyed by id kind; Env entries `EnvValue`/`EnvTypeCon` share one `DefId` per declaration, usage tracked by `DefId`: a singleton used only as a type or only as a value is used, an unused one is reported once, a wrapper type used only as its constructor is used
- expression `e: T` is a declaration like `x: T = e`: the value's type becomes `T` (`Terms.ValueAs`) and `T` is the expected type for `e` (e.g. `(n => n): Int => Int`)

# Roadmap (namespacing / enum / typeclasses)

Each step is a vertical slice (parser -> analyzer -> emitter) validated by an e2e fixture; keep existing tests green.

0. acceptance fixtures first: write the target `.clara` files (userland Option, Color, smart constructor) with expected `.messages`/`.output`; they fail until done
1. ~~groundwork~~ done (see Done); `ValueEntry`/`NamespaceRef` and `NsId` come with namespaces in 3, `InstanceId` with typeclasses in 8
2. block phases: types and namespaces first, then ::extend, then values (forward references, recursive types)
3. ::extend: companion statics, ::methods (replaces standalone method sections), `Bar` = `Bar.apply`, `::new`
4. member lookup: tagged/boxed fallback to wrapped (emit `.value` for boxed), alias -> expansion, union per alternative
5. patterns: dot-constant, construct pattern (`Some a`) + pattern validity (Check 1 in plans/enum2.md: the runtime test must tell constructors apart, a tag test like `$value.$type === 'Some'`); standalone ::namespace
6. ::enum as sugar (start with always boxed, disjointness-based tagged later) -> userland Option fixture passes
7. modules and imports
8. typeclasses: first-order with member fan-out, then constraints (dictionary passing), static members (`Int.empty`), HKT with re-sugaring

# Todo

## Pattern matching (current focus, in order)@

1. record patterns: record rules in `disjoint` (exhaustive `isDisjointFromRecords`); runtime test checks own properties (`Object.hasOwn`), plus object/array/`$type` checks only when the remaining matched type needs them
2. or-patterns, guard patterns, `??` default patterns (`{foo: String ?? "x"}` accepts `{} | {foo: String}`)
3. name missing cases in non-exhaustive errors: needs a union expansion whose results can be shown (`expandUnions` results must not be shown); alternatively compute the remaining type step by step (`matched -- covered1 -- covered2 ...`, needs `--` difference types): what remains after the last case is what's missing (e.g. `Some<Blue>`), and reachability becomes "case overlaps the remaining type", cheaper than checking against the growing union of earlier cases when the remaining type simplifies (unions of singletons/constructors)

## Pattern matching follow-ups

- not on the critical path (Option/enums don't need it): emitter: string patterns mixing captures and other parts (`"$(a)-$(b)"`) are still `???` in JsEmitter.emitCaseMatch; they need runtime matching, e.g. a helper (inline or a `$`-prefixed module-level helper emitted only when used, until the runtime module exists) where each capture takes the shortest text before the next literal part (first occurrence) and the last literal part has to end the string
- runtime module: helpers like `isArray` and the string pattern matcher live in a separate runtime module with tree-shakeable exports (`import * as $clara from ...`, `$clara.isArray`), not duplicated per module. Ties in with modules (Roadmap 7): where the runtime file lives and how outputs import it (CLI `-o` and stdout), output becomes an ES module, E2eSpec runs `node -e` (a script, needs `--input-type=module` or a temp `.mjs` next to the runtime). Until then the emitted code reaches globals through `globalThis` (e.g. `globalThis.Array.isArray`)
- analyzer: a lambda with a tuple parameter doesn't get its type from the expected type (`g: (String, String) => String = (_, b) => b` is "Could not infer type")
- `::declare` of a reserved name (e.g. `::declare undefined: ...`) is broken: references get the `$` suffix but the JS global has none; later: an annotation declaring the JS name explicitly
- tuples vs array types (when array literals land): both are JS arrays, so they can't be told apart at runtime. A tuple pattern means "an array of length n" structurally (matches `Array<T>` too and narrows to `(T, ..., T)`), and `disjoint` must not treat tuples and arrays as disjoint (`isDisjointFromTuples`). Type tests can't separate `(Int, Int)` from `Array<Int>`, so "can be tested at runtime" needs its own check, separate from `disjoint` (the same applies to records vs boxed). Consider `(A, B) <: ReadonlyArray<A | B>` like TypeScript
- runtime representation: rename constructor tag `$` to `$type` and boxed payload `value` to `$value` so no Clara field name can collide
- phantom type params of tagged/boxed: error until variance annotations exist, then infer variance per type con (see TODO in Types.AssignableImpl)
- polymorphic callee: the expected argument type contains the callee's type params, e.g. a lambda argument's parameter is typed `A` (see FIXME in Types.findSubstitutions)
- duplicate literal cases (`#(1 => ..., 1 => ...)`) aren't reported as unreachable since literals cover nothing (would need literal types)
- emitter: skip the `Array.isArray`/length checks of tuple cases when the matched type is known to be a tuple of that length (needs the matched type in `Terms.Piecewise`)
- later: `e :< T` checks that `e` is assignable to `T` but keeps `e`'s own type (like TypeScript's `satisfies`)
- TypeInterpreter warnings: unused import, non-exhaustive match in aliasWrappedMethodTargets
- runtime type tests for type patterns in cases (`n: Int` on `Int | String`): only needed when the matched type isn't already assignable to `T`, and `T` must be testable at runtime (not functions or type params); opaque types declare their runtime test (`typeof`, ties in with Option without boxing below). Until then cases needing one are an error (`PatternAnalyzer.needsTypeTest`, also single capture string patterns like `"$(s)"` on `Int | String`), remove that error then
- Option without boxing (JS interop): `None` as tagged host undefined/null, opaque types declare their runtime test via target annotations, `A -- B` difference types for the `Some` payload

## Other

- figure out how to namespace enums (Color.Red)
- revisit tagged/boxed and enum approach
- tagged/boxed/enum, method syntax review, replace companion objects with static methods (to be used later in typeclasses)
- support recursion for functions and types
- implement basic module support
  - add AST support for Module
  - implement import syntax in parser
- fix parsing negative numbers, revisit operator precedence (+/*)
- array literals
- support forward references on blocks (same level)
- ValueExprAnalyzerSpec
- Record field and methods access for union types
- patterns: dot-constant patterns (`X.Y`, `x.y`) need parser support; `Y Z` in pattern position is always a construct pattern
- subtype Uniq into DefId/TypeConId/NsId/InstanceId before the Env refactor (plans/namespacing.md)
- decide: singleton `Foo.x` merges companion members, inherent methods and instance members into one tier (duplicates within `::extend Foo` are definition-time errors, others lookup-time ambiguity)
- merge identical alternatives in Types.Union (keeps alias nodes, e.g. `Option<Int> | Option<Int>`)
- enum stays alias of union; generic calls need re-sugaring of unions to aliases (plans/namespacing.md)
- decide: are instances imported only via companions or from the whole module top level
- revisit: instance derivation for records/tuples/unions (no explicit `::impl` for structural types)
- revisit: intersection member lookup when several parts have the member

# Laters

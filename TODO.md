# Done

- warn on unused value definitions
- Fix isAssignable for polymorphic funcs — `<A>A => A` should be assignable to `<B>B => B`
- union/intersection assignability: rule order (`A & B` wasn't assignable to itself), lazy head normalization (distribute over unions, unwrap aliases, merge records, disjoint members to Bottom), `disjoint`, expanding nested unions against union targets (tuples, records, tagged/boxed wrapped types; max 25 combinations), function intersections (`#()`) against functions, records/tuples with empty members, invariant phantom type params of tagged/boxed
- constructors collapse single member unions/intersections (`#()` with one case is a plain function)
- patterns: refutable `#()` cases (`walkCase`): must be able to match some value and narrow the matched type, assignments/parameters stay irrefutable (`walkAssignment`); tuple patterns on unions of tuples
- matched type flows into `#()`/lambda callees (`x @ #(...)`) and into `#()`/lambdas nested in tuple/record arguments and annotated record fields
- "can never match" errors quote the pattern's source (`Pos.sourceText`)
- literal patterns (`1`, `1.5`, `"a"`) are errors in assignments and function parameters since they can fail to match (no literal types, revisit if added); a string pattern of a single capture stays allowed
- expression `e: T` is a declaration like `x: T = e`: the value's type becomes `T` (`Terms.ValueAs`) and `T` is the expected type for `e` (e.g. `(n => n): Int => Int`)

# Roadmap (namespacing / enum / typeclasses)

Each step is a vertical slice (parser -> analyzer -> emitter) validated by an e2e fixture; keep existing tests green.

0. acceptance fixtures first: write the target `.clara` files (userland Option, Color, smart constructor) with expected `.messages`/`.output`; they fail until done
1. groundwork, no behavior change: Uniq subtypes, Union dedupe, Env -> TypeEntry/ValueEntry/NamespaceRef with DefId usage tracking
2. block phases: types and namespaces first, then ::extend, then values (forward references, recursive types)
3. ::extend: companion statics, ::methods (replaces standalone method sections), `Bar` = `Bar.apply`, `::new`
4. member lookup: tagged/boxed fallback to wrapped (emit `.value` for boxed), alias -> expansion, union per alternative
5. patterns: dot-constant, construct pattern; standalone ::namespace
6. ::enum as sugar (start with always boxed, disjointness-based tagged later) -> userland Option fixture passes
7. modules and imports
8. typeclasses: first-order with member fan-out, then constraints (dictionary passing), static members (`Int.empty`), HKT with re-sugaring

# Todo

## Pattern matching (current focus, in order)

1. `#()` exhaustiveness and reachability: each pattern needs the type it fully covers besides its narrowed type (literals cover nothing, captures/wildcards everything, singletons their type, tuples only if all elements cover); exhaustive: matched <: covered1 | ... | coveredN, case i reachable: !(narrowed_i <: covered1 | ... | covered(i-1))
2. construct pattern (`Some a`) + pattern validity (Check 1 in plans/enum2.md: the runtime test must tell constructors apart); type patterns in cases (`n: T`) need no runtime test when the matched type is already assignable to `T`, otherwise `T` must be testable at runtime (not functions or type params)
3. record patterns: record rules in `disjoint` (exhaustive `isDisjointFromRecords`); runtime test checks own properties (`Object.hasOwn`), plus object/array/`$type` checks only when the remaining matched type needs them
4. or-patterns, guard patterns, `??` default patterns (`{foo: String ?? "x"}` accepts `{} | {foo: String}`)
5. name missing cases in non-exhaustive errors: needs a union expansion whose results can be shown (`expandUnions` results must not be shown)

## Pattern matching follow-ups

- runtime representation: rename constructor tag `$` to `$type` and boxed payload `value` to `$value` so no Clara field name can collide
- phantom type params of tagged/boxed: error until variance annotations exist, then infer variance per type con (see TODO in Types.AssignableImpl)
- polymorphic callee: the expected argument type contains the callee's type params, e.g. a lambda argument's parameter is typed `A` (see FIXME in Types.findSubstitutions)
- later: `e :< T` checks that `e` is assignable to `T` but keeps `e`'s own type (like TypeScript's `satisfies`)
- TypeInterpreter warnings: unused import, non-exhaustive match in aliasWrappedMethodTargets
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

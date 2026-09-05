# Done

- warn on unused value definitions

# Todo

- revisit tagged/boxed and enum approach
- tagged/boxed/enum, method syntax review, replace companion objects with static methods (to be used later in typeclasses)
- support recursion for functions and types
- construct pattern
- record pattern
- implement basic module support
  - add AST support for Module
  - implement import syntax in parser
- fix parsing negative numbers, revisit operator precedence (+/*)
- array literals
- support forward references on blocks (same level)
- Fix isAssignable for polymorphic funcs — <A>A => A should be assignable to <B>B => B or already fixed?
- ValueExprAnalyzerSpec
- Record field and methods access for union types
- call: if callee is a literal piecewise or lambda, resolve argument first and use that to help infer types in the callee
- piecewise exhaustiveness/reachability checks
- implement piecewise/patterns missing pieces

# Laters

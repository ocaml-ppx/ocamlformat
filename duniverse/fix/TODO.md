# TODO

* In `MEMOIZER`, some variations are missing, e.g. `visibly_fix`,
  `visibly_defensive_fix`.

* Think about a heterogeneous version of the fixed point computation
  algorithm, where valuations have type `forall 'a. 'a variable -> 'a property`.
  (This would internally require using heterogenous maps...)

* Do something with `src/attic/BoolEqs` and `src/attic/ChopFix`,
  or remove them.

* Consider using two data fields in `node` instead of one,
  so as to avoid using a separate `data` record. Benchmark.

* Provide an extensible-vector implementation of `IMPERATIVE_MAPS` for
  integers? Like `ArraysAsImperativeMaps`, but does not require `n`.
      Use `InfiniteArray`.

* Provide an API in the style of Menhir's `FixSolver`, where constraints are
  discovered incrementally during a first phase, then the solver is started?

* Develop a test suite. (Use `afl-fuzz`?)
  E.g., in `CFG`, write a CFG generator.
  Compare `Fix` with a naive solver.

* Develop a performance benchmark.

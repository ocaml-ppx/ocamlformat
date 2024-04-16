# Fix: memoization and fixed points made easy

`fix` is an OCaml library that helps with various algorithmic
constructions that involve memoization, recursion, and numbering.

## Documentation

See the [documentation of the latest released
version](http://cambium.inria.fr/~fpottier/fix/doc/fix/).

## Demos

A few demos are provided:

* [`brz`](demos/brz) sets up a hash-consed representation of regular
  expressions and shows how to convert a regular expression to a deterministic
  finite-state automaton by Brzozowski's method. This demo exploits many
  of the submodules listed above, and is accompanied with
  [a commentary](misc/post.md).

* [`cyk`](demos/cyk) presents a CYK-style parsing algorithm as an instance of
  `Fix`.

* [`cfg`](demos/cfg) uses `Fix` to perform certain static analyses of a
  context-free grammar; this includes computing nullability information and
  FIRST sets.

* [`fib`](demos/fib) defines Fibonacci's function in several different ways
  using the fixed-point combinators offered by `Memoize` and `Fix`.

* [`hco`](demos/hco) sets up simple-minded hash-consed trees
  using `HashCons`.

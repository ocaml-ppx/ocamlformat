  $ echo 'let x = 1' > a.ml
  $ ocamlformat --max-iters=1 a.ml
  let x = 1

  $ echo 'let x     = 1' > a.ml
  $ ocamlformat --max-iters=1 a.ml
  ocamlformat: "a.ml" was not already formatted. ([max-iters = 1])
  [1]

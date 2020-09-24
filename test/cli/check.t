  $ echo 'let x = 1' > a.ml
  $ ocamlformat --check a.ml

  $ echo 'let x     =1' > a.ml
  $ ocamlformat --check a.ml
  [1]

  $ echo 'let x = 1' > a.ml
  $ ocamlformat --output x.ml --check a.ml
  ocamlformat: Cannot specify --output with --check
  [1]

  $ echo 'let x = 1' > a.ml
  $ ocamlformat --inplace --check a.ml
  ocamlformat: Cannot specify --inplace with --check
  [1]

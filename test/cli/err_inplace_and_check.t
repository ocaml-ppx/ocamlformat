  $ echo 'let x = 1' > a.ml

  $ ocamlformat --inplace --check a.ml
  ocamlformat: Cannot specify --inplace with --check
  [1]

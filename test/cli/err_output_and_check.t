  $ echo 'let x = 1' > a.ml

  $ ocamlformat --output x.ml --check a.ml
  ocamlformat: Cannot specify --output with --check
  [1]

  $ echo 'let x = 1' > a.ml
  $ echo 'let x = 2' > b.ml

  $ ocamlformat --output x.ml a.ml b.ml
  ocamlformat: Must specify exactly one input file without --inplace or --check
  [1]

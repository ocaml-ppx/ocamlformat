  $ touch a.mli b.ml

  $ ocamlformat --name foo.ml --check a.mli b.ml
  ocamlformat: Cannot specify --name with multiple inputs
  [1]

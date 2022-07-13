  $ echo profile=default > .ocamlformat

  $ echo 'let x = y' > a.ml

Setting a deprecated option or a deprecated option value on the command line should display a warning message:

  $ ocamlformat a.ml --disambiguate-non-breaking-match
  let x = y

A warning is also reported if a deprecated option or a deprecated option value is set in an .ocamlformat file:

  $ echo 'disambiguate-non-breaking-match = true' > .ocamlformat
  $ ocamlformat a.ml
  let x = y

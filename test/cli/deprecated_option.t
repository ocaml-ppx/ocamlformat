  $ echo 'let x = y' > a.ml

Setting a deprecated option or a deprecated option value on the command line should display a warning message:

  $ ocamlformat a.ml --disambiguate-non-breaking-match
  Warning: disambiguate-non-breaking-match: This option is deprecated since version 0.20.0. It will be removed by version 1.0.
  let x = y

  $ ocamlformat a.ml --profile sparse
  Warning: option `profile`: value `sparse` is deprecated since version 0.20.0. It will be removed by version 1.0.
  let x = y

A warning is also reported if a deprecated option or a deprecated option value is set in an .ocamlformat file:

  $ echo 'disambiguate-non-breaking-match = true' > .ocamlformat
  $ ocamlformat a.ml
  Warning: disambiguate-non-breaking-match: This option is deprecated since version 0.20.0. It will be removed by version 1.0.
  let x = y

  $ echo 'profile = sparse' > .ocamlformat
  $ ocamlformat a.ml
  Warning: option `profile`: value `sparse` is deprecated since version 0.20.0. It will be removed by version 1.0.
  let x = y

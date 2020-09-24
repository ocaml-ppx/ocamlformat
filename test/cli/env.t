Invalid option:

  $ echo 'let x = 1' | OCAMLFORMAT="unknown=true" ocamlformat --impl -
  ocamlformat: Error while parsing OCAMLFORMAT environment variable:
               Unknown option "unknown"
  [1]

Invalid value:

  $ echo 'let x = 1' | OCAMLFORMAT="type-decl=unknown" ocamlformat --impl -
  ocamlformat: Error while parsing OCAMLFORMAT environment variable:
               For option "type-decl": invalid value `unknown', expected either `compact' or `sparse'
  [1]

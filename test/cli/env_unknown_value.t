  $ echo 'let x = 1' | OCAMLFORMAT="type-decl=unknown" ocamlformat --impl -
  ocamlformat: Error while parsing OCAMLFORMAT environment variable:
               For option "type-decl": invalid value `unknown', expected either `compact' or `sparse'
  [1]

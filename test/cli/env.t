Invalid option:

  $ echo 'let x = 1' | OCAMLFORMAT="unknown=true" ocamlformat --impl -
  ocamlformat: Error while parsing OCAMLFORMAT environment variable:
               Unknown option "unknown"
  [1]

Invalid value:

  $ echo 'let x = 1' | OCAMLFORMAT="exp-grouping=unknown" ocamlformat --impl -
  ocamlformat: Error while parsing OCAMLFORMAT environment variable:
               For option "exp-grouping": invalid value `unknown', expected either `parens' or `preserve'
  [1]

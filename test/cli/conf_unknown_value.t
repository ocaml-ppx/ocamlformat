  $ echo 'field-space = unknown_value' > .ocamlformat

  $ echo 'let x = 1' | ocamlformat --impl -
  ocamlformat: Error while parsing .ocamlformat:
               For option "field-space": invalid value `unknown_value', expected one of `loose', `tight' or `tight-decl'
  [1]

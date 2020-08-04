  $ echo 'unknown_option = true' > .ocamlformat

  $ echo 'let x = 1' | ocamlformat --impl -
  ocamlformat: Error while parsing .ocamlformat:
               Unknown option "unknown_option"
  [1]

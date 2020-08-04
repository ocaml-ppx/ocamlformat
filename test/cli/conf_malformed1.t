  $ echo 'a = b = c' > .ocamlformat

  $ echo 'let x = 1"' | ocamlformat --impl -
  ocamlformat: Error while parsing .ocamlformat:
               Invalid format "a = b = c"
  [1]

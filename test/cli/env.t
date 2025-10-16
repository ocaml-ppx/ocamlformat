Invalid option:

  $ echo 'let x = 1' | OCAMLFORMAT="unknown=true" ocamlformat --impl - 2>/dev/null
  [1]

Invalid value:

  $ echo 'let x = 1' | OCAMLFORMAT="type-decl=unknown" ocamlformat --impl - 2>/dev/null
  [1]

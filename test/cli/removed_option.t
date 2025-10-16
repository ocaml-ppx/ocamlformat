  $ echo 'let x = y' > a.ml

Setting a removed option on the command line should display an error message:

  $ ocamlformat a.ml --extension-sugar preserve 2>/dev/null
  [1]

An error is also reported if a removed option is set in an .ocamlformat file:

  $ echo 'escape-chars = preserve' > .ocamlformat
  $ ocamlformat a.ml 2>/dev/null
  [1]

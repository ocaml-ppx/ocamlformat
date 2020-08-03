  $ echo 'let y =' > syntax_error.ml

  $ ocamlformat --name foo.ml syntax_error.ml
  ocamlformat: ignoring "foo.ml" (syntax error)
  File "foo.ml", line 2, characters 0-0:
  Error: Syntax error
  [1]

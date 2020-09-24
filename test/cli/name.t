  $ echo 'let x =     1' > a.ml
  $ ocamlformat --name a.cpp a.ml
  let x = 1

  $ echo 'let y =' > syntax_error.ml
  $ ocamlformat --name foo.ml syntax_error.ml
  ocamlformat: ignoring "foo.ml" (syntax error)
  File "foo.ml", line 2, characters 0-0:
  Error: Syntax error
  [1]

  $ echo 'module X : S' > a.mli
  $ ocamlformat --name foo.ml a.mli
  ocamlformat: ignoring "foo.ml" (syntax error)
  File "foo.ml", line 2, characters 0-0:
  Error: Syntax error
  [1]

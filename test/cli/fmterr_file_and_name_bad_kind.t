  $ echo 'module X : S' > a.mli

  $ ocamlformat --name foo.ml a.mli
  ocamlformat: ignoring "foo.ml" (syntax error)
  File "foo.ml", line 2, characters 0-0:
  Error: Syntax error
  [1]

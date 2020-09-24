  $ echo 'module X : S' > a.mli

  $ ocamlformat --impl a.mli
  ocamlformat: ignoring "a.mli" (syntax error)
  File "a.mli", line 2, characters 0-0:
  Error: Syntax error
  [1]

  $ echo profile=default > .ocamlformat

One of '--impl', '--intf' or '--name' is required when the input is read from stdin:

  $ echo 'let x = 1' | ocamlformat - 2>/dev/null
  [1]

  $ echo 'let x = 1' | ocamlformat --inplace - 2>/dev/null
  [1]

Nominal cases:

  $ echo 'let x =       1' | ocamlformat --impl -
  let x = 1

  $ echo 'val x :     int' | ocamlformat --intf -
  val x : int

The kind of syntax --impl/--intf is inferred from the name:

  $ echo 'let x =      1' | ocamlformat --name a.ml -
  let x = 1

The syntax cannot be inferred if the extension of the file is unsupported:

  $ ocamlformat --name a.cpp - 2>/dev/null
  [1]

Cannot specify files with stdin:

  $ echo 'let x = 1' > a.ml
  $ ocamlformat a.ml - 2>/dev/null
  [1]

The input is named after the '--name' argument in formatting error messages:

  $ echo 'let y =' | ocamlformat --name foo.ml -
  ocamlformat: ignoring "foo.ml" (syntax error)
  File "foo.ml", line 2, characters 0-0:
  Error: Syntax error
  [1]

Bad kind of syntax --impl/--intf:

  $ echo 'module X : S' | ocamlformat --impl -
  ocamlformat: ignoring "<standard input>" (syntax error)
  File "<standard input>", line 2, characters 0-0:
  Error: Syntax error
  [1]

Not specifying --impl/--intf (inferred from --name) resulting in a syntax error:

  $ echo 'module X : S' | ocamlformat --name foo.ml -
  ocamlformat: ignoring "foo.ml" (syntax error)
  File "foo.ml", line 2, characters 0-0:
  Error: Syntax error
  [1]

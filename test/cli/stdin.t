One of '--impl', '--intf' or '--name' is required when the input is read from stdin:

  $ ocamlformat -
  ocamlformat: Must specify at least one of --name, --impl or --intf when reading from stdin
  [1]

  $ echo 'let x =       1' | ocamlformat -
  ocamlformat: Must specify at least one of --name, --impl or --intf when reading from stdin
  [1]

  $ ocamlformat --inplace -
  ocamlformat: Must specify at least one of --name, --impl or --intf when reading from stdin
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

  $ ocamlformat --name a.cpp -
  ocamlformat: Cannot deduce file kind from passed --name. Please specify --impl or --intf
  [1]

Cannot specify files with stdin:

  $ echo 'let x = 1' > a.ml
  $ ocamlformat a.ml -
  ocamlformat: Cannot specify stdin together with other inputs
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

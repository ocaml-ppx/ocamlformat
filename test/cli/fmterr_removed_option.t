  $ echo 'let x = y' > a.ml

Setting a removed option on the command line should display an error message:

  $ ocamlformat a.ml --escape-chars preserve
  ocamlformat: option `--escape-chars': This option has been removed in version
               0.16.0. Concrete syntax will now always be preserved.
  Usage: ocamlformat [OPTION]... [SRC]...
  Try `ocamlformat --help' for more information.
  [1]

  $ ocamlformat a.ml --escape-strings preserve
  ocamlformat: option `--escape-strings': This option has been removed in
               version 0.16.0. Concrete syntax will now always be preserved.
  Usage: ocamlformat [OPTION]... [SRC]...
  Try `ocamlformat --help' for more information.
  [1]

An error is also reported if a removed option is set in an .ocamlformat file:

  $ echo 'escape-chars = preserve' > .ocamlformat
  $ ocamlformat a.ml
  ocamlformat: Error while parsing .ocamlformat:
               For option "escape-chars": This option has been removed in version 0.16.0. Concrete syntax will now always be preserved.
  [1]

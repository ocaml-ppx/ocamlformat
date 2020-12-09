  $ echo 'let x = y' > a.ml

Setting a removed option on the command line should display an error message:

  $ ocamlformat a.ml --let-open preserve
  ocamlformat: option `--let-open': This option has been removed in version
               0.17.0. Concrete syntax will now always be preserved.
  Usage: ocamlformat [OPTION]... [SRC]...
  Try `ocamlformat --help' for more information.
  [1]

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

  $ ocamlformat a.ml --doc-comments-val unset
  ocamlformat: option `--doc-comments-val': This option has been removed in
               version 0.16.0. If you are using `doc-comments-val=before` in
               combination with `doc-comments=before` then only
               `doc-comments=before` is now required to achive the same
               behavior. If you are using `doc-comments-val=before` in
               combination with `doc-comments=after` this behavior is not
               available anymore. If you are using `doc-comments-val=after` in
               combination with `doc-comments=before` please now use
               `doc-comments=before-except-val`. If you are using
               `doc-comments-val=after` in combination with
               `doc-comments=after` then only
               `doc-comments=after-when-possible` is now required to achieve
               the same behavior. If you are using `doc-comments-val=unset` the
               same behavior can now be achieved by setting `doc-comments`
               only.
  Usage: ocamlformat [OPTION]... [SRC]...
  Try `ocamlformat --help' for more information.
  [1]

An error is also reported if a removed option is set in an .ocamlformat file:

  $ echo 'escape-chars = preserve' > .ocamlformat
  $ ocamlformat a.ml
  ocamlformat: Error while parsing .ocamlformat:
               For option "escape-chars": This option has been removed in version 0.16.0. Concrete syntax will now always be preserved.
  [1]

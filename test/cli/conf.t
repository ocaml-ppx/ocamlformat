Invalid version:

  $ mkdir prj
  $ cd prj

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (name prj)
  > EOF

  $ echo 'version = bad' > .ocamlformat
  $ echo 'let x = "Hello World"' > a.ml

Exit code is printed by hand because sed succeeding would hide the error.

  $ <a.ml ocamlformat --impl - 2>/dev/null
  [1]

Disable version check:

  $ <a.ml ocamlformat --impl --no-version-check -
  let x = "Hello World"

Invalid syntax in .ocamlformat file:

  $ echo 'a = b = c' > .ocamlformat
  $ echo 'let x = 1"' | ocamlformat --impl - 2>/dev/null
  [1]

Invalid option:

  $ echo 'unknown_option = true' > .ocamlformat
  $ echo 'let x = 1' | ocamlformat --impl - 2>/dev/null
  [1]

Invalid option (short negated form):

  $ echo 'no-wrap-comments' > .ocamlformat
  $ echo 'let x = 1' | ocamlformat --impl - 2>/dev/null
  [1]

Invalid value:

  $ echo 'field-space = unknown_value' > .ocamlformat
  $ echo 'let x = 1' | ocamlformat --impl - 2>/dev/null
  [1]

Invalid version:

  $ echo 'version = bad' > .ocamlformat
  $ echo 'let x = "Hello World"' > a.ml

Exit code is printed by hand because sed succeeding would hide the error.

  $ (<a.ml ocamlformat --impl -; echo [$?]) 2>&1 | sed 's/expecting "[^"]*"/expecting "..."/g'
  ocamlformat: Error while parsing .ocamlformat:
               For option "version": expecting "..." but got "bad"
  [1]

Invalid syntax in .ocamlformat file:

  $ echo 'a = b = c' > .ocamlformat
  $ echo 'let x = 1"' | ocamlformat --impl -
  ocamlformat: Error while parsing .ocamlformat:
               Invalid format "a = b = c"
  [1]

Invalid option:

  $ echo 'unknown_option = true' > .ocamlformat
  $ echo 'let x = 1' | ocamlformat --impl -
  ocamlformat: Error while parsing .ocamlformat:
               Unknown option "unknown_option"
  [1]

Invalid value:

  $ echo 'field-space = unknown_value' > .ocamlformat
  $ echo 'let x = 1' | ocamlformat --impl -
  ocamlformat: Error while parsing .ocamlformat:
               For option "field-space": invalid value `unknown_value', expected one of `loose', `tight' or `tight-decl'
  [1]

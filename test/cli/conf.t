Invalid version:

  $ echo 'version = bad' > .ocamlformat
  $ echo 'let x = "Hello World"' > a.ml

Exit code is printed by hand because sed succeeding would hide the error.

  $ (<a.ml ocamlformat --impl -; echo [$?]) 2>&1 | sed 's/version is "[^"]*"/version is "..."/g'
  ocamlformat: Error while parsing $TESTCASE_ROOT/.ocamlformat:
               Project should be formatted using ocamlformat version "bad", but the installed version is "..."
  [1]

Disable version check:

  $ (<a.ml ocamlformat --impl --no-version-check -; echo [$?]) 2>&1 | sed 's/version is "[^"]*"/version is "..."/g'
  let x = "Hello World"
  [0]

Invalid syntax in .ocamlformat file:

  $ echo 'a = b = c' > .ocamlformat
  $ echo 'let x = 1"' | ocamlformat --impl -
  ocamlformat: Error while parsing $TESTCASE_ROOT/.ocamlformat:
               Invalid format "a = b = c"
  [1]

Invalid option:

  $ echo 'unknown_option = true' > .ocamlformat
  $ echo 'let x = 1' | ocamlformat --impl -
  ocamlformat: Error while parsing $TESTCASE_ROOT/.ocamlformat:
               Unknown option "unknown_option"
  [1]

Invalid option (short negated form):

  $ echo 'no-wrap-comments' > .ocamlformat
  $ echo 'let x = 1' | ocamlformat --impl -
  ocamlformat: Error while parsing $TESTCASE_ROOT/.ocamlformat:
               Unknown option "no-wrap-comments": "no-wrap-comments" is the short form for "wrap-comments=false". It is only accepted on command line, please use "wrap-comments=false" or "wrap-comments=true" instead.
  [1]

Invalid value:

  $ echo 'field-space = unknown_value' > .ocamlformat
  $ echo 'let x = 1' | ocamlformat --impl -
  ocamlformat: Error while parsing $TESTCASE_ROOT/.ocamlformat:
               For option "field-space": invalid value 'unknown_value', expected one of 'loose', 'tight' or 'tight-decl'
  [1]

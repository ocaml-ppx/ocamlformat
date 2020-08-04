  $ echo 'version = bad' > .ocamlformat
  $ echo 'let x = "Hello World"' > a.ml

Exit code is printed by hand because sed succeeding would hide the error.

  $ (<a.ml ocamlformat --impl -; echo [$?]) 2>&1 | sed 's/expecting "[^"]*"/expecting "..."/g'
  ocamlformat: Error while parsing .ocamlformat:
               For option "version": expecting "..." but got "bad"
  [1]

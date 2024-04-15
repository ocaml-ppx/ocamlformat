This tests the version picker by providing two mock ocamlformat-$version$
executable and checking that the correct one is picked.
  $ ocamlc ocamlformat_blue.ml -o ocamlformat-blue
  $ ocamlc ocamlformat_red.ml -o ocamlformat-red
  $ ocamlopt ocamlformat_blue.ml -o ocamlformat-blue 2> null > null || true
  $ ocamlopt ocamlformat_red.ml -o ocamlformat-red  2> null > null || true
  $ echo "let x = 3" > test.ml
  $ echo "version = red" > .ocamlformat
  $ PATH=$PATH:. ocamlformat-red
  version red
  $ PATH=$PATH:. ocamlformat test.ml
  version red
  $ echo "version = blue" > .ocamlformat
  $ PATH=$PATH:. ocamlformat test.ml
  version blue
  $ echo "version = green" > .ocamlformat
  $ PATH=$PATH:. ocamlformat test.ml
  Ocamlformat version green not installed.
  You may be able to get it with: `opam install ocamlformat-green`
  If this package does not exists, try `opam update`.
  If it still does not exist, there might be a typo in your config, or the version is very old and has been not been yet repackaged.
  [1]
  $ echo "" > .ocamlformat
  $ PATH=$PATH:. ocamlformat test.ml
  let x = 3
  $ printf "version = green\nversion-check = false" > .ocamlformat
  $ PATH=$PATH:. ocamlformat test.ml
  let x = 3
  $ printf "version = red\nversion-check = false" > .ocamlformat
  $ PATH=$PATH:. ocamlformat test.ml
  version red

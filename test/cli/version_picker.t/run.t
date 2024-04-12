This tests the version picker by providing two mock ocamlformat-$version$
executable and checking that the correct one is picked.
  $ ocamlopt ocamlformat_blue.ml -o ocamlformat-blue
  $ ocamlopt ocamlformat_red.ml -o ocamlformat-red
  $ ocamlopt ocamlformat_blue.ml -o ocamlformat-blue.exe
  $ ocamlopt ocamlformat_red.ml -o ocamlformat-red.exe
  $ ls
  ocamlformat-blue
  ocamlformat-blue.exe
  ocamlformat-red
  ocamlformat-red.exe
  ocamlformat_blue.cmi
  ocamlformat_blue.cmx
  ocamlformat_blue.ml
  ocamlformat_blue.o
  ocamlformat_red.cmi
  ocamlformat_red.cmx
  ocamlformat_red.ml
  ocamlformat_red.o
  $ cp _build/install/default/bin* .
  cp: cannot stat '_build/install/default/bin*': No such file or directory
  [1]
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

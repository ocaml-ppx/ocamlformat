This tests the version picker by providing two mock ocamlformat-$version$
executable and checking that the correct one is picked.
  $ printf "#!/bin/sh\necho \"version red\"" > ocamlformat-red
  $ printf "#!/bin/sh\necho \"version blue\"" > ocamlformat-blue
  $ printf "#!/bin/sh\necho \"version red\"" > ocamlformat-red.exe
  $ printf "#!/bin/sh\necho \"version blue\"" > ocamlformat-blue.exe
  $ chmod +x ocamlformat-red
  $ chmod +x ocamlformat-blue
  $ chmod +x ocamlformat-red.exe
  $ chmod +x ocamlformat-blue.exe
  $ cat ocamlformat-red
  #!/bin/bash
  echo "version red"
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

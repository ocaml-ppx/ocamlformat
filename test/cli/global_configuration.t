The user's global configuration should be used when [--enable-outside-detected-project] is passed.

  $ mkdir -p root xdg
  $ export XDG_CONFIG_HOME=$PWD/xdg
  $ echo 'break-cases = vertical' > xdg/ocamlformat

  $ cd root
  $ touch dune-project
  $ echo 'let _=match x with A->()|B->()' > test.ml

  $ ocamlformat --enable-outside-detected-project test.ml
  let _ =
    match x with
    | A ->
        ()
    | B ->
        ()

No global configuration:

  $ XDG_CONFIG_HOME= ocamlformat --enable-outside-detected-project test.ml
  let _ = match x with A -> () | B -> ()

No --enable-outside-detected-project:

  $ ocamlformat test.ml
  File "test.ml", line 1:
  Warning: Ocamlformat disabled because [--enable-outside-detected-project] is not set and no [.ocamlformat] was found within the project (root: ../root)
  let _=match x with A->()|B->()

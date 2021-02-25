Reproduction of the issue in #2896

We have a dependency cycle of the form impl <- lib <- vlib

where vlib is a virtual library, and impl implements this library.

  $ echo "(lang dune 2.3)" > dune-project
  $ mkdir vlib impl lib
  $ touch impl/vlib.ml
  $ echo "val run : unit -> unit" > vlib/vlib.mli
  $ cat >vlib/dune <<EOF
  > (library (name vlib) (virtual_modules vlib))
  > EOF
  $ echo "let bar () = Vlib.run ();;" > lib/lib.ml
  $ cat >lib/dune <<EOF
  > (library (name lib) (libraries vlib))
  > EOF
  $ echo "let run () = Lib.bar ();;" > impl/vlib.ml
  $ cat >impl/dune <<EOF
  > (library (name impl) (implements vlib) (libraries lib))
  > EOF

The implementation impl was built, but it's not usable:

  $ echo 'Vlib.run ()' > foo.ml
  $ echo "(executable (name foo) (libraries impl))" > dune
  $ dune exec ./foo.exe
  File "_none_", line 1:
  Error: No implementations provided for the following modules:
           Vlib referenced from lib/lib.cmxa(Lib)
  [1]

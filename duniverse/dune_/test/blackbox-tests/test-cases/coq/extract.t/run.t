  $ cat >dune-project <<EOF
  > (lang dune 2.5)
  > (using coq 0.2)
  > EOF

  $ cat >extract.v <<EOF
  > Definition nb (b : bool) : bool :=
  >   match b with
  >   | false => true
  >   | true => false
  >   end.
  > 
  > Require Extraction.
  > Separate Extraction nb.
  > EOF

  $ cat >dune <<EOF
  > (coq.extraction
  >  (prelude extract)
  >  (extracted_modules Datatypes extract))
  > 
  > (executable
  >  (name foo))
  > EOF

  $ cat >foo.ml <<EOF
  > open Datatypes
  > let () =
  >   print_endline (
  >     match Extract.nb Datatypes.Coq_true with
  >     | Coq_true -> "true"
  >     | Coq_false -> "false"
  >   )
  > EOF

  $ dune exec ./foo.exe
  false
  $ ls _build/default
  Datatypes.ml
  Datatypes.mli
  extract.glob
  extract.ml
  extract.mli
  extract.v
  extract.v.d
  extract.vo
  extract.vok
  extract.vos
  foo.exe
  foo.ml

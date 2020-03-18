(* Tests of special cases added to avoid emitting [>\] and [>\}], which are
   keywords. *)

(* Regression tests for https://github.com/ocaml-ppx/ocamlformat/issues/1295
   (unnecessary tailing spaces added after object types with attributes). *)

type t = {foo: (< .. >[@a]) }

type t = {foo: < .. > [@a] }

type t = A of {foo: (< .. >[@a]) }

type t = A of {foo: < .. > [@a] }

type t = [`Foo of (< .. >[@a]) ]

type t = [`Foo of < .. >[@a] ]

let _ =
  object
    inherit [b, (< f: unit >[@a]) ] foo
  end

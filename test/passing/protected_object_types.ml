(* Tests of special cases added to avoid emitting [>\] and [>\}], which are
   keywords. *)

(* Regression tests for https://github.com/ocaml-ppx/ocamlformat/issues/1295
   (unnecessary trailing spaces added after object types with attributes). *)

type t = {foo: (< .. >[@a])}

type t = {foo: < .. > [@a]}

type t = A of {foo: (< .. >[@a])}

type t = A of {foo: < .. > [@a]}

type t = [`Foo of (< .. >[@a])]

type t = [`Foo of < .. >[@a]]

let _ =
  object
    inherit [b, (< f: unit >[@a])] foo
  end

module Space_around = struct
  (* Ensure that the protection mechanism does not add extra spaces when
     [--space-around-*] options are sufficient. *)

  module Records = struct
    type t = { foo: < .. > }

    type t = A of { foo: < .. > }
  end
  [@@ocamlformat "space-around-records = true"]

  module Variants = struct
    type t = [ `Foo of < .. > ]
  end
  [@@ocamlformat "space-around-variants"]
end

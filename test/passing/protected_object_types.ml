(* Tests of special cases added to avoid emitting [>\] and [>\}], which are
   keywords. *)

(* Regression tests for https://github.com/ocaml-ppx/ocamlformat/issues/1295
   (unnecessary trailing spaces added after object types with attributes). *)

type t = {foo: (< .. >[@a])}

type t = {foo: < .. > [@a]}

type t = A of {foo: (< .. >[@a])}

type t = A of {foo: < .. > [@a]}

type t = [`Foo of (< .. >[@a])]

type t = [`Foo of < .. > [@a]]

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

module Inside_payloads = struct
  (* Regression tests for
     https://github.com/ocaml-ppx/ocamlformat/issues/1267 (failure to protect
     against object types inside extension and attribute payloads). *)

  let _ = [%ext: < .. > ]

  [%%ext: < .. > ]

  [%%ext
  ;;
  ()

  type a = < f: t > ]

  [@@@a: val b : < .. > ]

  let _ = () [@a: val b : < .. > ]

  let _ = () [@@a: val b : < .. > ]

  [@@@a: type x = < .. > ]

  [@@@a:
  val x : t

  type x = < .. > ]

  [@@@a: type t = < .. > ]

  [@@@a: type t = (< .. >[@a])]

  [@@@a: type a = A of t | B of t | C of < .. > ]

  [@@@a: type a = A of t | B of t | C of (t -> < .. >)]

  [@@@a: type a += C of a * b * < .. > ]

  [@@@a: type a += C of a * b * < .. > [@a]]

  [@@@a: type a += C of (a -> b * < .. >)]

  [@@@a: type a = t constraint t = < .. > ]

  [@@@a: type a = t constraint t = (< .. >[@a])]

  [@@@a: exception C of a * b * < .. > ]

  (* Simple attributes on exceptions not supported pre-4.08 *)
  [@@@a: exception C of a * b * < .. > [@@a]]
end

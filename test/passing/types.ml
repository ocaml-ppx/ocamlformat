type uu = A of int | B of (< leq: 'a > as 'a)

type uu = A of int | B of (< leq: 'a > as 'a) * 'a

type uu = A of (int as 'a) | B of 'a * (< leq: 'a > as 'a)

type uu += A of (int as 'a)

type uu += B of 'a * (< leq: 'a > as 'a)

let _ = ignore Async_unix.Fd.(([stdin (); stdout (); stderr ()] : t list))

type t = {a: int; b: int}

type t = [`A | `B]

type t =
  | Internal_error of
      [ `Doc_comment of
        [ `Moved of Location.t * Location.t * string
        | `Unstable of Location.t * string ] ]

type t =
  { a: int (* Comment *)
  ; b: int (* Comment *) }
[@@ocamlformat "type-decl=sparse"]

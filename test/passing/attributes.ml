let _ = (function[@warning "-4"] None -> true | _ -> false) None

let f (x[@warning ""]) = ()

let v = (fun [@inline] x -> x) 1

external f : (float[@unboxed]) -> int = "blah"  [@@noalloc]

val x : ?x:unit (** not dropped *) -> unit

type t = {a: int; b: int [@default 1] [@drop_if]}

type t =
  { a: int
  ; b: someloooooooooooooooooooooooooooooong typ
         [@default looooooooooooooooooooooooooooooooooooooooong]
         [@drop_if somethingelse]
  ; b: somelong typ [@default 1] }

val foo : int
  [@@deprecated "it is good the salad"] [@@warning "-32"] [@@warning "-99"]

val foo : int
  [@@deprecated "it is good the salad"]
  [@@warning "-32"]
  [@@warning "-99"]
  [@@some long comment]

type t = [`A of int[@default] | `B of (float[@default])]

let f x =
  let[@something] e = 1 in
  e[@@inline always]

module type M = S [@test1]

module type M = sig
  module T (T : sig end) : (S with type t = r) [@test2]

  module T (S : S [@test]) : S

  module T : (S with type t = (r[@test3])) [@test4]

  module T :
    (S with type t = t and type u := u and module R = R and module S := S)
  [@test]

  module T : module type of X [@test5]

  module T : (module type of X) [@test6]

  module T : [%ext] [@test7]

  module T = T [@@test8]
end

let f = fun [@inline] [@inline never] x -> x

let g = fun [@inline] [@something_else] [@ocaml.inline] x -> x

let h x = (g [@inlined] [@ocaml.inlined never]) x

let v = (fun [@inline] [@inlined] x -> x) 1

let i = fun [@inline] x -> x[@@inline]

;; if [@test] true then () else ()

;; if [@test] true then () else if [@test] true then () else ()

type blocklist =
  { f1: int [@version 1, 1, 0]  (** short comment *)
  ; f2: (int64 * int64) list
        (** loooooooooooooooooooooooooooooong
            commmmmmmmmmmmmmmmmmmmmmmmmmmmmmmment *) }

type blocklist =
  | F1 of int [@version 1, 1, 0]  (** short comment *)
  | F2: int -> blocklist [@version 1, 1, 0]  (** short comment *)
  | F3 of (int64 * int64) list
      (** loooooooooooooooooooooooooooooong
          commmmmmmmmmmmmmmmmmmmmmmmmmmmmmmment *)

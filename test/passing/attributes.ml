let _ = (function[@warning "-4"] None -> true | _ -> false) None

let f (x[@warning ""]) = ()

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

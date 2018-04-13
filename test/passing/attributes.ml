let _ = (function[@warning "-4"] None -> true | _ -> false) None

let f (x[@warning ""]) = ()

external f : (float[@unboxed]) -> int = "blah"  [@@noalloc]

val x : ?x:unit (** not dropped *) -> unit

val foo : int
  [@@deprecated "it is good the salad"] [@@warning "-32"] [@@warning "-99"]

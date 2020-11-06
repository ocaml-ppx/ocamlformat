module M = M

module M : sig
  type t

  val f : t -> t
end = struct
  type t = A | B of int * int | C of {a: int; b: int}

  let f x = x
end

module type S = sig
  val x : unit -> unit
end

let get = failwith "TODO"

let foo () =
  let module X = (val (get : (module S))) in
  X.x ()

module type S = sig
  
end

type t = (module S)

type 'a monoid_a = (module Monoid with type t = 'a)

let sumi (type a) ((module A): a monoid_a) (n: a) = A.mappend n A.mempty

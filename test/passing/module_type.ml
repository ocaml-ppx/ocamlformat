module type S = sig
  val x : unit -> unit
end

let get = failwith "TODO"

let foo () =
  let module X = (val get : S) in
  X.x ()

module type S = sig end

type t = (module S)

type 'a monoid_a = (module Monoid with type t = 'a)

type 'a monoid_a = (module Monoid with type F.t = 'a)

let sumi (type a) ((module A) : a monoid_a) (n : a) = A.mappend n A.mempty

module type BAR = sig
  module rec A : (FOO with type t = < b: B.t >)
  and B : FOO
end

module type M =
  module type of M
  with module A := A
  (*test*)
   and module A = A
  (*test*)
   and module A = A
  with module A = A
  (*test*)
  with module A = A

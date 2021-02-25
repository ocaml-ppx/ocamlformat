open Stdune

module type Input = sig
  type t

  val to_dyn : t -> Dyn.t
end

module type S = sig
  type key

  type 'a t

  val create : unit -> 'a t

  val clear : 'a t -> unit

  val set : 'a t -> key -> 'a -> unit

  val find : 'a t -> key -> 'a option
end

module type Instance = sig
  type key

  type value

  type t

  val clear : t -> unit

  val set : t -> key -> value -> unit

  val find : t -> key -> value option

  val store : t
end

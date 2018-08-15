(** test *)
module A = B

include A  (** @open *)

include B  (** @open *)

include A

type t = C of int  (** docstring comment *)

type t = C of int [@ocaml.doc " docstring attribute "]

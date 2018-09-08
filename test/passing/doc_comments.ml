(** test *)
module A = B

(** @open *)
include A

(** @open *)
include B

include A

type t = C of int  (** docstring comment *)

type t = C of int [@ocaml.doc " docstring attribute "]

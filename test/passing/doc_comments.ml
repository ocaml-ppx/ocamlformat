(** test *)
module A = B

include A  (** @open *)

include B  (** @open *)

include A

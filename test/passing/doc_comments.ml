(** test *)
module A = B

(** @open *)
include A

(** @open *)
include B

include A

type t = C of int  (** docstring comment *)

type t = C of int [@ocaml.doc " docstring attribute "]

(** comment *)
include Mod

(** before *)
let x = 2
(** after *)

(**floatting1*)
(**floatting2*)

(**before*)
and y = 2
(** after *)

module A = B
(** test *)

include A
(** @open *)

include B
(** @open *)

include A

type t = C of int  (** docstring comment *)

type t = C of int [@ocaml.doc " docstring attribute "]

include Mod
(** comment *)

(** before *)
let x = 2
(** after *)

(**floatting1*)
(**floatting2*)

(**before*)
and y = 2
(** after *)

(** A *)
let a = 0
(** A' *)

module Comment_placement : sig
  type t
  (** Type *)

  (** Variant declaration *)
  type t = T

  (** Type extension *)
  type t += T

  module A : B
  (** Module *)

  val a : b
  (** Val *)

  exception E
  (** Exception *)

  include M
  (** Include *)

  open M
  (** Open *)

  external a : b = "c"
  (** External *)

  module rec A : B
  (** Rec module *)

  module type A
  (** Module type *)

  class a : b
  (** Class *)

  class type a = b
  (** Class type *)

  (* [@@@some attribute] *)
  (* (** Attribute *) *)

  [%%some extension]  (** Extension *)

  (** A *)
  external a : b = "double_comment"
  (** B *)
end = struct
  type t = {a: int}
  (** Type *)

  (** Variant declaration *)
  type t = T

  (** Type extension *)
  type t += T

  module A = B
  (** Module *)

  (** Let *)
  let a = b

  exception E
  (** Exception *)

  include M
  (** Include *)

  open M
  (** Open *)

  external a : b = "c"
  (** External *)

  module rec A : B = C
  (** Rec module *)

  module type A = B
  (** Module type *)

  class a = b
  (** Class *)

  class b =
    object
      method f = 0
      (** Method *)

      inherit a
      (** Inherit *)

      val x = 1
      (** Val *)

      constraint 'a = [> ]
      (** Constraint *)

      initializer do_init ()
      (** Initialiser *)
    end

  class type a = b
  (** Class type *)

  (* [@@@some attribute] *)
  (* (** Attribute *) *)

  (** Extension *)[%%some
  extension]

  (* ;; *)
  (* (** Eval *) *)
  (* 1 + 1 *)
  (* ;; *)

  (** A *)
  external a : b = "double_comment"
  (** B *)
end

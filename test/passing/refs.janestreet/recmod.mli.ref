module rec A : sig
  type t = AA of B.t
end

and B : sig
  type t = BB of A.t
end

include sig
  (* a *)
end

module type S = sig end

(** A *)
module rec A : S

(** B *)
and B : S

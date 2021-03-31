module rec A : sig
  type t = AA of B.t
end

and B : sig
  type t = BB of A.t
end

include sig
  (* a *)
end

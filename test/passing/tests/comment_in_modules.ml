module M = struct
  (* comments *)
end

module M : sig
  (* comments *)
end = struct
  (* comments *)
end

module type M = sig
  (* comments *)
end

(** Xxxxxxx xxxxxxxx xx xxxxxxx xxxxxxxxxxxxx xxxxxxxxx xx xxxx *)
module Mmmmmmmmmmmmmmmmmmmmmm = Aaaaaaaaaaaaaaaaaaaaaa.Bbbbbbbbbbbbbbbbbbbbbbbb

(** Xxxxxxx xxxxxxxx xx xxxxxxx xxxxxxxxxxxxx xxxxxxxxx xx xxxx *)
module Fffffffffffffff (Yyyyyyyyyyyyyyy : Z.S) = Gggggggggg (Wwwwwwwwww.Make (Yyyyyyyyyy))

module A (* comment *) (A:sig end) : sig end = struct end

module A (A:sig end) (* comment *) (B: sig end) : sig end = struct end

module A (A:sig end) (* comment *) : sig end = struct end

module (* comment *) A (A : sig end) : sig end = struct end

module rec A : A =
  struct

  end

(** floatting *)

and B : B = struct end
(** about b *)


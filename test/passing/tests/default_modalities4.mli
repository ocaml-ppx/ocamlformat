(** cmt *)

@@ moda1 moda2

type t

module type T = sig
  (** cmt *)

  @@ moda1 moda2

  type t

  module T : sig
    (** cmt *)

    @@ moda1 moda2

    type t
  end
end

[@@@ocamlformat "break-struct=natural"]

module M =
  X
    (Y)
    (struct
      let x = k
    end)

(* Here is a comment. *)
module Hash = struct
  include Hash
  module type S = S.HASH
end

(** doc *)

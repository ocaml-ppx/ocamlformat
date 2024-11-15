(* Attributes [[@annot1]] and [[@annot3]] are dropped by the compiler;
   an upcoming patch should fix this. *)

module _ : sig
  val foo : (module T [@annot1] with type t = 'a [@annot2]) -> unit
end = struct
  let foo (type a) (module M : T [@annot3] with type t = a [@annot4]) = ()
end

(* Attributes [[@annot2]] and [[@annot4]] used to be dropped by ocamlformat *)

module _ : sig
  val foo : (module T with type t = 'a [@annot2]) -> unit
end = struct
  let foo (type a) (module M : T with type t = a [@annot4]) = ()
end

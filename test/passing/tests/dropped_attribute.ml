module _ : sig
  val foo : (module T with type t = 'a [@annot1]) -> unit
end = struct
  let foo (type a) (module M : T with type t = a [@annot2]) =
    (module M : T with type t = a [@annot3])
  ;;
end

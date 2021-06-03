module List : sig
  include module type of List

  val find_map : ('a -> 'b option) -> 'a list -> 'b option
  (** @since ocaml-4.10 *)
end

open Result

module Let_syntax : sig
  val ( let+ ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

module Global_scope : sig
  type 'a or_error = ('a, [`Msg of string]) t

  include module type of Let_syntax
end

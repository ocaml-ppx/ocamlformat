module List : sig
  include module type of List

  val find_map : ('a -> 'b option) -> 'a list -> 'b option
  (** @since ocaml-4.10 *)
end

module Misc : sig
  include module type of Misc

  module Color : sig
    include module type of Color

    val default_setting : setting
    (** @since ocaml-4.09 *)
  end

  module Error_style : sig
    include module type of Error_style

    val default_setting : setting
    (** @since ocaml-4.09 *)
  end
end

module List : sig
  include module type of List

  val find_map : ('a -> 'b option) -> 'a list -> 'b option
  (** @since ocaml-4.10 *)
end

module Int : sig
  include module type of Int

  val min : int -> int -> int
  (** @since ocaml-4.13.0 *)

  val max : int -> int -> int
  (** @since ocaml-4.13.0 *)
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

  module Style : sig
    val inline_code: Format.formatter -> string -> unit
    (** @since ocaml-5.2 *)

    val setup : Color.setting option -> unit
    (** @since ocaml-5.2 *)
  end
end

module Clflags : sig
  val include_dirs : string list ref
  val debug : bool ref
  val unsafe : bool ref
  val open_modules : string list ref
  val absname : bool ref
  val use_threads : bool ref
  val principal : bool ref
  val recursive_types : bool ref
  val applicative_functors : bool ref
  val for_package : string option ref
  val transparent_modules : bool ref
  val locations : bool ref
  val color : Misc.Color.setting option ref
  val error_style : Misc.Error_style.setting option ref
  val unboxed_types : bool ref
  val no_std_include : bool ref
end

module Load_path : sig
  type dir
  type auto_include_callback =
    (dir -> string -> string option) -> string -> string
  val init : auto_include:auto_include_callback -> string list -> unit
  val get_paths : unit -> string list
  val auto_include_otherlibs : (string -> unit) -> auto_include_callback
end

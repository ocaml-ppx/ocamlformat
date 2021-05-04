include module type of Fpath

val cwd : unit -> t
(** Current working directory. *)

val exists : t -> bool
(** [exists p] returns whether the given path [p] exists. *)

val to_absolute : t -> t
(** [to_absolute p] returns [cwd]/[p] if the [p] is relative, otherwise
    returns [p]. *)

val to_string : ?relativize:bool -> t -> string
(** If [relativize] is set to [true] (it is set to [false] by default), the
    path is relativized according to the [cwd]. *)

val pp : Format.formatter -> t -> unit

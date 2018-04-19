val init : string -> unit

val string_between : Location.t -> Location.t -> string option

val string_litteral : [`Normalize_nl | `Preserve] -> Location.t -> string

val char_litteral : Location.t -> string

val string_at : Location.t -> string

val begins_line : Location.t -> bool

val ends_line : Location.t -> bool

val sub : pos:int -> len:int -> string

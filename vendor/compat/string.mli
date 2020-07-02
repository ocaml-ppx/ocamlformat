include module type of Base.String

val starts_with_whitespace : string -> bool
(** [starts_with_whitespace s] holds if [s] is non empty and starts with a
    whitespace character. *)

val ends_with_whitespace : string -> bool
(** [ends_with_whitespace s] holds if [s] has at least two characters and
    ends with a whitespace character. *)

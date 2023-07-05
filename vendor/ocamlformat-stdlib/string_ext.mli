include module type of Base.String

val starts_with_whitespace : string -> bool
(** [starts_with_whitespace s] holds if [s] is non empty and starts with a
    whitespace character. *)

val ends_with_whitespace : string -> bool
(** [ends_with_whitespace s] holds if [s] is non empty and ends with a
    whitespace character. *)

val indent_of_line : string -> int option
(** [indent_of_line s] is the indentation at the beginning of [s].
    Returns [None] if the first line of the string is only whitespaces or is empty. *)

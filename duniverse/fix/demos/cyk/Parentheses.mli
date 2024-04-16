open Sigs

(* A sample grammar of well-parenthesized expressions. *)

include CNF

(* The terminal symbols are '(' and ')'. *)

val lpar: terminal
val rpar: terminal

(* Toggling a parenthesis. *)

val toggle: terminal -> terminal

(* Printing a terminal symbol. *)

val print_terminal: terminal -> unit

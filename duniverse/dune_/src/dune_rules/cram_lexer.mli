(** .t file parser *)
open! Dune_engine

(** A command or comment. Output blocks are skipped *)
type 'command block =
  | Command of 'command
  | Comment of string list

val block : Lexing.lexbuf -> string list block option

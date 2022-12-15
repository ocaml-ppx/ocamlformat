(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*              Damien Doligez, projet Para, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Raw printer for {!Parsetree}

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

open Parsetree
open Format

val interface : formatter -> signature_item list -> unit
val implementation : formatter -> structure_item list -> unit
val top_phrase : formatter -> toplevel_phrase -> unit
val repl_phrase : formatter -> repl_phrase -> unit

val expression: formatter -> expression -> unit
val structure: int -> formatter -> structure -> unit
val payload: formatter -> payload -> unit
val core_type: formatter -> core_type -> unit
val module_type: formatter -> module_type -> unit
val pattern: formatter -> pattern -> unit
val type_declaration: formatter -> type_declaration -> unit
val value_binding: formatter -> value_binding -> unit
val module_binding: formatter -> module_binding -> unit
val module_declaration: formatter -> module_declaration -> unit
val class_expr: formatter -> class_expr -> unit
val class_type: formatter -> class_type -> unit
val class_field: formatter -> class_field -> unit
val class_type_field: formatter -> class_type_field -> unit
val module_expr: formatter -> module_expr -> unit
val structure_item: formatter -> structure_item -> unit
val signature_item: formatter -> signature_item -> unit

type cmts =
  { before: Location.t -> string list option
  ; within: Location.t -> string list option
  ; after: Location.t -> string list option }

val cmts : cmts option ref

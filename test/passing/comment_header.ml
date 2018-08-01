(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Common subexpression elimination by value numbering over extended basic
   blocks. *)

open Mach

type valnum = int

(* Classification of operations *)


(* b *)(*******)
       (*     *)
       (*     *)
       (*     *)
       (*     *)
       (*******)(* b *)


(*******)
(*     *)
(*     *)
(*     *)
(*     *)
(*****)

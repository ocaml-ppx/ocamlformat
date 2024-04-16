(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Syntax

(**[check grammar] applies certain well-formedness checks to the grammar
   before parameterized nonterminal symbols are expanded away and before
   [%inline] symbols are eliminated. The failure of a check causes an exit.

   At the moment, only one validity check is performed. We check that the
   [@name] attributes carried by productions are valid OCaml identifiers.
   This property is required by the unparsing API. We require this property
   even if [--unparsing] is off. *)
val check: grammar -> unit

(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* The type [grammar] is defined in [Cmly_format]. *)
open Cmly_format

(* The signature [GRAMMAR] is defined in [Cmly_api]. *)
open Cmly_api

(**This module offers facilities for reading and decoding [.cmly] files.

   We encourage users to work with modules of type [GRAMMAR], which offer a
   high-level description of the grammar and automaton. Values of type
   [grammar], on the other hand, offer a lower-level description.

   Furthermore, the type [grammar], which serves as a definition of the
   [.cmly] file format, may evolve in incompatible ways in the future. *)

(**This exception is raised when a [.cmly] file cannot be read or decoded.
   It carries a human-readable error message. *)
exception Error of string

(**The functor [Read] reads and decodes a [.cmly] file whose name is
   [filename]. It returns a module of type [GRAMMAR], a high-level
   description of the grammar and automaton.
   @raise Error if the file cannot be read or decoded *)
module Read (X : sig val filename : string end) : GRAMMAR

(**The functor [FromString] decodes a [.cmly] file whose content is
   [content]. It returns a module of type [GRAMMAR], a high-level
   description of the grammar and automaton.
   @raise Error if the file cannot be decoded *)
module FromString (X : sig val content : string end) : GRAMMAR

(**[read_channel c] reads and decodes a [.cmly] file via the input channel
   [ic]. It returns a value of type [grammar], a low-level description of
   the grammar and automaton.
   @raise Error if the file cannot be read or decoded *)
val read_channel : in_channel -> grammar

(**The functor [Lift] converts a value of type [grammar] into a module of
   type [GRAMMAR]. *)
module Lift (X : sig val grammar : grammar end) : GRAMMAR

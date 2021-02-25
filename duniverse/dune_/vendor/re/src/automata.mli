(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

(* Regular expressions *)

type mark = int

type sem = [ `Longest | `Shortest | `First ]
type rep_kind = [ `Greedy | `Non_greedy ]

val pp_sem : Format.formatter -> sem -> unit
val pp_rep_kind : Format.formatter -> rep_kind -> unit

type expr
val is_eps : expr -> bool
val pp : Format.formatter -> expr -> unit

type ids
val create_ids : unit -> ids

val cst : ids -> Cset.t -> expr
val empty : ids -> expr
val alt : ids -> expr list -> expr
val seq : ids -> sem -> expr -> expr -> expr
val eps : ids -> expr
val rep : ids -> rep_kind -> sem -> expr -> expr
val mark : ids -> mark -> expr
val pmark : ids -> Pmark.t -> expr
val erase : ids -> mark -> mark -> expr
val before : ids -> Category.t -> expr
val after : ids -> Category.t -> expr

val rename : ids -> expr -> expr

(****)

(* States of the automata *)

type idx = int
module Marks : sig
  type t =
    { marks: (mark * idx) list
    ; pmarks: Pmark.Set.t }
end

module E : sig
  type t
  val pp : Format.formatter -> t -> unit
end

type hash
type mark_infos = int array
type status = Failed | Match of mark_infos * Pmark.Set.t | Running

module State : sig
  type t =
    { idx: idx
    ; category: Category.t
    ; desc: E.t list
    ; mutable status: status option
    ; hash: hash }
  val dummy : t
  val create : Category.t -> expr -> t
  module Table : Hashtbl.S with type key = t
end

(****)

(* Computation of the states following a given state *)

type working_area
val create_working_area : unit -> working_area
val index_count : working_area -> int

val delta : working_area -> Category.t -> Cset.c -> State.t -> State.t
val deriv :
  working_area -> Cset.t -> (Category.t * Cset.t) list -> State.t ->
  (Cset.t * State.t) list

(****)

val status : State.t -> status

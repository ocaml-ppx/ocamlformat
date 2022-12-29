(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* Projection out of a singleton list. *)

val single: 'a list -> 'a

(* A variant of [List.map] where [f] returns a pair of elements,
   to be flattened into the new list. *)

val mapd: ('a -> 'b * 'b) -> 'a list -> 'b list

(* Tabulating a function using an internal array. [tabulate n f]
   returns a function that is extensionally equal to [f], but relies
   on an internal array. Arguments to [f] are of type [int] and are
   supposed to lie in the range [0..n). *)

val tabulate: int -> (int -> 'a) -> (int -> 'a)

(* [sum n f] computes the sum [f 0 + f 1 + ... + f (n-1)]. *)

val sum: int -> (int -> int) -> int

(* [with_buffer n f] creates a fresh buffer of size [n], passes it to [f],
   and returns the final content of the buffer. *)

val with_buffer: int -> (Buffer.t -> unit) -> string

(* [separated_list_to_string printer sep l] converts [l] into a string
   representation built by using [printer] on each element and [sep] as
   a separator. *)

type 'a iter = ('a -> unit) -> unit

val separated_iter_to_string:  ('a -> string) -> string -> 'a iter -> string
val separated_list_to_string:  ('a -> string) -> string -> 'a list -> string

(* If [a] is an array, therefore a mapping of integers to elements, then
   [inverse a] computes its inverse, a mapping of elements to integers.
   The type ['a] of elements must support the use of OCaml's generic
   equality and hashing functions. *)

val inverse: 'a array -> ('a -> int)

(* [support_assoc l x] returns the second component of the first couple
   in [l] whose first component is [x]. If it does not exist, it returns
   [x]. *)

val support_assoc : ('a * 'a) list -> 'a -> 'a

(* Turning an implicit list, stored using pointers through a hash
   table, into an explicit list. The head of the implicit list is
   not included in the explicit list. *)

val materialize: ('a, 'a option) Hashtbl.t -> 'a -> 'a list

(* [iteri] implements a [for] loop over integers, from 0 to
   [n-1]. *)

val iteri:         int -> (int -> unit) -> unit
val iterij: int -> int -> (int -> unit) -> unit

(* [foldij i j f accu] iterates on the semi-open interval [i, j),
   with an accumulator. [foldij_lazy i j f accu] is analogous,
   but is interruptible: if at some point the function [f] does
   not demand its second argument, then iteration stops early.
   [foldij] and [foldij_lazy] iterate in the same direction, from
   left to right, but do not build the accumulator in the same way:
   the calls to [f] are associated differently. (In that respect,
   [foldij] is a left fold, while [foldij_lazy] is a right fold.) *)

(* [foldi] implements a [for] loop over integers, from 0 to [n-1],
   with an accumulator. *)

val foldi:              int -> (int ->          'a  -> 'a) -> 'a -> 'a
val foldij:      int -> int -> (int ->          'a  -> 'a) -> 'a -> 'a
val foldij_lazy: int -> int -> (int -> (unit -> 'a) -> 'a) -> 'a -> 'a

(* [mapij start n f] produces the list [ f start; ... f (n-1) ]. *)

val mapij: int -> int -> (int -> 'a) -> 'a list

(* [mapi n f] produces the list [ f 0; ... f (n-1) ]. *)

val mapi: int -> (int -> 'a) -> 'a list

(* [qfold f accu q] repeatedly takes an element [x] off the queue [q]
   and applies [f] to the accumulator and to [x], until [q] becomes
   empty. Of course, [f] can add elements to [q] as a side-effect. *)

val qfold: ('a -> 'b -> 'a) -> 'a -> 'b Queue.t -> 'a

(* [qiter f q] repeatedly takes an element [x] off the queue [q] and
   applies [f] to [x], until [q] becomes empty. Of course, [f] can add
   elements to [q] as a side-effect. *)

val qiter: ('b -> unit) -> 'b Queue.t -> unit

(* [smap] has the same semantics as [List.map], but attempts to
   physically return the input list when [f] is the identity. *)

val smap: ('a -> 'a) -> 'a list -> 'a list

(* [smapa] is a variant of [smap] that maintains an accumulator. *)

val smapa: ('b -> 'a -> 'b * 'a) -> 'b -> 'a list -> 'b * 'a list

(* [normalize s] returns a copy of [s] where parentheses and commas
   are replaced with underscores. *)

val normalize: string -> string

(* [postincrement r] increments [r] and returns its original value. *)

val postincrement: int ref -> int

(* [mkgensym()] returns a fresh generator of unique integers. *)

val mkgensym : unit -> (unit -> int)

(* [filter_map f l] returns the list of [y]s such that [f x = Some y] where [x]
   is in [l], preserving the order of elements of [l]. *)
val filter_map : ('a -> 'b option) -> 'a list -> 'b list

(* [new_encode_decode capacity] creates a new service for assigning unique
   integer codes to strings. [capacity] is the initial capacity of the
   internal hash table. [new_encode_decode] returns a triple [encode, decode,
   verbose], where [encode] and [decode] translate between strings and unique
   integer codes and [verbose] prints statistics about the use of the service
   so far. *)
val new_encode_decode: int -> (string -> int) * (int -> string) * (unit -> unit)

(* If [preferable] is a partial order on elements, then [best preferable xs]
   returns the best (least) element of [xs], if there is one. Its complexity
   is quadratic. *)

val best: ('a -> 'a -> bool) -> 'a list -> 'a option

(* Assuming that the list [xs] is sorted with respect to the ordering [cmp],
   [levels cmp xs] is the list of levels of [xs], where a level is a maximal
   run of adjacent equal elements. Every level is a nonempty list. *)

val levels: ('a -> 'a -> int) -> 'a list -> 'a list list

(* Suppose [xs] is an arbitrary list of elements. Then [trim (<=) xs] is the
   sublist of the elements of [xs] that are maximal with respect to the
   partial order [<=]. In other words, it is a sublist where every element
   that is less than some other element has been removed. *)

val trim: ('a -> 'a -> bool) -> 'a list -> 'a list

(* Assuming that the list [xs] is sorted with respect to the ordering [cmp],
   [dup cmp xs] returns a duplicate element of the list [xs], if one exists. *)

val dup: ('a -> 'a -> int) -> 'a list -> 'a option

(* [once x y] produces a function [f] which produces [x] the first time it
   is called and produces [y] forever thereafter. *)

val once: 'a -> 'a -> (unit -> 'a)

(* Equality and hashing for lists, parameterized over equality and hashing
   for elements. *)

module ListExtras : sig
  val equal: ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
  val hash: ('a -> int) -> 'a list -> int
end

(* A nice way of printing [n] in English, for concrete values of [n]. *)

val count: int -> string

(* A nice way of printing "nth" in English, for concrete values of [n]. *)

val nth: int -> string

(* [padded_index n i] produces a padded string representation of the index
   [i], which must lie in the semi-open interval [0, n). It is defined in such
   a way that all indices are mapped to strings of equal length. This ensures
   that alphabetical ordering coincides with numeric ordering. *)

val padded_index: int -> int -> string

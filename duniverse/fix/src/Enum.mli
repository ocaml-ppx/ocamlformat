(******************************************************************************)
(*                                                                            *)
(*                                    Fix                                     *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

(**This module offers a few functions that help deal with enumerations. *)

(**A value of type ['a enum] is (a description of, or a producer of) a
   finite sequence of elements of type ['a]. *)
type 'a enum

(**[enum iter] converts the function [iter] into an enumeration. *)
val enum : (('a -> unit) -> unit) -> 'a enum

val foreach : 'a enum -> ('a -> unit) -> unit
(**[foreach] converts an enumeration to an [iter] function. Thus, a loop
   over an enumeration [xs] is written [foreach xs (fun x -> ...)]. *)

val length : 'a enum -> int
(**[length xs] computes and returns the length of the enumeration [xs].
   It runs in linear time. The enumeration [xs] must be persistent. *)

val empty : 'a enum
(**[empty] is an empty enumeration. *)

val cons : 'a -> 'a enum -> 'a enum
(**The enumeration [cons x xs] begins with [x], followed with the elements
   of the enumeration [xs]. *)

val singleton : 'a -> 'a enum
(**The enumeration [singleton x] contains just the element [x]. *)

val list : 'a list -> 'a enum
(**[list xs] is an enumeration of the elements of the list [xs]. *)

val array : 'a array -> 'a enum
(**[array xs] is an enumeration of the elements of the array [xs],
   from left to right. The array is read only when the elements of the
   enumeration are demanded. *)

val enum_to_list : 'a enum -> 'a list
(**[enum_to_list xs] demands the elements of the enumeration [xs] and
   returns a list of these elements. The elements appear in the list in the
   order of their production: that is, the first element of the list is the
   first element that was produced. *)

val enum_to_reversed_list : 'a enum -> 'a list
(**[enum_to_reversed_list xs] demands the elements of the enumeration [xs]
   and returns a list of these elements. The elements appear in the list in
   the reverse order of their production: that is, the first element of the
   list is the last element that was produced. *)

val enum_to_array : 'a enum -> 'a array
(**[enum_to_array xs] demands the elements of the enumeration [xs] and
   returns a (fresh) array of these elements. The elements appear in the
   array in the order of their production: that is, the first element of the
   array is the first element that was produced. *)

val enum_to_reversed_array : 'a enum -> 'a array
(**[enum_to_reversed_array xs] demands the elements of the enumeration [xs]
   and returns a (fresh) array of these elements. The elements appear in the
   array in the reverse order of their production: that is, the first
   element of the array is the last element that was produced. *)

val filter : ('a -> bool) -> 'a enum -> 'a enum
(**[filter f xs] is an enumeration of the elements [x], where [x] ranges
   over [xs] and [f x] is true. *)

val map : ('a -> 'b) -> 'a enum -> 'b enum
(**[map f xs] is the enumeration of the elements [f x], where [x] ranges
   over [xs]. *)

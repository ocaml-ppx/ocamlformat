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

type 'a enum =
  ('a -> unit) -> unit

let[@inline] enum (xs : ('a -> unit) -> unit) : 'a enum =
  xs

let[@inline] foreach (xs : 'a enum) : ('a -> unit) -> unit =
  xs

let[@inline] empty =
  fun _yield ->
    ()

let[@inline] cons (x : 'a) (xs : 'a enum) : 'a enum =
  fun yield ->
    yield x;
    foreach xs yield

let[@inline] singleton x =
  fun yield ->
    yield x

let list (xs : 'a list) : 'a enum =
  fun yield ->
    List.iter yield xs

let array (xs : 'a array) : 'a enum =
  fun yield ->
    Array.iter yield xs

let enum_to_reversed_list (iter : 'a enum) : 'a list =
  let xs = ref [] in
  iter (fun x -> xs := x :: !xs);
  !xs

let enum_to_list (iter : 'a enum) : 'a list =
  List.rev (enum_to_reversed_list iter)

(* [enum_to_array] and [enum_to_reversed_array] use one or two intermediate
   lists. This presents the drawback of allocating intermediate data
   structures, and the advantage of consuming the enumeration only once (which
   could be important, if the enumeration is not persistent). *)

let enum_to_reversed_array (iter : 'a enum) : 'a array =
  Array.of_list (enum_to_reversed_list iter)

let enum_to_array (iter : 'a enum) : 'a array =
  Array.of_list (enum_to_list iter)

let length (iter : 'a enum) : int =
  let c = ref 0 in
  iter (fun _x -> incr c);
  !c

let filter (f : 'a -> bool) (iter : 'a enum) : 'a enum =
  fun yield ->
    iter @@ fun x ->
      if f x then yield x

let map (f : 'a -> 'b) (iter : 'a enum) : 'b enum =
  fun yield ->
    iter @@ fun x ->
      yield (f x)

(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Binary tree uchar ranges maps. *)

type 'a tree =
  | Empty
  | R of int * int * 'a
  | Rn of 'a tree * 'a tree * int * int * 'a

type 'a t = { default : 'a; tree : 'a tree }

let get m cp =
  let rec loop cp = function
  | Rn (l, r, is, ie, v) ->
      if cp < is then loop cp l else
      if cp > ie then loop cp r else
      v
  | R (is, ie, v) ->
      if cp < is then m.default else
      if cp > ie then m.default else
      v
  | Empty -> m.default
  in
  loop cp m.tree

let of_sorted_list default l =                           (* perfect balance. *)
  let rec loop len l =
    if len = 1 then match l with
    | `R (is, ie, v) :: r -> R (is, ie, v), r
    | _ -> assert false
    else
    let len_ll = len / 2 in
    let len_rl = len - len_ll in
    let ltree, rlist = loop len_ll l in
    match rlist with
    | [] -> ltree, []
    | `R (is, ie, v) :: r ->
        if len_rl = 1 then Rn (ltree, Empty, is, ie, v), r else
        let rtree, rlist = loop (len_rl - 1) r in
        Rn (ltree, rtree, is, ie, v), rlist
  in
  let keep acc (`R (_, _, v) as p) = if v <> default then p :: acc else acc in
  let l = List.rev (List.fold_left keep [] l) in
  let len = List.length l in
  let tree = if len = 0 then Empty else fst (loop len l) in
  { default; tree }

let height m =
  let rec loop = function
  | Empty -> 0
  | R _ -> 1
  | Rn (l, r, _, _, _) -> 1 + max (loop l) (loop r)
  in
  loop m.tree

let rec word_size v_size m =        (* value sharing not taken into account. *)
  let rec loop = function
  | Empty -> 0
  | R (_, _, v) -> 4 + v_size v
  | Rn (l, r, _, _, v) -> 6 + loop l + loop r + v_size v
  in
  loop m.tree

let iter_values f m =
  let rec loop f = function
  | Empty -> ()
  | R (_, _, v) -> f v
  | Rn (l, r, _,  _, v) -> f v; loop f l; loop f r
  in
  f m.default; loop f m.tree

let rec dump pp_v ppf m =
  let open Uucp_fmt in
  let rec dump_tree ppf = function
  | Rn (l, r, is, ie, v) ->
      pf ppf "@[<4>Rn(%a,@,%a,@,0x%04X,@,0x%04X,@,%a)@]"
        dump_tree l dump_tree r is ie pp_v v
  | R (is, ie, v) ->
      pf ppf "@[<3>R(0x%04X,@,0x%04X,@,%a)@]" is ie pp_v v
  | Empty ->
      pf ppf "Empty"
  in
  record ["default", pp_v; "tree", dump_tree] ppf m.default m.tree

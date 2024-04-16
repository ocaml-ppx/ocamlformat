(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* uchar to arbitrary value trie maps *)

type 'a t =
  { default : 'a;                                      (* default value. *)
    l0 : 'a array array array }       (* 0x1FFFFF as 0x1FF - 0xFF - 0xF. *)

let nil = [||]
let l0_shift = 12
let l0_size = 0x10F + 1
let l1_shift = 4
let l1_mask = 0xFF
let l1_size = 0xFF + 1
let l2_mask = 0xF
let l2_size = 0xF + 1

let create default = { default; l0 = Array.make l0_size nil }

let get m u =
  let l1 = Array.unsafe_get m.l0 (u lsr l0_shift) in
  if l1 == nil then m.default else
  let l2 = Array.unsafe_get l1 (u lsr l1_shift land l1_mask) in
  if l2 == nil then m.default else
  Array.unsafe_get l2 (u land l2_mask)

let set m u v =
  if v = m.default then () else
  let i = u lsr l0_shift in
  if m.l0.(i) == nil then m.l0.(i) <- Array.make l1_size nil;
  let j = u lsr l1_shift land l1_mask in
  if m.l0.(i).(j) == nil then m.l0.(i).(j) <- Array.make l2_size m.default;
  m.l0.(i).(j).(u land l2_mask) <- v

let word_size v_size m = match m.l0 with
| [||] -> 3 + 1 + v_size m.default
| l0 ->
    let size = ref (3 + v_size m.default + 1 + Array.length l0) in
    for i = 0 to Array.length l0 - 1 do match l0.(i) with
    | [||] -> ()
    | l1 ->
        size := !size + (1 + Array.length l1);
        for j = 0 to Array.length l1 - 1 do match l1.(j) with
        | [||] -> ()
        | l2 ->
            size := !size + (1 + Array.length l2);
            for k = 0 to Array.length l2 - 1 do
              size := !size + v_size l2.(k)
            done;
        done;
    done;
    !size

let dump pp_v ppf m =
  let open Uucp_fmt in
  record ["default", pp_v; "l0", pp_v |> array_N |> array_N |> array]
         ppf m.default m.l0

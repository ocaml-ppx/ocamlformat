(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* uchar to 5 bytes trie maps. *)

let str = Printf.sprintf
let err_default l = str "default value length is %d, must be at least 5" l

type t =
  { default : string;                                  (* default value. *)
    l0 : string array array }          (* 0x1FFFFF as 0x1FF - 0xF - 0xFF *)

let nil = [||]
let snil = ""
let l0_shift = 12
let l0_size = 0x10F + 1
let l1_shift = 8
let l1_mask = 0xF
let l1_size = 0xF + 1
let l2_mask = 0xFF
let l2_size = (0xFF + 1) * 5

let create default =
  let dlen = String.length default in
  if dlen >= 5 then { default; l0 = Array.make l0_size nil } else
  invalid_arg (err_default dlen)

let word_size m = match m.l0 with
| [||] -> 3 + 4 + 1
| l0 ->
    let size = ref (3 + 4 + 1 + Array.length l0) in
    for i = 0 to Array.length l0 - 1 do match l0.(i) with
    | [||] -> ()
    | l1 ->
        size := !size + 1 + Array.length l1;
        for j = 0 to Array.length l1 - 1 do
          size := !size + 1 + ((String.length l1.(j) * 8) / Sys.word_size)
        done;
    done;
    !size

let iter_blobs i m = Array.(iter (iter i)) m.l0

let dump_pp pp_v ppf m =
  let open Uucp_fmt in
  record ["default", string_X; "l0", pp_v |> array_N |> array]
    ppf m.default m.l0

let pp_v = Uucp_fmt.string_XN
let dump = dump_pp pp_v

(* Five bytes as an uint20 pair *)

let create_uint20_pair (d0, d1) =
  let default = Bytes.create 5 in
  Bytes.set default 0 (Char.unsafe_chr ((d0 land 0xFF)));
  Bytes.set default 1 (Char.unsafe_chr ((d0 lsr 8 land 0xFF)));
  Bytes.set default 2 (Char.unsafe_chr
                         ((d0 lsr 12 land 0xF0) lor
                          (d1 lsr 16 land 0x0F)));
  Bytes.set default 3 (Char.unsafe_chr ((d1 lsr 8 land 0xFF)));
  Bytes.set default 4 (Char.unsafe_chr ((d1 land 0xFF)));
  create (Bytes.unsafe_to_string default)

let get_uint20_pair m u =
  let l1 = Array.unsafe_get m.l0 (u lsr l0_shift) in
  let s, k =
    if l1 == nil then m.default, 0 else
    let l2 = Array.unsafe_get l1 (u lsr l1_shift land l1_mask) in
    if l2 == snil then m.default, 0 else
    l2, (u land l2_mask) * 5
  in
  let i00 = Char.code (String.unsafe_get s (k    )) in
  let i01 = Char.code (String.unsafe_get s (k + 1)) in
  let im  = Char.code (String.unsafe_get s (k + 2)) in
  let i11 = Char.code (String.unsafe_get s (k + 3)) in
  let i10 = Char.code (String.unsafe_get s (k + 4)) in
  let i0 = ((im land 0xF0) lsl 12) lor (i01 lsl 8) lor i00 in
  let i1 = ((im land 0x0F) lsl 16) lor (i11 lsl 8) lor i10 in
  i0, i1

let set_uint20_pair m u (i0, i1) =
  let l2_make m =
    let s = Bytes.create l2_size in
    for i = 0 to l2_size - 1 do Bytes.set s i (m.default.[i mod 4]) done;
    s
  in
  let d0 =
    ((Char.code m.default.[2] land 0xF0) lsl 12) lor
    (Char.code m.default.[1] lsl 8)
    lor (Char.code m.default.[0])
  in
  let d1 =
    ((Char.code m.default.[2] land 0x0F) lsl 16) lor
    (Char.code m.default.[3] lsl 8) lor (Char.code m.default.[4])
  in
  if d0 = i0 && d1 = i1 then () else
  let i = u lsr l0_shift in
  if m.l0.(i) == nil then m.l0.(i) <- Array.make l1_size snil;
  let j = u lsr l1_shift land l1_mask in
  if m.l0.(i).(j) == snil then
    m.l0.(i).(j) <- Bytes.unsafe_to_string (l2_make m);
  let k = (u land l2_mask) * 5 in
  let s = Bytes.unsafe_of_string (m.l0.(i).(j)) in
  Bytes.set s (k    ) (Char.unsafe_chr ((i0 land 0xFF)));
  Bytes.set s (k + 1) (Char.unsafe_chr ((i0 lsr 8 land 0xFF)));
  Bytes.set s (k + 2) (Char.unsafe_chr
                         ((i0 lsr 12 land 0xF0) lor
                          (i1 lsr 16 land 0x0F)));
  Bytes.set s (k + 3) (Char.unsafe_chr ((i1 lsr 8 land 0xFF)));
  Bytes.set s (k + 4) (Char.unsafe_chr ((i1 land 0xFF)));
  ()

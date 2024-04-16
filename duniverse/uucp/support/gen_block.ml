(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let pp_block ppf b = Gen.pp ppf "`%a" Uucp_block_base.pp b
let pp_block_option ppf = function
| None -> Gen.pp ppf "None"
| Some b -> Gen.pp ppf "@[<1>(Some@ %a)@]" pp_block b

let block_prop ucd u = match Gen.ucd_get ucd u Uucd.block with
| `High_Surrogates -> assert false
| `Low_Surrogates -> assert false
| `High_PU_Surrogates -> assert false
| #Uucp_block_base.t as b -> b

let pp_block_prop ppf ucd =
  let size v = 0 in
  let prop u = block_prop ucd u in
  Gen.pp_prop_rmap ~share:false ppf prop "block" "Uucp_block_base.t" pp_block
    ~default:`NB size

let pp_blocks ppf ucd =
  let ranges = Gen.prop_find_ranges (block_prop ucd) in
  let not_nb (`R (_, _, b)) = b <> `NB in
  let ranges = List.find_all not_nb ranges in
  let pp_block ppf (`R (is,ie,b)) =
    Gen.pp ppf "@[<1>(%a,@,(%a,@,%a))@]"
      pp_block b Gen.pp_uchar is Gen.pp_uchar ie
  in
  Gen.pp ppf "@[let block_list : \
              (Uucp_block_base.t * (Uchar.t * Uchar.t)) list =\
              @\n %a@]@\n"
    (Gen.pp_list pp_block) ranges

let pp_props ppf ucd =
  pp_block_prop ppf ucd;
  pp_blocks ppf ucd;
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd

(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let pp_age ppf ucd =
  let size = function
  | `Unassigned -> 0
  | `Version _ -> 3
  in
  let pp_age ppf = function
  | `Unassigned -> Gen.pp ppf "`Unassigned"
  | `Version (maj, min) -> Gen.pp ppf "(`Version (%d,%d))" maj min
  in
  Gen.pp_prop_rmap_ucd ppf ucd
    Uucd.age "age" "[ `Unassigned | `Version of int * int ]" pp_age
    ~default:`Unassigned size

let pp_props ppf ucd =
  pp_age ppf ucd;
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd

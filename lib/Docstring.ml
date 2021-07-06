(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

let parse ~loc text =
  let location = loc.Location.loc_start in
  let location =
    { location with
      pos_cnum= location.pos_cnum + 3 (* Length of comment opening *) }
  in
  let v = Odoc_parser.parse_comment ~location ~text in
  match Odoc_parser.warnings v with
  | [] -> Ok (Odoc_parser.ast v)
  | warnings -> Error warnings

let warn fmt warning =
  Format.fprintf fmt "Warning: Invalid documentation comment:@,%s\n%!"
    (Odoc_parser.Warning.to_string warning)

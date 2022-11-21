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

type t = {lower: int; upper: int; whole: bool}

let make ?range source =
  let lines_nb = List.length @@ String.split_lines source in
  match range with
  | Some (lower, upper)
    when 1 <= lower && lower <= upper && upper <= lines_nb + 1 ->
      (* the last line of the buffer (lines_nb + 1) should be valid *)
      let whole = lower = 1 && upper >= lines_nb in
      {lower; upper; whole}
  | _ -> {lower= 1; upper= lines_nb; whole= true}

let get t = (t.lower, t.upper)

let is_whole t = t.whole

let conv =
  let open Cmdliner in
  let pair_conv = Arg.(pair ~sep:'-' int int) in
  let parse x =
    let+ range = Arg.conv_parser pair_conv x in
    Ok (make ~range)
  in
  let pp fs x =
    match x "this string is not important" with
    | {whole= true; _} -> Format.fprintf fs "<whole input>"
    | {lower; upper; _} -> Arg.conv_printer pair_conv fs (lower, upper)
  in
  Arg.conv (parse, pp)

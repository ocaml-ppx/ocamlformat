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

open Result.Monad_infix

let make ?range source =
  let lines_nb = List.length @@ String.split_lines source in
  match range with
  | Some (lower, upper)
    when 1 <= lower && lower <= upper && upper <= lines_nb ->
      let whole = lower = 1 && lower = lines_nb in
      {lower; upper; whole}
  | _ -> {lower= 1; upper= lines_nb; whole= true}

let get t = (t.lower, t.upper)

let conv =
  let open Cmdliner in
  let pair_conv = Arg.(pair ~sep:'-' int int) in
  let parse x = Arg.conv_parser pair_conv x >>| fun range -> make ~range in
  let pp fs x =
    match x "" with
    | {whole= true; _} -> Format.fprintf fs "<whole input>"
    | {lower; upper; _} -> Arg.conv_printer pair_conv fs (lower, upper)
  in
  Arg.conv (parse, pp)

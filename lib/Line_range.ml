(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

type t = All | Only_between of {low: int; high: int}

let all = All

let only_between ~low ~high = Only_between {low; high}

let pp_hum ppf = function
  | All -> Format.fprintf ppf "(all)"
  | Only_between {low; high} -> Format.fprintf ppf "%d-%d" low high

let index_nth s c i =
  let rec go start i =
    if i < 0 then None
    else if i = 1 then String.index_from s start c
    else
      match String.index_from s start c with
      | None -> None
      | Some new_start -> go (new_start + 1) (i - 1)
  in
  go 0 i

let split_lines ~low ~high source =
  index_nth source '\n' (low - 1)
  >>= fun s_pos ->
  index_nth source '\n' high
  >>= fun e_pos ->
  let p1 = s_pos + 1 in
  let p2 = e_pos + 1 in
  Some
    ( String.subo source ~len:p1
    , String.sub source ~pos:p1 ~len:(p2 - p1)
    , String.subo source ~pos:p2 )

let apply range ~f ~on_invalid source =
  let open Result.Monad_infix in
  match range with
  | All -> f source
  | Only_between {low; high} -> (
    match split_lines ~low ~high source with
    | None -> Error on_invalid
    | Some (before, source, after) ->
        f source
        >>| fun fmted_between -> String.concat [before; fmted_between; after]
    )

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

let normalize_eol ?(exclude_locs = []) ~line_endings s =
  let buf = Buffer.create (String.length s) in
  let add_cr n = Buffer.add_string buf (String.make n '\r') in
  let rec normalize_segment ~seen_cr i stop =
    if i = stop then add_cr seen_cr
    else
      match s.[i] with
      | '\r' -> normalize_segment ~seen_cr:(seen_cr + 1) (i + 1) stop
      | '\n' ->
          ( match line_endings with
          | `Crlf -> Buffer.add_char buf '\r'
          | `Lf -> () ) ;
          Buffer.add_char buf '\n' ;
          normalize_segment ~seen_cr:0 (i + 1) stop
      | c ->
          add_cr seen_cr ;
          Buffer.add_char buf c ;
          normalize_segment ~seen_cr:0 (i + 1) stop
  in
  let rec loop locs i =
    match locs with
    | [] ->
        normalize_segment ~seen_cr:0 i (String.length s) ;
        Buffer.contents buf
    | (start, stop) :: xs ->
        normalize_segment ~seen_cr:0 i start ;
        Buffer.add_substring buf s ~pos:start ~len:(stop - start) ;
        loop xs stop
  in
  loop exclude_locs 0

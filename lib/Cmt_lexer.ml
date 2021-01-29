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

type token = Cmt of string Location.loc | S of string Location.loc

open Location

let inc_pos (pos : Lexing.position) = {pos with pos_cnum= pos.pos_cnum + 1}

let nl_pos (pos : Lexing.position) =
  { pos with
    pos_lnum= pos.pos_lnum + 1
  ; pos_bol= pos.pos_cnum + 1
  ; pos_cnum= pos.pos_cnum + 1 }

let update_pos pos input ~start_i ~end_i =
  let rec aux pos i =
    if i < end_i then
      match String.unsafe_get input i with
      | '\n' -> aux (nl_pos pos) (i + 1)
      | _ -> aux (inc_pos pos) (i + 1)
    else pos
  in
  (aux pos start_i, String.sub ~pos:start_i ~len:(end_i - start_i) input)

let loc_ghost = false

let lex_comments input =
  let pos_fname = !Location.input_name in
  let rec aux acc (pos : Lexing.position) =
    if pos.pos_cnum >= String.length input then acc
    else
      match String.substr_index ~pos:pos.pos_cnum ~pattern:"(*" input with
      | Some opn -> (
          let acc, pos =
            if opn = pos.pos_cnum then (acc, pos)
            else
              let pos', s =
                update_pos pos input ~start_i:pos.pos_cnum ~end_i:opn
              in
              let loc = {loc_ghost; loc_start= pos; loc_end= pos'} in
              (S (Location.mkloc s loc) :: acc, pos')
          in
          match String.substr_index ~pos:opn ~pattern:"*)" input with
          | Some cls ->
              let pos', cmt =
                update_pos pos input ~start_i:opn ~end_i:(cls + 2)
              in
              let loc = {loc_ghost; loc_start= pos; loc_end= pos'} in
              let acc = Cmt (Location.mkloc cmt loc) :: acc in
              aux acc pos'
          | None ->
              let pos', cmt =
                update_pos pos input ~start_i:opn
                  ~end_i:(String.length input)
              in
              let loc = {loc_ghost; loc_start= pos; loc_end= pos'} in
              Cmt (Location.mkloc cmt loc) :: acc )
      | None ->
          let pos', s =
            update_pos pos input ~start_i:pos.pos_cnum
              ~end_i:(String.length input)
          in
          let loc = {loc_ghost; loc_start= pos; loc_end= pos'} in
          S (Location.mkloc s loc) :: acc
  in
  List.rev (aux [] {pos_fname; pos_lnum= 0; pos_bol= 0; pos_cnum= 0})

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

let find_semisemi_start ?pos txt =
  String.substr_index_all ~may_overlap:false ~pattern:";;" txt
  |> fun l ->
  ( match pos with
  | Some pos -> List.filter l ~f:(fun x -> x <= pos - 2)
  | None -> l )
  |> List.last

let find_semisemi_end ?pos txt =
  String.substr_index_all ~may_overlap:false ~pattern:";;" txt
  |> fun l ->
  ( match pos with
  | Some pos -> List.filter l ~f:(fun x -> x > pos)
  | None -> l )
  |> List.hd

let find_nl_start ?pos txt =
  let followed_by_space x =
    match String.get txt (x + 2) with
    | exception _ -> true
    | ' ' -> false
    | _ -> true
  in
  String.substr_index_all ~may_overlap:false ~pattern:"\n\n" txt
  |> fun l ->
  ( match pos with
  | Some pos ->
      List.filter l ~f:(fun x -> x <= pos - 2 && followed_by_space x)
  | None -> List.filter l ~f:followed_by_space )
  |> List.last

let find_nl_end ?pos txt =
  let followed_by_space x =
    match String.get txt (x + 2) with
    | exception _ -> true
    | ' ' -> false
    | _ -> true
  in
  String.substr_index_all ~may_overlap:false ~pattern:"\n\n" txt
  |> fun l ->
  ( match pos with
  | Some pos -> List.filter l ~f:(fun x -> x > pos && followed_by_space x)
  | None -> List.filter l ~f:followed_by_space )
  |> List.hd

let sub_start i (loc : Location.t) =
  String.sub ~pos:(i + 2)
    ~len:(loc.loc_end.pos_cnum - loc.loc_start.pos_cnum - i - 2)

let sub_end i = String.sub ~pos:0 ~len:(i - 1)

let find_start ?pos (lexed : Cmt_lexer.token array) ~start_i
    ~split_on_semisemi =
  let rec aux ?pos i =
    if i < 0 then
      match Array.unsafe_get lexed 0 with
      | Cmt {txt; _} -> (0, txt, 0, 0)
      | S {txt; _} -> (0, txt, 0, 0)
    else
      let lex = Array.unsafe_get lexed i in
      match lex with
      | Cmt _ -> aux (i - 1)
      | S {txt; loc} -> (
          let ldiff max_cnum =
            let rec aux lnum i =
              if i >= max_cnum then lnum
              else
                match String.unsafe_get txt i with
                | '\n' -> aux (lnum + 1) (i + 1)
                | _ -> aux lnum (i + 1)
            in
            aux loc.loc_start.pos_lnum 0
          in
          if split_on_semisemi then
            match find_semisemi_start ?pos txt with
            | Some x -> (
              match find_nl_start ?pos txt with
              | Some y ->
                  let x = max x y in
                  let cnum = x + 2 in
                  let ldiff = ldiff cnum in
                  (i, sub_start x loc txt, cnum, ldiff)
              | None ->
                  let cnum = x + 2 in
                  let ldiff = ldiff cnum in
                  (i, sub_start x loc txt, cnum, ldiff) )
            | None -> (
              match find_nl_start ?pos txt with
              | Some x ->
                  let cnum = x + 2 in
                  let ldiff = ldiff cnum in
                  (i, sub_start x loc txt, cnum, ldiff)
              | None -> aux (i - 1) )
          else
            match find_nl_start ?pos txt with
            | Some x ->
                let cnum = x + 2 in
                let ldiff = ldiff cnum in
                (i, sub_start x loc txt, cnum, ldiff)
            | None -> aux (i - 1) )
  in
  aux ?pos start_i

let find_end ?pos (lexed : Cmt_lexer.token array) ~end_i ~split_on_semisemi =
  let max_i = Array.length lexed - 1 in
  let rec aux ?pos i =
    if i > max_i then
      match Array.unsafe_get lexed max_i with
      | Cmt {txt; _} -> (max_i, txt, String.length txt - 1)
      | S {txt; _} -> (max_i, txt, String.length txt - 1)
    else
      let lex = Array.unsafe_get lexed i in
      match lex with
      | Cmt _ -> aux (i + 1)
      | S {txt; loc= _} -> (
          if split_on_semisemi then
            match find_semisemi_end ?pos txt with
            | Some x -> (
              match find_nl_end ?pos txt with
              | Some y ->
                  let x = min x y in
                  (i, sub_end x txt, x - 1)
              | None -> (i, sub_end x txt, x - 1) )
            | None -> (
              match find_nl_end ?pos txt with
              | Some x -> (i, sub_end x txt, x - 1)
              | None -> aux (i + 1) )
          else
            match find_nl_end ?pos txt with
            | Some x -> (i, sub_end x txt, x - 1)
            | None -> aux (i + 1) )
  in
  aux ?pos end_i

let split ~range:((low, high) as range) ~split_on_semisemi = function
  | [] -> ("", range)
  | lexed ->
      let lexed : Cmt_lexer.token array = Array.of_list lexed in
      let high =
        match Array.unsafe_get lexed (Array.length lexed - 1) with
        | Cmt {loc; _} | S {loc; _} ->
            if high - 1 = loc.loc_end.pos_lnum + 1 then high - 1 else high
      in
      let range = (low, high) in
      let start_i, start_lex =
        Array.findi_exn lexed ~f:(fun _ -> function
          | Cmt {loc; _} | S {loc; _} ->
              loc.loc_start.pos_lnum <= low - 1
              && low - 1 <= loc.loc_end.pos_lnum )
      in
      let end_i, end_lex =
        Array.findi_exn lexed ~f:(fun _ -> function
          | Cmt {loc; _} | S {loc; _} ->
              loc.loc_start.pos_lnum <= high - 1
              && high - 1 <= loc.loc_end.pos_lnum )
      in
      let start_i, start_lex, start_pos, ldiff =
        match start_lex with
        | S {txt; loc} ->
            let rec aux ~lnum i =
              if lnum = low - 1 then i
              else
                match String.unsafe_get txt i with
                | '\n' -> aux ~lnum:(lnum + 1) (i + 1)
                | _ -> aux ~lnum (i + 1)
            in
            let pos = aux 0 ~lnum:loc.loc_start.pos_lnum in
            find_start ~pos ~start_i ~split_on_semisemi lexed
        | Cmt _ -> find_start ~start_i ~split_on_semisemi lexed
      in
      let end_i, end_lex, end_pos =
        match end_lex with
        | S {txt; loc} ->
            let rec aux ~lnum i =
              if lnum = high - 1 then i
              else
                match String.unsafe_get txt i with
                | '\n' -> aux ~lnum:(lnum + 1) (i + 1)
                | _ -> aux ~lnum (i + 1)
            in
            let pos = aux 0 ~lnum:loc.loc_start.pos_lnum in
            find_end ~pos ~end_i ~split_on_semisemi lexed
        | Cmt _ -> find_end ~end_i ~split_on_semisemi lexed
      in
      if start_i = end_i then
        match Array.unsafe_get lexed start_i with
        | Cmt {txt; _} -> (txt, range)
        | S {txt; _} ->
            ( String.sub txt ~pos:start_pos ~len:(end_pos - start_pos + 1)
            , (low - ldiff, high - ldiff) )
      else
        let rec aux acc i =
          if i = end_i then acc ^ end_lex
          else
            match Array.unsafe_get lexed i with
            | Cmt {txt; _} -> aux (acc ^ txt) (i + 1)
            | S {txt; _} -> aux (acc ^ txt) (i + 1)
        in
        (aux start_lex (start_i + 1), (low - ldiff, high - ldiff))

let fragment (type a) (fg : a Extended_ast.t) ~range input =
  let split_on_semisemi =
    match fg with
    | Structure | Use_file -> true
    | Signature -> false
    | Core_type -> failwith "Slicer.fragment not implemented for Core_type"
    | Module_type ->
        failwith "Slicer.fragment not implemented for Module_type"
    | Expression -> failwith "Slicer.fragment not implemented for Expression"
    | Repl_file -> failwith "Slicer.fragment not implemented for Repl_file"
  in
  Cmt_lexer.lex_comments input |> split ~range ~split_on_semisemi

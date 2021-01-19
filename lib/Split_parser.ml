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

open Migrate_ast

let map_flatten l ~f =
  List.rev
  @@ List.fold_left ~init:[] l ~f:(fun acc str ->
         List.rev_append (f str) acc )

let rec first_non_empty = function
  | [] -> None
  | "" :: t -> first_non_empty t
  | h :: _ -> Some h

module Line = struct
  let parse str =
    let lexbuf = Lexing.from_string str in
    let rec loop acc =
      match Lexer.token_with_comments lexbuf with
      | Parser.EOF -> List.rev (Parser.EOF :: acc)
      | tok -> loop (tok :: acc)
    in
    loop []

  let starts_new_item str =
    match parse str with
    | [] -> false
    | (LET | MODULE | CLASS | TYPE | EXCEPTION) :: _ -> true
    | _ -> false

  let expects_followup str =
    match List.rev (parse str) with
    | [] -> false
    | (IN | LPAREN | LBRACKET | STRUCT | SIG | BEGIN) :: _ -> true
    | _ -> false

  let starts_with tok str =
    match parse str with t :: _ when Poly.(t = tok) -> true | _ -> false

  let indent x = String.(length x - length (lstrip x))

  (* only oneliners for now *)
  let is_cmt x =
    String.is_prefix x ~prefix:"(*" && String.is_suffix x ~suffix:"*)"
end

module Cmt_lexer = struct
  type token = Cmt of string | Other of string

  let lex_comments input =
    let rec aux acc pos =
      if pos >= String.length input then acc
      else
        match String.substr_index ~pos ~pattern:"(*" input with
        | Some opn -> (
            let acc =
              if opn = pos then acc
              else Other (String.sub ~pos ~len:(opn - pos) input) :: acc
            in
            match String.substr_index ~pos:opn ~pattern:"*)" input with
            | Some cls ->
                aux
                  (Cmt (String.sub ~pos:opn ~len:(cls - opn + 2) input)
                   :: acc )
                  (cls + 2)
            | None ->
                Cmt
                  (String.sub ~pos:opn
                     ~len:(String.length input - opn)
                     input )
                :: acc )
        | None ->
            Other (String.sub ~pos ~len:(String.length input - pos) input)
            :: acc
    in
    List.rev (aux [] 0)
end

module Split = struct
  let concat str = function [] -> [str] | x :: xs -> (x ^ str) :: xs

  let add_all ~from ~to_:init =
    List.fold_left from ~init ~f:(fun acc x -> x :: acc)

  let split_on_linebreaks lexed =
    let rec aux (acc, break) = function
      | [] -> if break then "" :: acc else acc
      | Cmt_lexer.Cmt x :: xs ->
          aux ((if break then x :: acc else concat x acc), false) xs
      | Cmt_lexer.Other x :: xs -> (
        match Astring.String.cuts ~rev:false ~empty:true ~sep:"\n" x with
        | [] -> impossible "should at least contain an empty string"
        | x :: others as cuts -> (
          match List.last_exn cuts with
          | "" -> (
            match List.rev (List.tl_exn (List.rev cuts)) with
            | [] -> aux (acc, true) xs
            | x :: others ->
                let acc = add_all ~from:others ~to_:(concat x acc) in
                aux (acc, true) xs )
          | _ ->
              let acc = add_all ~from:others ~to_:(concat x acc) in
              aux (acc, false) xs ) )
    in
    List.rev (aux ([], false) lexed)

  let split input ~f =
    Cmt_lexer.lex_comments input
    |> split_on_linebreaks
    |> List.fold_left ~init:([], []) ~f
    |> (fun (ret, prev_lines) ->
         (List.rev prev_lines |> Astring.String.concat ~sep:"\n") :: ret )
    |> List.map ~f:String.strip
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.rev

  let split_according_to_tokens input =
    split input ~f:(fun (ret, prev_lines) line ->
        match first_non_empty prev_lines with
        | None -> (ret, [line])
        | Some last -> (
          match first_non_empty (List.rev prev_lines) with
          | None -> impossible "filtered by previous pattern matching"
          | Some first ->
              if
                Line.starts_new_item line
                && Line.indent line = Line.indent first
                && Line.indent line <= Line.indent last
                && not (Line.expects_followup last)
              then
                match prev_lines with
                | cmt :: "" :: prev_lines when Line.is_cmt cmt ->
                    ( (List.rev prev_lines |> String.concat ~sep:"\n") :: ret
                    , [line; cmt] )
                | _ ->
                    ( (List.rev prev_lines |> String.concat ~sep:"\n") :: ret
                    , [line] )
              else (ret, line :: prev_lines) ) )

  let split_according_to_semisemi input =
    split input ~f:(fun (ret, prev_lines) line ->
        if Line.starts_with Parser.SEMISEMI line && Line.indent line = 0 then
          ((List.rev prev_lines |> String.concat ~sep:"\n") :: ret, [line])
        else (ret, line :: prev_lines) )

  let split_toplevel input =
    split_according_to_semisemi input
    |> map_flatten ~f:split_according_to_tokens

  let fragment (type a) (fg : a Traverse.fragment) input =
    match fg with
    | Structure -> split_toplevel input
    | Use_file -> split_toplevel input
    | Signature -> split_according_to_tokens input
end

module Recover = struct
  let append x y = match x with "" -> y | _ -> x ^ "\n\n" ^ y

  let sep = "_i_n_v_a_l_i_d_"

  let fragment (type a) (fg : a Traverse.fragment) input =
    Split.fragment fg input
    |> List.fold_left ~init:"" ~f:(fun acc item ->
           match Parse.fragment fg (Lexing.from_string item) with
           | exception _ ->
               append acc
                 (Format.sprintf "[%%%%invalid.ast.node {%s|%s|%s}]" sep item
                    sep )
           | _parsed -> append acc item )
end

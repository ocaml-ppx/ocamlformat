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
         List.rev_append (f str) acc)

let rec first_non_empty = function
  | [] -> None
  | "" :: t -> first_non_empty t
  | h :: _ -> Some h

module Token = struct
  let parse str =
    let lexbuf = Lexing.from_string str in
    let rec loop acc =
      match Lexer.token lexbuf with
      | Parser.EOF -> List.rev acc
      | tok -> loop (tok :: acc)
    in
    loop []
end

module Line = struct
  let starts_new_item str =
    match Token.parse str with
    | [] -> false
    | (LET | AND | MODULE | CLASS | TYPE | EXCEPTION) :: _ -> true
    | _ -> false

  let expects_followup str =
    match List.rev (Token.parse str) with
    | [] -> false
    | (IN | LPAREN | LBRACKET | STRUCT | SIG | BEGIN) :: _ -> true
    | _ -> false

  let indent x = String.(length x - length (lstrip x))
end

module Split = struct
  let split_according_to_tokens input =
    Astring.String.cuts ~rev:false ~empty:true ~sep:"\n" input
    |> List.fold_left
         ~f:(fun (ret, prev_lines) line ->
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
                   ( (List.rev prev_lines |> String.concat ~sep:"\n") :: ret
                   , [line] )
                 else (ret, line :: prev_lines) ))
         ~init:([], [])
    |> (fun (ret, prev_lines) ->
         (List.rev prev_lines |> Astring.String.concat ~sep:"\n") :: ret)
    |> List.map ~f:String.strip
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.rev

  let split_according_to_semisemi input =
    match Astring.String.cuts ~rev:false ~empty:false ~sep:";;" input with
    | [] -> []
    | [h] -> [h]
    | h :: t -> h :: List.map ~f:(fun x -> ";;" ^ x) t

  let split_toplevel input =
    split_according_to_semisemi input
    |> map_flatten ~f:split_according_to_tokens

  let fragment (type a) (fg : a Mapper.fragment) input =
    match fg with
    | Mapper.Structure -> split_toplevel input
    | Mapper.Use_file -> split_toplevel input
    | Mapper.Signature -> split_according_to_tokens input
end

module Recover = struct
  let fragment (type a) (fg : a Mapper.fragment) input =
    Split.fragment fg input
    |> List.fold_left ~init:"" ~f:(fun acc item ->
           let append x y = match x with "" -> y | _ -> x ^ "\n\n" ^ y in
           match Parse.fragment fg (Lexing.from_string item) with
           | exception _ ->
               append acc (Format.sprintf "[%%%%invalid.ast.node %S]" item)
           | _parsed -> append acc item)
end

module Parse = struct
  let fragment (type a) (fg : a Mapper.fragment) input =
    Split.fragment fg input
    |> List.fold_left ~init:[] ~f:(fun acc item ->
           match Parse.fragment fg (Lexing.from_string item) with
           | exception _ -> Error item :: acc
           | parsed -> Ok parsed :: acc)
    |> List.rev
end

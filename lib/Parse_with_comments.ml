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

type 'a with_comments =
  {ast: 'a; comments: Cmt.t list; prefix: string; source: Source.t}

module W = struct
  type t = int

  let in_lexer = [1; 2; 3; 14; 29]

  let disable x = -abs x

  let enable x = abs x

  let to_string x =
    String.concat ~sep:"" (List.map ~f:(Format.sprintf "%+d") x)
end

exception Warning50 of (Location.t * Warnings.t) list

let token_with_comments (type a) : a Ast_passes.Ast0.t -> _ -> _ = function
  | Structure | Use_file | Expression -> Lexer.token_with_comments
  | Signature | Core_type | Module_type -> Lexer_merlin.token_with_comments

let skip_hash_bang (type a) : a Ast_passes.Ast0.t -> _ -> _ = function
  | Structure | Use_file | Expression -> Lexer.skip_hash_bang
  | Signature | Core_type | Module_type -> Lexer_merlin.skip_hash_bang

let comments (type a) : a Ast_passes.Ast0.t -> _ -> _ = function
  | Structure | Use_file | Expression -> Lexer.comments
  | Signature | Core_type | Module_type -> Lexer_merlin.comments

let tokens fragment lexbuf =
  let rec loop acc =
    match token_with_comments fragment lexbuf with
    (* The location in lexbuf are invalid for comments *)
    | COMMENT (_, loc) as tok -> loop ((tok, loc) :: acc)
    | DOCSTRING ds as tok -> loop ((tok, Docstrings.docstring_loc ds) :: acc)
    | tok -> (
        let loc = Location.of_lexbuf lexbuf in
        let acc = (tok, loc) :: acc in
        match tok with EOF -> List.rev acc | _ -> loop acc )
  in
  loop []

let fresh_lexbuf fragment source =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf !Location.input_name ;
  let hash_bang =
    skip_hash_bang fragment lexbuf ;
    let len = lexbuf.lex_last_pos in
    String.sub source ~pos:0 ~len
  in
  (lexbuf, hash_bang)

let parse parse fragment (conf : Conf.t) ~source =
  let warnings =
    W.enable 50
    :: (if conf.quiet then List.map ~f:W.disable W.in_lexer else [])
  in
  Warnings.parse_options false (W.to_string warnings) ;
  let w50 = ref [] in
  let t =
    let lexbuf, hash_bang = fresh_lexbuf fragment source in
    Warning.with_warning_filter
      ~filter:(fun loc warn ->
        if Warning.is_unexpected_docstring warn && conf.comment_check then (
          w50 := (loc, warn) :: !w50 ;
          false )
        else not conf.quiet )
      ~f:(fun () ->
        let ast = parse fragment lexbuf in
        Warnings.check_fatal () ;
        let comments =
          List.map
            ~f:(fun (txt, loc) -> Cmt.create txt loc)
            (comments fragment ())
        in
        let tokens =
          let lexbuf, _ = fresh_lexbuf fragment source in
          tokens fragment lexbuf
        in
        let source = Source.create ~text:source ~tokens in
        {ast; comments; prefix= hash_bang; source} )
  in
  match List.rev !w50 with [] -> t | w50 -> raise (Warning50 w50)

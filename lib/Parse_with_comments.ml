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

let tokens lexbuf =
  let rec loop acc =
    match Lexer.token_with_comments lexbuf with
    (* The location in lexbuf are invalid for comments *)
    | COMMENT (_, loc) as tok -> loop ((tok, loc) :: acc)
    | DOCSTRING ds as tok -> loop ((tok, Docstrings.docstring_loc ds) :: acc)
    | tok -> (
        let loc = Location.of_lexbuf lexbuf in
        let acc = (tok, loc) :: acc in
        match tok with EOF -> List.rev acc | _ -> loop acc )
  in
  loop []

let fresh_lexbuf source =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf !Location.input_name ;
  let hash_bang =
    Lexer.skip_hash_bang lexbuf ;
    let len = lexbuf.lex_last_pos in
    String.sub source ~pos:0 ~len
  in
  (lexbuf, hash_bang)

let parse ?(disable_w50 = false) parse fragment (conf : Conf.t) ~source =
  let warnings =
    if conf.opr_opts.quiet then List.map ~f:W.disable W.in_lexer else []
  in
  let warnings = if disable_w50 then warnings else W.enable 50 :: warnings in
  ignore @@ Warnings.parse_options false (W.to_string warnings) ;
  let w50 = ref [] in
  let t =
    let lexbuf, hash_bang = fresh_lexbuf source in
    Warning.with_warning_filter
      ~filter:(fun loc warn ->
        if
          Warning.is_unexpected_docstring warn && conf.opr_opts.comment_check
        then (
          w50 := (loc, warn) :: !w50 ;
          false )
        else not conf.opr_opts.quiet )
      ~f:(fun () ->
        let ast = parse fragment lexbuf in
        Warnings.check_fatal () ;
        let comments =
          List.map
            ~f:(fun (txt, loc) -> Cmt.create txt loc)
            (Lexer.comments ())
        in
        let tokens =
          let lexbuf, _ = fresh_lexbuf source in
          tokens lexbuf
        in
        let source = Source.create ~text:source ~tokens in
        {ast; comments; prefix= hash_bang; source} )
  in
  match List.rev !w50 with [] -> t | w50 -> raise (Warning50 w50)

(** [is_repl_block x] returns whether [x] is a list of REPL phrases and
    outputs of the form:

    {v
    # let this is = some phrase;;
    this is some output
    v} *)
let is_repl_block x =
  String.length x >= 2 && Char.equal x.[0] '#' && Char.is_whitespace x.[1]

let parse_toplevel ?disable_w50 (conf : Conf.t) ~source =
  let open Extended_ast in
  let preserve_beginend = Poly.(conf.fmt_opts.exp_grouping = `Preserve) in
  let parse_ast = Parse.ast ~preserve_beginend in
  if is_repl_block source && conf.fmt_opts.parse_toplevel_phrases then
    Either.Second (parse ?disable_w50 parse_ast Repl_file conf ~source)
  else First (parse ?disable_w50 parse_ast Use_file conf ~source)

(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2019-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

type 'a with_comments = {ast: 'a; comments: Cmt.t list; prefix: string}

module W = struct
  type t = int

  let in_lexer = [1; 2; 14; 29]

  let disable x = -abs x

  let enable x = abs x

  let to_string x =
    String.concat ~sep:"" (List.map ~f:(Format.sprintf "%+d") x)
end

exception Warning50 of (Location.t * Warnings.t) list

let parse parse_ast (conf : Conf.t) ~source =
  let lexbuf = Lexing.from_string source in
  let warnings =
    W.enable 50
    :: (if conf.quiet then List.map ~f:W.disable W.in_lexer else [])
  in
  Warnings.parse_options false (W.to_string warnings) ;
  let hash_bang =
    Lexer.skip_hash_bang lexbuf ;
    let len = lexbuf.lex_last_pos in
    String.sub source ~pos:0 ~len
  in
  Location.init lexbuf !Location.input_name ;
  let w50 = ref [] in
  let t =
    Compat.with_warning_filter
      ~filter:(fun loc warn ->
        match warn with
        | Warnings.Bad_docstring _ when conf.comment_check ->
            w50 := (loc, warn) :: !w50 ;
            false
        | _ -> not conf.quiet)
      ~f:(fun () ->
        let ast = parse_ast lexbuf in
        Warnings.check_fatal () ;
        let comments = Lexer.comments () in
        {ast; comments; prefix= hash_bang})
  in
  match List.rev !w50 with [] -> t | w50 -> raise (Warning50 w50)

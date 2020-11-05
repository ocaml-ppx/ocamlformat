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

type 'a with_comments = {ast: 'a; comments: Cmt.t list; prefix: string}

module W = struct
  type t = int

  let in_lexer = [1; 2; 3; 14; 29]

  let disable x = -abs x

  let enable x = abs x

  let to_string x =
    String.concat ~sep:"" (List.map ~f:(Format.sprintf "%+d") x)
end

exception Warning50 of (Location.t * Warnings.t) list

type pending =
  | No
  | Pending of {tokens: Parser.token list; invalid_loc: Location.t}

let wrap_token source invalid =
  match invalid with
  | [] -> Lexer.token
  | _ -> (
      let invalid = ref invalid in
      let pending = ref No in
      let maybe_emit lexbuf pending =
        match !pending with
        | No -> None
        | Pending {tokens= []; invalid_loc= _} -> assert false
        | Pending {tokens= x :: xs; invalid_loc} ->
            lexbuf.Lexing.lex_start_p <- invalid_loc.Location.loc_start ;
            lexbuf.Lexing.lex_curr_p <- invalid_loc.Location.loc_end ;
            (pending :=
               match xs with
               | [] ->
                   lexbuf.Lexing.lex_curr_pos <-
                     invalid_loc.Location.loc_end.pos_cnum ;
                   lexbuf.Lexing.lex_start_pos <-
                     invalid_loc.Location.loc_start.pos_cnum ;
                   No
               | tokens -> Pending {tokens; invalid_loc} ) ;
            Some x
      in
      fun lexbuf ->
        match maybe_emit lexbuf pending with
        | Some x -> x
        | None -> (
            let tok = Lexer.token lexbuf in
            match !invalid with
            | [] -> tok
            | first_invalid :: rest_invalid ->
                if
                  first_invalid.Location.loc_start.pos_cnum
                  <> lexbuf.Lexing.lex_start_p.pos_cnum
                then tok
                else (
                  invalid := rest_invalid ;
                  let source =
                    String.sub source
                      ~pos:first_invalid.Location.loc_start.pos_cnum
                      ~len:
                        ( first_invalid.Location.loc_end.pos_cnum
                        - first_invalid.Location.loc_start.pos_cnum )
                  in
                  pending :=
                    Pending
                      { invalid_loc= first_invalid
                      ; tokens=
                          List.concat
                            [ [ Parser.LBRACKETPERCENTPERCENT
                              ; LIDENT "invalid"
                              ; DOT
                              ; LIDENT "ast"
                              ; DOT
                              ; LIDENT "node" ]
                            ; [ STRING
                                  ( source ^ "\n;;"
                                  , Location.none
                                  , Some "_i_n_v_a_l_i_d_" ) ]
                            ; [RBRACKET] ] } ;
                  match maybe_emit lexbuf pending with
                  | None -> assert false
                  | Some x -> x ) ) )

let parse fragment (conf : Conf.t) ~source ~invalid =
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
  let token = wrap_token source invalid in
  let t =
    with_warning_filter
      ~filter:(fun loc warn ->
        if is_unexpected_docstring warn && conf.comment_check then (
          w50 := (loc, warn) :: !w50 ;
          false )
        else not conf.quiet )
      ~f:(fun () ->
        let ast = Migrate_ast.Parse.fragment fragment lexbuf token in
        Warnings.check_fatal () ;
        let comments =
          List.map
            ~f:(fun (txt, loc) -> Cmt.create txt loc)
            (Lexer.comments ())
        in
        {ast; comments; prefix= hash_bang} )
  in
  match List.rev !w50 with [] -> t | w50 -> raise (Warning50 w50)

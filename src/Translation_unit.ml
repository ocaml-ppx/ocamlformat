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

module Format = Format_

(** Translation units *)

open Migrate_ast

type 'a with_comments = {ast: 'a; comments: (string * Location.t) list}

(** Operations on translation units. *)
type 'a t =
  { input: Conf.t -> In_channel.t -> 'a with_comments
  ; init_cmts:
      Source.t -> Conf.t -> 'a -> (string * Location.t) list -> Cmts.t
  ; fmt: Source.t -> Cmts.t -> Conf.t -> 'a -> Fmt.t
  ; parse: Lexing.lexbuf -> 'a
  ; equal:
         ignore_doc_comments:bool
      -> 'a with_comments
      -> 'a with_comments
      -> bool
  ; moved_docstrings:
         'a with_comments
      -> 'a with_comments
      -> (Location.t * Location.t * string) list
  ; normalize: 'a with_comments -> 'a
  ; printast: Caml.Format.formatter -> 'a -> unit }

(** Existential package of a type of translation unit and its operations. *)
type x = XUnit : 'a t -> x

exception Warning50 of (Location.t * Warnings.t) list

exception
  Internal_error of
    [ `Ast
    | `Doc_comment of (Location.t * Location.t * string) list
    | `Comment
    | `Comment_dropped of (Location.t * string) list ]
    * (string * Sexp.t) list

let internal_error msg kvs = raise (Internal_error (msg, kvs))

module W : sig
  type t

  val in_lexer : int list

  val disable : int -> t

  val enable : int -> t

  val to_string : t list -> string
end = struct
  type t = int

  let in_lexer = [1; 2; 14; 29]

  let disable x = -abs x

  let enable x = abs x

  let to_string x =
    String.concat ~sep:"" (List.map ~f:(Format.sprintf "%+d") x)
end

let parse parse_ast (conf : Conf.t) ic =
  let warnings =
    W.enable 50
    :: (if conf.quiet then List.map ~f:W.disable W.in_lexer else [])
  in
  Warnings.parse_options false (W.to_string warnings) ;
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf !Location.input_name ;
  let warning_printer = !Location.warning_printer in
  let w50 = ref [] in
  (Location.warning_printer :=
     fun loc fmt warn ->
       match warn with
       | Warnings.Bad_docstring _ when conf.comment_check ->
           w50 := (loc, warn) :: !w50
       | _ -> if not conf.quiet then warning_printer loc fmt warn) ;
  try
    let ast = parse_ast lexbuf in
    Warnings.check_fatal () ;
    Location.warning_printer := warning_printer ;
    match List.rev !w50 with
    | [] ->
        let comments = Lexer.comments () in
        {ast; comments}
    | w50 -> raise (Warning50 w50)
  with e ->
    Location.warning_printer := warning_printer ;
    raise e

(** Debug: dump internal ast representation to file. *)
let dump xunit dir base suf ext ast =
  if Conf.debug then (
    let tmp = Filename.concat dir (base ^ suf ^ ext) in
    let oc = Out_channel.create tmp in
    xunit.printast (Caml.Format.formatter_of_out_channel oc) ast ;
    Out_channel.close oc )

type result =
  | Ok
  | Invalid_source of exn
  | Unstable of int
  | Ocamlformat_bug of exn
  | User_error of string

let parse_print (XUnit xunit) (conf : Conf.t) ~input_name ~input_file ic
    ofile =
  let dir =
    match ofile with
    | Some ofile -> Filename.dirname ofile
    | None -> Filename.get_temp_dir_name ()
  in
  let base = Filename.(remove_extension (basename input_name)) in
  let ext = Filename.extension input_name in
  (* iterate until formatting stabilizes *)
  let rec print_check ~i ~(conf : Conf.t) ~ast ~comments ~source_txt
      ~source_file : result =
    let tmp, oc =
      if not Conf.debug then Filename.open_temp_file ~temp_dir:dir base ext
      else
        let name = Format.sprintf "%s.%i%s" base i ext in
        let tmp = Filename.concat dir name in
        Format.eprintf "%s@\n%!" tmp ;
        let oc = Out_channel.create ~fail_if_exists:(not Conf.debug) tmp in
        (tmp, oc)
    in
    dump xunit dir base ".old" ".ast" ast ;
    let source = Source.create source_txt in
    let cmts_t = xunit.init_cmts source conf ast comments in
    let fs = Format.formatter_of_out_channel oc in
    Fmt.set_margin conf.margin fs ;
    xunit.fmt source cmts_t conf ast fs ;
    Format.pp_print_newline fs () ;
    Out_channel.close oc ;
    let conf = if Conf.debug then conf else {conf with Conf.quiet= true} in
    let fmted = In_channel.with_file tmp ~f:In_channel.input_all in
    if String.equal source_txt fmted then (
      match (Conf.action, ofile) with
      | _, None ->
          Out_channel.output_string stdout fmted ;
          Unix.unlink tmp ;
          Ok
      | In_out _, Some ofile -> Unix.rename tmp ofile ; Ok
      | Inplace _, Some ofile when i > 1 -> Unix.rename tmp ofile ; Ok
      | Inplace _, Some _ -> Unix.unlink tmp ; Ok )
    else
      match
        Location.input_name := tmp ;
        In_channel.with_file tmp ~f:(parse xunit.parse conf)
      with
      | exception Sys_error msg -> User_error msg
      | exception e -> Ocamlformat_bug e
      | new_ ->
          let old = {ast; comments} in
          if
            (* Ast not preserved ? *)
            not
              (xunit.equal ~ignore_doc_comments:(not conf.comment_check) old
                 new_)
          then (
            dump xunit dir base ".old" ".ast" (xunit.normalize old) ;
            dump xunit dir base ".new" ".ast" (xunit.normalize new_) ;
            if not Conf.debug then Unix.unlink tmp ;
            if xunit.equal ~ignore_doc_comments:true old new_ then
              let docstrings = xunit.moved_docstrings old new_ in
              internal_error (`Doc_comment docstrings)
                [("output file", String.sexp_of_t tmp)]
            else internal_error `Ast [("output file", String.sexp_of_t tmp)] ) ;
          (* Comments not preserved ? *)
          if conf.comment_check then (
            ( match Cmts.remaining_comments cmts_t with
            | [] -> ()
            | l ->
                let l = List.map l ~f:(fun (l, n, _t, _s) -> (l, n)) in
                internal_error (`Comment_dropped l) [] ) ;
            let diff_cmts = Cmts.diff comments new_.comments in
            if not (Sequence.is_empty diff_cmts) then (
              dump xunit dir base ".old" ".ast" old.ast ;
              dump xunit dir base ".new" ".ast" new_.ast ;
              if not Conf.debug then Unix.unlink tmp ;
              internal_error `Comment
                [ ( "diff"
                  , Sequence.sexp_of_t
                      (Either.sexp_of_t String.sexp_of_t String.sexp_of_t)
                      diff_cmts ) ] ) ) ;
          (* Too many iteration ? *)
          if i >= conf.max_iters then (
            Caml.flush_all () ;
            ( if Conf.debug then
              let command =
                Printf.sprintf "diff %s %s 1>&2" source_file tmp
              in
              ignore (Unix.system command) ) ;
            Unstable i )
          else
            let result =
              print_check ~i:(i + 1) ~conf ~ast:new_.ast
                ~comments:new_.comments ~source_txt:fmted ~source_file:tmp
            in
            Unix.unlink tmp ; result
  in
  let source_txt =
    In_channel.with_file input_file ~f:In_channel.input_all
  in
  Location.input_name := input_name ;
  let result =
    match xunit.input conf ic with
    | exception exn -> Invalid_source exn
    | _ when conf.disable ->
        ( match (Conf.action, ofile) with
        | _, None -> Out_channel.output_string stdout source_txt
        | In_out _, Some ofile ->
            Out_channel.write_all ofile ~data:source_txt
        | Inplace _, _ -> () ) ;
        Ok
    | {ast; comments} -> (
      try
        print_check ~i:1 ~conf ~ast ~comments ~source_txt
          ~source_file:input_file
      with
      | Sys_error msg -> User_error msg
      | exc -> Ocamlformat_bug exc )
  in
  let fmt = Caml.Format.err_formatter in
  let exe = Filename.basename Sys.argv.(0) in
  let quiet_unstable = false in
  let quiet_comments = false in
  let quiet_doc_comments = false in
  let quiet_exn exn =
    match[@ocaml.warning "-28"] exn with
    | Internal_error ((`Comment | `Comment_dropped _), _) -> quiet_comments
    | Warning50 _ -> quiet_doc_comments
    | Internal_error (`Doc_comment _, _) -> quiet_doc_comments
    | Internal_error (`Ast, _) -> false
    | Internal_error (_, _) -> .
    | Syntaxerr.Error _ | Lexer.Error _ -> false
    | _ -> false
  in
  ( match result with
  | Ok -> ()
  | Invalid_source _ when conf.Conf.quiet -> ()
  | Invalid_source exn -> (
      let reason =
        (* NOTE: Warning 28 is suppressed due to a difference in exception
           constructor arit y between OCaml versions. See this
           ocaml-migrate-parsetree issue for potential future mitigation.
           https://github.com/ocaml-ppx/ocaml-migrate-parsetree/issues/34 *)
        match[@ocaml.warning "-28"] exn with
        | Syntaxerr.Error _ | Lexer.Error _ -> " (syntax error)"
        | Warning50 _ -> " (misplaced documentation comments - warning 50)"
        | _ -> ""
      in
      Format.eprintf "%s: ignoring %S%s\n%!" exe input_name reason ;
      match[@ocaml.warning "-28"] exn with
      | Syntaxerr.Error _ | Lexer.Error _ ->
          Location.report_exception fmt exn
      | Warning50 l ->
          List.iter l ~f:(fun (l, w) -> !Location.warning_printer l fmt w)
      | exn -> Format.eprintf "%s\n%!" (Exn.to_string exn) )
  | Unstable _ when quiet_unstable -> ()
  | Unstable i when i <= 1 ->
      Format.eprintf
        "%s: %S was not already formatted. ([max-iters = 1])\n%!" exe
        input_name
  | Unstable i ->
      Format.eprintf
        "%s: Cannot process %S.\n\
        \  Please report this bug at \
         https://github.com/ocaml-ppx/ocamlformat/issues.\n\
         %!"
        exe input_name ;
      Format.eprintf
        "  BUG: formatting did not stabilize after %i iterations.\n%!" i
  | User_error msg -> Format.eprintf "%s: %s.\n%!" exe msg
  | Ocamlformat_bug exn when quiet_exn exn -> ()
  | Ocamlformat_bug exn -> (
      Format.eprintf
        "%s: Cannot process %S.\n\
        \  Please report this bug at \
         https://github.com/ocaml-ppx/ocamlformat/issues.\n\
         %!"
        exe input_name ;
      match[@ocaml.warning "-28"] exn with
      | Syntaxerr.Error _ | Lexer.Error _ ->
          Format.eprintf "  BUG: generating invalid ocaml syntax.\n%!" ;
          if Conf.debug then Location.report_exception fmt exn
      | Warning50 l ->
          Format.eprintf "  BUG: misplaced documentation comments.\n%!" ;
          if Conf.debug then
            List.iter l ~f:(fun (l, w) -> !Location.warning_printer l fmt w)
      | Internal_error (m, l) ->
          let s =
            match m with
            | `Ast -> "ast changed"
            | `Doc_comment _ -> "doc comments changed"
            | `Comment -> "comments changed"
            | `Comment_dropped _ -> "comments dropped"
          in
          Format.eprintf "  BUG: %s.\n%!" s ;
          ( match m with
          | `Doc_comment l when not conf.Conf.quiet ->
              List.iter l ~f:(fun (loc1, loc2, msg) ->
                  Caml.Format.eprintf
                    "%!@{<loc>%a@}:@,@{<error>Error@}: Docstring (** %s *) \
                     moved to @{<loc>%a@}.\n\
                     %!"
                    Location.print_loc loc1 (String.strip msg)
                    Location.print_loc loc2 )
          | `Comment_dropped l when not conf.Conf.quiet ->
              List.iter l ~f:(fun (loc, msg) ->
                  Caml.Format.eprintf
                    "%!@{<loc>%a@}:@,@{<error>Error@}: Comment (* %s *) \
                     dropped.\n\
                     %!"
                    Location.print_loc loc (String.strip msg) )
          | _ -> () ) ;
          if Conf.debug then
            List.iter l ~f:(fun (msg, sexp) ->
                Format.eprintf "  %s: %s\n%!" msg (Sexp.to_string sexp) )
      | exn ->
          Format.eprintf
            "  BUG: unhandled exception. Use [--debug] for details.\n%!" ;
          if Conf.debug then Format.eprintf "%s\n%!" (Exn.to_string exn) )
  ) ;
  result

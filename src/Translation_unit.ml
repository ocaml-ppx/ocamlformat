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

(** Operations on translation units. *)
type 'a t =
  { input:
      input_name:string -> In_channel.t -> 'a * (string * Location.t) list
  ; init_cmts: string -> 'a -> (string * Location.t) list -> unit
  ; fmt: Conf.t -> 'a -> Fmt.t
  ; parse:
         ?warn:bool
      -> input_name:string
      -> In_channel.t
      -> 'a * (string * Location.t) list
  ; equal:
         'a * (string * Location.t) list
      -> 'a * (string * Location.t) list
      -> bool
  ; normalize: 'a * (string * Location.t) list -> 'a
  ; no_translation: 'a -> bool
  ; printast: Caml.Format.formatter -> 'a -> unit }

(** Existential package of a type of translation unit and its operations. *)
type x = XUnit: 'a t -> x

let parse parse_ast ?(warn= Conf.warn_error) ~input_name ic =
  Warnings.parse_options false (if warn then "@50" else "-50") ;
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf input_name ;
  Location.input_name := input_name ;
  let ast = parse_ast lexbuf in
  Warnings.check_fatal () ;
  let comments = Lexer.comments () in
  (ast, comments)

(** Debug: dump internal ast representation to file. *)
let dump xunit dir base suf ext ast =
  if Conf.debug then (
    let tmp = Filename.concat dir (base ^ suf ^ ext) in
    let oc = Out_channel.create tmp in
    xunit.printast (Caml.Format.formatter_of_out_channel oc) ast ;
    Out_channel.close oc )

let parse_print (XUnit xunit) (conf: Conf.t) ~input_name:iname
    ~input_file:ifile ic ofile =
  let dir =
    match ofile with
    | Some ofile -> Filename.dirname ofile
    | None -> Filename.get_temp_dir_name ()
  in
  let base = Filename.(remove_extension (basename iname)) in
  let ext = Filename.extension iname in
  (* iterate until formatting stabilizes *)
  let rec parse_print_ i source ic =
    Format.pp_print_flush Format.err_formatter () ;
    let tmp, oc =
      if not Conf.debug then Filename.open_temp_file ~temp_dir:dir base ext
      else
        let name = Format.sprintf "%s.%i%s" base i ext in
        let tmp = Filename.concat dir name in
        Format.eprintf "%s@\n" tmp ;
        let oc = Out_channel.create ~fail_if_exists:(not Conf.debug) tmp in
        (tmp, oc)
    in
    let ast, cmts =
      if i = 1 then xunit.input ~input_name:iname ic
      else xunit.parse ~warn:false ~input_name:tmp ic
    in
    if Conf.debug then
      dump xunit dir base ".old" ".ast" (xunit.normalize (ast, cmts)) ;
    xunit.init_cmts source ast cmts ;
    ( if xunit.no_translation ast then (
        In_channel.seek ic Int64.zero ;
        Out_channel.output_string oc (In_channel.input_all ic) )
    else
      let fs = Format.formatter_of_out_channel oc in
      Fmt.set_margin conf.margin fs ;
      xunit.fmt conf ast fs ;
      Format.pp_print_newline fs () ) ;
    Out_channel.close oc ;
    let fmted = In_channel.with_file tmp ~f:In_channel.input_all in
    if not (String.equal source fmted) then (
      ( try Cmts.final_check () with exc ->
          dump xunit dir base ".old" ".ast" ast ;
          if not Conf.debug then Unix.unlink tmp ;
          raise exc ) ;
      In_channel.with_file tmp ~f:(fun ic' ->
          let ast', cmts' = xunit.parse ~input_name:tmp ic' in
          let eq_ast = xunit.equal (ast, cmts) (ast', cmts') in
          if not eq_ast then (
            dump xunit dir base ".old" ".ast" (xunit.normalize (ast, cmts)) ;
            dump xunit dir base ".new" ".ast"
              (xunit.normalize (ast', cmts')) ;
            if not Conf.debug then Unix.unlink tmp ;
            internal_error "formatting changed ast"
              [("output file", String.sexp_of_t tmp)] ) ;
          let diff_cmts = Cmts.diff cmts cmts' in
          if not (Sequence.is_empty diff_cmts) then (
            dump xunit dir base ".old" ".ast" ast ;
            dump xunit dir base ".new" ".ast" ast' ;
            if not Conf.debug then Unix.unlink tmp ;
            internal_error "formatting changed comments"
              [ ( "diff"
                , Sequence.sexp_of_t
                    (Either.sexp_of_t String.sexp_of_t String.sexp_of_t)
                    diff_cmts ) ] ) ) ;
      if i < conf.max_iters then (
        In_channel.with_file tmp ~f:(fun ic -> parse_print_ (i + 1) fmted ic) ;
        Unix.unlink tmp )
      else (
        ignore (Unix.system ("diff " ^ ifile ^ " " ^ tmp)) ;
        internal_error
          (Format.sprintf "formatting did not stabilize after %i iterations"
             i)
          [] ) )
    else
      match (Conf.action, ofile) with
      | _, None ->
          Out_channel.output_string stdout fmted ;
          Unix.unlink tmp
      | In_out _, Some ofile -> Unix.rename tmp ofile
      | Inplace _, Some ofile when i > 1 -> Unix.rename tmp ofile
      | Inplace _, _ -> Unix.unlink tmp
  in
  let source = In_channel.with_file ifile ~f:In_channel.input_all in
  (* NOTE: Warning 28 is suppressed due to a difference in exception
     constructor arity between OCaml versions. See this
     ocaml-migrate-parsetree issue for potential future mitigation.
     https://github.com/ocaml-ppx/ocaml-migrate-parsetree/issues/34 *)
  try[@ocaml.warning "-28"] parse_print_ 1 source ic with
  | Fmt_ast.Formatting_disabled -> (
    match (Conf.action, ofile) with
    | _, None -> Out_channel.output_string stdout source
    | In_out _, Some ofile -> Out_channel.write_all ofile source
    | Inplace _, _ -> () )
  | Warnings.Errors _ -> Caml.exit 1
  | Syntaxerr.Error _ as exc ->
      Location.report_exception Caml.Format.err_formatter exc ;
      Caml.exit 1

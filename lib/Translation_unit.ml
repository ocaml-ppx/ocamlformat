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

(** Translation units *)

module Location = Migrate_ast.Location
open Parse_with_comments
open Result.Monad_infix

exception
  Internal_error of
    [ `Cannot_parse of exn
    | `Ast_changed
    | `Doc_comment of Docstring.error list
    | `Comment
    | `Comment_dropped of Cmt.t list
    | `Warning50 of (Location.t * Warnings.t) list ]
    * (string * Sexp.t) list

let internal_error msg kvs = raise (Internal_error (msg, kvs))

let chop_any_extension s =
  match Filename.chop_extension s with
  | r -> r
  | exception Invalid_argument _ -> s

let exe = chop_any_extension (Filename.basename Caml.Sys.argv.(0))

module Error = struct
  type t =
    | Invalid_source of {exn: exn; input_name: string}
    | Unstable of
        {iteration: int; prev: string; next: string; input_name: string}
    | Ocamlformat_bug of {exn: exn; input_name: string}
    | User_error of string

  let user_error x = User_error x

  let equal : t -> t -> bool = Poly.equal

  let print_diff input_name ~prev ~next =
    let ext = Filename.extension input_name in
    let input_name =
      Filename.chop_extension (Filename.basename input_name)
    in
    let p = Filename.temp_file input_name (Printf.sprintf ".prev%s" ext) in
    let n = Filename.temp_file input_name (Printf.sprintf ".next%s" ext) in
    Out_channel.write_all p ~data:prev ;
    Out_channel.write_all n ~data:next ;
    ignore
      (Caml.Sys.command
         (Printf.sprintf "git diff --no-index -u %S %S | sed '1,4d' 1>&2" p n) ) ;
    Caml.Sys.remove p ;
    Caml.Sys.remove n

  let print ?(debug = false) ?(quiet = false) fmt = function
    | Invalid_source _ when quiet -> ()
    | Invalid_source {exn; input_name} -> (
        let reason =
          match exn with
          | Syntaxerr.Error _ | Lexer.Error _ -> " (syntax error)"
          | Warning50 _ -> " (misplaced documentation comments - warning 50)"
          | _ -> ""
        in
        Format.fprintf fmt "%s: ignoring %S%s\n%!" exe input_name reason ;
        match exn with
        | Syntaxerr.Error _ | Lexer.Error _ ->
            Location.report_exception fmt exn
        | Warning50 l ->
            List.iter l ~f:(fun (l, w) -> Warning.print_warning l w) ;
            Format.fprintf fmt
              "@{<warning>Hint@}: (Warning 50) This file contains a \
               documentation comment (** ... *) that the OCaml compiler \
               does not know how to attach to the AST. OCamlformat does not \
               support these cases. You can find more information at: \
               https://github.com/ocaml-ppx/ocamlformat#overview. If you'd \
               like to disable this check and let ocamlformat make a choice \
               (though it might not be consistent with the ocaml compilers \
               and odoc), you can set the --no-comment-check option.\n\
               %!"
        | exn -> Format.fprintf fmt "%s\n%!" (Exn.to_string exn) )
    | Unstable {iteration; prev; next; input_name} ->
        if debug then print_diff input_name ~prev ~next ;
        if iteration <= 1 then
          Format.fprintf fmt
            "%s: %S was not already formatted. ([max-iters = 1])\n%!" exe
            input_name
        else (
          Format.fprintf fmt
            "%s: Cannot process %S.\n\
            \  Please report this bug at \
             https://github.com/ocaml-ppx/ocamlformat/issues.\n\
             %!"
            exe input_name ;
          Format.fprintf fmt
            "  BUG: formatting did not stabilize after %i iterations.\n%!"
            iteration )
    | User_error msg -> Format.fprintf fmt "%s: %s.\n%!" exe msg
    | Ocamlformat_bug {exn; input_name} -> (
        Format.fprintf fmt
          "%s: Cannot process %S.\n\
          \  Please report this bug at \
           https://github.com/ocaml-ppx/ocamlformat/issues.\n\
           %!"
          exe input_name ;
        match exn with
        | Internal_error (m, l) ->
            let s =
              match m with
              | `Cannot_parse _ -> "generating invalid ocaml syntax"
              | `Ast_changed -> "ast changed"
              | `Doc_comment _ -> "doc comments changed"
              | `Comment -> "comments changed"
              | `Comment_dropped _ -> "comments dropped"
              | `Warning50 _ -> "misplaced documentation comments"
            in
            Format.fprintf fmt "  BUG: %s.\n%!" s ;
            ( match m with
            | `Doc_comment l when not quiet ->
                List.iter l ~f:(function
                  | Added (loc, msg) ->
                      Format.fprintf fmt
                        "%!@{<loc>%a@}:@,\
                         @{<error>Error@}: Docstring (** %s *) added.\n\
                         %!"
                        Location.print loc msg
                  | Removed (loc, msg) ->
                      Format.fprintf fmt
                        "%!@{<loc>%a@}:@,\
                         @{<error>Error@}: Docstring (** %s *) dropped.\n\
                         %!"
                        Location.print loc msg
                  | Moved (loc_before, loc_after, msg) ->
                      if Location.compare loc_before Location.none = 0 then
                        Format.fprintf fmt
                          "%!@{<loc>%a@}:@,\
                           @{<error>Error@}: Docstring (** %s *) added.\n\
                           %!"
                          Location.print loc_after msg
                      else if Location.compare loc_after Location.none = 0
                      then
                        Format.fprintf fmt
                          "%!@{<loc>%a@}:@,\
                           @{<error>Error@}: Docstring (** %s *) dropped.\n\
                           %!"
                          Location.print loc_before msg
                      else
                        Format.fprintf fmt
                          "%!@{<loc>%a@}:@,\
                           @{<error>Error@}: Docstring (** %s *) moved to \
                           @{<loc>%a@}.\n\
                           %!"
                          Location.print loc_before msg Location.print
                          loc_after
                  | Unstable (loc, x, y) ->
                      Format.fprintf fmt
                        "%!@{<loc>%a@}:@,\
                         @{<error>Error@}: Formatting of doc-comment is \
                         unstable (e.g. parses as a list or not depending \
                         on the margin):\n\
                         %!"
                        Location.print loc ;
                      print_diff input_name ~prev:x ~next:y ;
                      Format.fprintf fmt
                        "Please tighten up this comment in the source or \
                         disable the formatting using the option \
                         --no-parse-docstrings.\n\
                         %!" )
            | `Comment_dropped l when not quiet ->
                List.iter l ~f:(fun Cmt.{txt= msg; loc} ->
                    Format.fprintf fmt
                      "%!@{<loc>%a@}:@,\
                       @{<error>Error@}: Comment (* %s *) dropped.\n\
                       %!"
                      Location.print loc msg )
            | `Cannot_parse ((Syntaxerr.Error _ | Lexer.Error _) as exn) ->
                if debug then Location.report_exception fmt exn
            | `Warning50 l ->
                if debug then
                  List.iter l ~f:(fun (l, w) -> Warning.print_warning l w)
            | _ -> () ) ;
            if debug then
              List.iter l ~f:(fun (msg, sexp) ->
                  Format.fprintf fmt "  %s: %s\n%!" msg (Sexp.to_string sexp) )
        | exn ->
            Format.fprintf fmt
              "  BUG: unhandled exception. Use [--debug] for details.\n%!" ;
            if debug then Format.fprintf fmt "%s\n%!" (Exn.to_string exn) )
end

let with_file input_name output_file suf ext f =
  let dir =
    match output_file with
    | Some filename -> Filename.dirname filename
    | None -> Filename.get_temp_dir_name ()
  in
  let base = Filename.remove_extension (Filename.basename input_name) in
  let tmp = Filename.concat dir (base ^ suf ^ ext) in
  Out_channel.with_file tmp ~f ;
  tmp

let dump_ast ~input_name ?output_file ~suffix fmt =
  let ext = ".ast" in
  with_file input_name output_file suffix ext (fun oc ->
      fmt (Format.formatter_of_out_channel oc) )

let dump_formatted ~input_name ?output_file ~suffix fmted =
  let ext = Filename.extension input_name in
  with_file input_name output_file suffix ext (fun oc ->
      Out_channel.output_string oc fmted )

let check_all_locations fmt cmts_t =
  match Cmts.remaining_locs cmts_t with
  | [] -> ()
  | l ->
      let print l = Format.fprintf fmt "%a\n%!" Location.print l in
      Format.fprintf fmt
        "Warning: Some locations have not been considered\n%!" ;
      List.iter ~f:print (List.sort l ~compare:Location.compare)

let check_margin (conf : Conf.t) ~filename ~fmted =
  List.iteri (String.split_lines fmted) ~f:(fun i line ->
      if String.length line > conf.fmt_opts.margin then
        Format.fprintf Format.err_formatter
          "Warning: %s:%i exceeds the margin\n%!" filename i )

let with_optional_box_debug ~box_debug k =
  if box_debug then Fmt.with_box_debug k else k

let with_buffer_formatter ~buffer_size k =
  let buffer = Buffer.create buffer_size in
  let fs = Format_.formatter_of_buffer buffer in
  Fmt.eval fs k ;
  Format_.pp_print_flush fs () ;
  if Buffer.length buffer > 0 then Format_.pp_print_newline fs () ;
  Buffer.contents buffer

let recover (type a) : a Extended_ast.t -> _ -> a = function
  | Structure -> Parser_recovery.structure
  | Signature -> Parser_recovery.signature
  | Use_file -> Parser_recovery.use_file
  | Core_type -> failwith "no recovery for core_type"
  | Module_type -> failwith "no recovery for module_type"
  | Expression -> failwith "no recovery for expression"
  | Repl_file -> failwith "no recovery for repl_file"

let strconst_mapper locs =
  let constant self c =
    match c.Parsetree.pconst_desc with
    | Parsetree.Pconst_string (_, {Location.loc_start; loc_end; _}, Some _)
      ->
        locs := (loc_start.Lexing.pos_cnum, loc_end.Lexing.pos_cnum) :: !locs ;
        c
    | _ -> Ast_mapper.default_mapper.constant self c
  in
  {Ast_mapper.default_mapper with constant}

let collect_strlocs (type a) (fg : a Extended_ast.t) (ast : a) :
    (int * int) list =
  let locs = ref [] in
  let _ = Extended_ast.map fg (strconst_mapper locs) ast in
  let compare (c1, _) (c2, _) = Stdlib.compare c1 c2 in
  List.sort ~compare !locs

let format (type a b) (fg : a Extended_ast.t) (std_fg : b Std_ast.t)
    ?output_file ~input_name ~prev_source ~parsed ~std_parsed (conf : Conf.t)
    =
  let open Result.Monad_infix in
  let dump_ast fg ~suffix ast =
    if conf.opr_opts.debug then
      Some
        (dump_ast ~input_name ?output_file ~suffix (fun fmt ->
             Std_ast.Printast.ast fg fmt ast ) )
    else None
  in
  let dump_formatted ~suffix fmted =
    if conf.opr_opts.debug then
      Some (dump_formatted ~input_name ?output_file ~suffix fmted)
    else None
  in
  Location.input_name := input_name ;
  (* iterate until formatting stabilizes *)
  let rec print_check ~i ~(conf : Conf.t) ~prev_source t std_t =
    let format ~box_debug =
      let open Fmt in
      let cmts_t =
        Cmts.init fg ~debug:conf.opr_opts.debug t.source t.ast t.comments
      in
      let contents =
        with_buffer_formatter
          ~buffer_size:(String.length prev_source)
          ( set_margin conf.fmt_opts.margin
          $ opt conf.fmt_opts.max_indent set_max_indent
          $ fmt_if_k
              (not (String.is_empty t.prefix))
              (str t.prefix $ fmt "@.")
          $ with_optional_box_debug ~box_debug
              (Fmt_ast.fmt_ast fg ~debug:conf.opr_opts.debug t.source cmts_t
                 conf t.ast ) )
      in
      (contents, cmts_t)
    in
    if conf.opr_opts.debug then
      format ~box_debug:true |> fst
      |> dump_formatted ~suffix:".boxes"
      |> (ignore : string option -> unit) ;
    let fmted, cmts_t = format ~box_debug:false in
    let conf =
      if conf.opr_opts.debug then conf
      else {conf with opr_opts= {conf.opr_opts with quiet= true}}
    in
    if String.equal prev_source fmted then (
      if conf.opr_opts.debug then
        check_all_locations Format.err_formatter cmts_t ;
      if conf.opr_opts.margin_check then
        check_margin conf ~fmted
          ~filename:(Option.value output_file ~default:input_name) ;
      let strlocs = collect_strlocs fg t.ast in
      Ok (strlocs, fmted) )
    else
      let exn_args () =
        [("output file", dump_formatted ~suffix:".invalid-ast" fmted)]
        |> List.filter_map ~f:(fun (s, f_opt) ->
               Option.map f_opt ~f:(fun f -> (s, String.sexp_of_t f)) )
      in
      let preserve_beginend =
        Poly.(conf.fmt_opts.exp_grouping = `Preserve)
      in
      let parse_ast = Extended_ast.Parse.ast ~preserve_beginend in
      ( match parse parse_ast ~disable_w50:true fg conf ~source:fmted with
      | exception Sys_error msg -> Error (Error.User_error msg)
      | exception exn -> internal_error (`Cannot_parse exn) (exn_args ())
      | t_new -> Ok t_new )
      >>= fun t_new ->
      ( match parse Std_ast.Parse.ast std_fg conf ~source:fmted with
      | exception Sys_error msg -> Error (Error.User_error msg)
      | exception Warning50 l -> internal_error (`Warning50 l) (exn_args ())
      | exception exn -> internal_error (`Cannot_parse exn) (exn_args ())
      | std_t_new -> Ok std_t_new )
      >>= fun std_t_new ->
      (* Ast not preserved ? *)
      ( if
        (not
           (Normalize_std_ast.equal std_fg conf std_t.ast std_t_new.ast
              ~ignore_doc_comments:(not conf.opr_opts.comment_check) ) )
        && not
             (Normalize_extended_ast.equal fg conf t.ast t_new.ast
                ~ignore_doc_comments:(not conf.opr_opts.comment_check) )
      then
        let old_ast =
          dump_ast std_fg ~suffix:".old"
            (Normalize_std_ast.ast std_fg conf std_t.ast)
        in
        let new_ast =
          dump_ast std_fg ~suffix:".new"
            (Normalize_std_ast.ast std_fg conf std_t_new.ast)
        in
        let args ~suffix =
          [ ("output file", dump_formatted ~suffix fmted)
          ; ("old ast", old_ast)
          ; ("new ast", new_ast) ]
          |> List.filter_map ~f:(fun (s, f_opt) ->
                 Option.map f_opt ~f:(fun f -> (s, String.sexp_of_t f)) )
        in
        if
          Normalize_std_ast.equal std_fg ~ignore_doc_comments:true conf
            std_t.ast std_t_new.ast
        then
          let docstrings =
            Normalize_std_ast.moved_docstrings std_fg conf std_t.ast
              std_t_new.ast
          in
          let args = args ~suffix:".unequal-docs" in
          internal_error (`Doc_comment docstrings) args
        else
          let args = args ~suffix:".unequal-ast" in
          internal_error `Ast_changed args ) ;
      (* Comments not preserved ? *)
      if conf.opr_opts.comment_check then (
        ( match Cmts.remaining_comments cmts_t with
        | [] -> ()
        | l -> internal_error (`Comment_dropped l) [] ) ;
        let is_docstring (Cmt.{txt; loc} as cmt) =
          match txt with
          | "" | "*" -> Either.Second cmt
          | _ when Char.equal txt.[0] '*' ->
              (* Doc comments here (comming directly from the lexer) include
                 their leading star *. It is not part of the docstring and
                 should be dropped. *)
              let txt = String.drop_prefix txt 1 in
              let cmt = Cmt.create txt loc in
              if conf.fmt_opts.parse_docstrings then Either.First cmt
              else Either.Second cmt
          | _ -> Either.Second cmt
        in
        let old_docstrings, old_comments =
          List.partition_map t.comments ~f:is_docstring
        in
        let t_newdocstrings, t_newcomments =
          List.partition_map t_new.comments ~f:is_docstring
        in
        let diff_cmts =
          Sequence.append
            (Normalize_extended_ast.diff_cmts conf old_comments t_newcomments)
            (Normalize_extended_ast.diff_docstrings conf old_docstrings
               t_newdocstrings )
        in
        if not (Sequence.is_empty diff_cmts) then
          let old_ast = dump_ast std_fg ~suffix:".old" std_t.ast in
          let new_ast = dump_ast std_fg ~suffix:".new" std_t_new.ast in
          let args =
            [ ( "diff"
              , Some
                  (Sequence.sexp_of_t
                     (Either.sexp_of_t String.sexp_of_t String.sexp_of_t)
                     diff_cmts ) )
            ; ("old ast", Option.map old_ast ~f:String.sexp_of_t)
            ; ("new ast", Option.map new_ast ~f:String.sexp_of_t) ]
            |> List.filter_map ~f:(fun (s, f_opt) ->
                   Option.map f_opt ~f:(fun f -> (s, f)) )
          in
          internal_error `Comment args ) ;
      (* Too many iteration ? *)
      if i >= conf.opr_opts.max_iters then (
        Caml.flush_all () ;
        Error
          (Unstable {iteration= i; prev= prev_source; next= fmted; input_name}
          ) )
      else
        (* All good, continue *)
        print_check ~i:(i + 1) ~conf ~prev_source:fmted t_new std_t_new
  in
  try print_check ~i:1 ~conf ~prev_source parsed std_parsed with
  | Sys_error msg -> Error (User_error msg)
  | exn -> Error (Ocamlformat_bug {exn; input_name})

let parse_result ?disable_w50 f fragment conf ~source ~input_name =
  match parse ?disable_w50 f fragment conf ~source with
  | exception exn -> Error (Error.Invalid_source {exn; input_name})
  | parsed -> Ok parsed

let normalize_eol ~strlocs ~line_endings s =
  let buf = Buffer.create (String.length s) in
  let add_cr n = Buffer.add_string buf (String.init n ~f:(fun _ -> '\r')) in
  let rec normalize_segment ~seen_cr i stop =
    if i = stop then add_cr seen_cr
    else
      match s.[i] with
      | '\r' -> normalize_segment ~seen_cr:(seen_cr + 1) (i + 1) stop
      | '\n' ->
          Buffer.add_string buf
            (match line_endings with `Crlf -> "\r\n" | `Lf -> "\n") ;
          normalize_segment ~seen_cr:0 (i + 1) stop
      | c ->
          add_cr seen_cr ;
          Buffer.add_char buf c ;
          normalize_segment ~seen_cr:0 (i + 1) stop
  in
  let rec loop locs i =
    match locs with
    | [] ->
        normalize_segment ~seen_cr:0 i (String.length s) ;
        Buffer.contents buf
    | (start, stop) :: xs ->
        normalize_segment ~seen_cr:0 i start ;
        Buffer.add_substring buf s ~pos:start ~len:(stop - start) ;
        loop xs stop
  in
  loop strlocs 0

let parse_and_format (type a b) (fg : a Extended_ast.t)
    (std_fg : b Std_ast.t) ?output_file ~input_name ~source (conf : Conf.t) =
  Location.input_name := input_name ;
  let preserve_beginend = Poly.(conf.fmt_opts.exp_grouping = `Preserve) in
  let parse_ast = Extended_ast.Parse.ast ~preserve_beginend in
  parse_result parse_ast ~disable_w50:true fg conf ~source ~input_name
  >>= fun parsed ->
  parse_result Std_ast.Parse.ast std_fg conf ~source ~input_name
  >>= fun std_parsed ->
  format fg std_fg ?output_file ~input_name ~prev_source:source ~parsed
    ~std_parsed conf
  >>= fun (strlocs, formatted) ->
  Ok
    (normalize_eol ~strlocs ~line_endings:conf.fmt_opts.line_endings
       formatted )

let parse_and_format = function
  | Syntax.Structure -> parse_and_format Structure Structure
  | Syntax.Signature -> parse_and_format Signature Signature
  | Syntax.Use_file -> parse_and_format Use_file Use_file
  | Syntax.Core_type -> parse_and_format Core_type Core_type
  | Syntax.Module_type -> parse_and_format Module_type Module_type
  | Syntax.Expression -> parse_and_format Expression Expression
  | Syntax.Repl_file -> parse_and_format Repl_file Repl_file

let check_line nlines i =
  (* the last line of the buffer (nlines + 1) should not raise an error *)
  if 1 <= i && i <= nlines + 1 then Ok ()
  else Error (Error.User_error (Format.sprintf "Invalid line number %i" i))

let check_range nlines (low, high) =
  check_line nlines low
  >>= fun () ->
  check_line nlines high
  >>= fun () ->
  if low <= high then Ok ()
  else
    Error (Error.User_error (Format.sprintf "Invalid range %i-%i" low high))

let numeric (type a b) (fg : a list Extended_ast.t)
    (std_fg : b list Std_ast.t) ~input_name ~source ~range (conf : Conf.t) =
  let lines = String.split_lines source in
  let nlines = List.length lines in
  check_range nlines range
  >>| fun () ->
  Location.input_name := input_name ;
  let preserve_beginend = Poly.(conf.fmt_opts.exp_grouping = `Preserve) in
  let parse_ast = Extended_ast.Parse.ast ~preserve_beginend in
  let fallback () = Indent.Partial_ast.indent_range ~source ~range in
  let indent_parsed parsed std_parsed ~src ~range =
    let {ast= parsed_ast; _} = parsed in
    match
      format fg std_fg ~input_name ~prev_source:src ~parsed ~std_parsed conf
    with
    | Ok (_, fmted_src) -> (
      match parse_result parse_ast fg ~source:fmted_src conf ~input_name with
      | Ok {ast= fmted_ast; source= fmted_src; _} ->
          Indent.Valid_ast.indent_range fg ~lines ~range
            ~unformatted:(parsed_ast, src) ~formatted:(fmted_ast, fmted_src)
      | Error _ -> fallback () )
    | Error _ -> fallback ()
  in
  let parse_or_recover ~src =
    match parse_result parse_ast fg conf ~source:src ~input_name with
    | Ok parsed -> Ok parsed
    | Error _ -> parse_result recover fg conf ~source:src ~input_name
  in
  match parse_or_recover ~src:source with
  | Ok parsed -> (
    match parse_result Std_ast.Parse.ast std_fg conf ~source ~input_name with
    | Ok std_parsed -> indent_parsed parsed std_parsed ~src:source ~range
    | Error _ -> fallback () )
  | Error _ -> fallback ()

let numeric = function
  | Syntax.Structure -> numeric Structure Structure
  | Syntax.Signature -> numeric Signature Signature
  | Syntax.Use_file -> numeric Use_file Use_file
  | Syntax.Core_type -> failwith "numeric not implemented for Core_type"
  | Syntax.Module_type -> failwith "numeric not implemented for Module_type"
  | Syntax.Expression -> failwith "numeric not implemented for Expression"
  | Syntax.Repl_file -> failwith "numeric not implemented for Repl_file"

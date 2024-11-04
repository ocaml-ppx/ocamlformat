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

let ( let* ) = Result.( >>= )

let ( let+ ) = Result.( >>| )

exception
  Internal_error of
    [ `Cannot_parse of exn
    | `Ast_changed
    | `Comment of Cmt.error
    | `Warning50 of Location.t * Warnings.t ]
    list
    * (string * Sexp.t) list

let internal_error msg kvs = raise (Internal_error (msg, kvs))

let chop_any_extension s =
  match Filename.chop_extension s with
  | r -> r
  | exception Invalid_argument _ -> s

let exe = chop_any_extension (Filename.basename Stdlib.Sys.argv.(0))

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
      (Stdlib.Sys.command
         (Printf.sprintf "git diff --no-index -u %S %S | sed '1,4d' 1>&2" p n) ) ;
    Stdlib.Sys.remove p ;
    Stdlib.Sys.remove n

  let print_internal_error ~debug ~quiet fmt e =
    let s =
      match e with
      | `Cannot_parse (Parse_with_comments.Warning50 _) ->
          "generating invalid comment attachment"
      | `Cannot_parse _ -> "generating invalid ocaml syntax"
      | `Ast_changed -> "ast changed"
      | `Comment _ -> "comment changed"
      | `Warning50 _ -> "misplaced documentation comment"
    in
    Format.fprintf fmt "  BUG: %s.\n%!" s ;
    match e with
    | `Comment x when not quiet -> Cmt.pp_error fmt x
    | `Cannot_parse ((Syntaxerr.Error _ | Lexer.Error _) as exn) ->
        if debug then Location.report_exception fmt exn
    | `Cannot_parse (Parse_with_comments.Warning50 _) ->
        (* Printing the warning is not useful because it doesn't reference
           the right filename *)
        ()
    | `Cannot_parse exn ->
        if debug then Format.fprintf fmt "%s" (Stdlib.Printexc.to_string exn)
    | `Warning50 (l, w) -> if debug then Warning.print_warning l w
    | _ -> ()

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
        | Internal_error (errors, l) ->
            List.iter errors ~f:(print_internal_error ~debug ~quiet fmt) ;
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

let dump_formatted ~input_name ?output_file
    ?(ext = Filename.extension input_name) ~suffix fmted =
  with_file input_name output_file suffix ext (fun oc ->
      Out_channel.output_string oc fmted )

let check_all_locations fmt cmts_t =
  match Cmts.remaining_locs cmts_t with
  | [] -> ()
  | l ->
      let print l = Format.fprintf fmt "%a\n%!" Location.print_loc l in
      Format.fprintf fmt
        "Warning: Some locations have not been considered\n%!" ;
      List.iter ~f:print (List.sort l ~compare:Location.compare)

let check_margin (conf : Conf.t) ~filename ~fmted =
  List.iteri (String.split_lines fmted) ~f:(fun i line ->
      if String.length line > conf.fmt_opts.margin.v then
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

let check_remaining_comments cmts =
  let dropped x = {Cmt.kind= `Dropped x; cmt_kind= `Comment} in
  match Cmts.remaining_comments cmts with
  | [] -> Ok ()
  | cmts -> Error (List.map cmts ~f:dropped)

let check_comments (conf : Conf.t) cmts ~old:t_old ~new_:t_new =
  if conf.opr_opts.comment_check.v then
    let errors =
      let* () = check_remaining_comments cmts in
      Normalize_extended_ast.diff_cmts conf t_old.comments t_new.comments
    in
    match errors with
    | Ok () -> ()
    | Error e -> internal_error (List.map e ~f:(fun x -> `Comment x)) []

let format (type ext std) (ext_fg : ext Extended_ast.t)
    (std_fg : std Std_ast.t) ?output_file ~input_name ~prev_source
    ~ext_parsed ~std_parsed (conf : Conf.t) =
  Box_debug.enable_stacktraces := conf.opr_opts.debug.v ;
  let dump_ast fg ~suffix ast =
    if conf.opr_opts.debug.v then
      Some
        (dump_ast ~input_name ?output_file ~suffix (fun fmt ->
             Std_ast.Printast.ast fg fmt ast ) )
    else None
  in
  let dump_formatted ?ext ~suffix fmted =
    if conf.opr_opts.debug.v then
      Some (dump_formatted ?ext ~input_name ?output_file ~suffix fmted)
    else None
  in
  Location.input_name := input_name ;
  (* iterate until formatting stabilizes *)
  let rec print_check ~i ~(conf : Conf.t) ~prev_source ext_t std_t =
    let format ~box_debug =
      let open Fmt in
      let cmts_t =
        Cmts.init ext_fg ~debug:conf.opr_opts.debug.v ext_t.source ext_t.ast
          ext_t.comments
      in
      let contents =
        with_buffer_formatter
          ~buffer_size:(String.length prev_source)
          ( set_margin conf.fmt_opts.margin.v
          $ set_max_indent conf.fmt_opts.max_indent.v
          $ fmt_if
              (not (String.is_empty ext_t.prefix))
              (str ext_t.prefix $ force_newline)
          $ with_optional_box_debug ~box_debug
              (Fmt_ast.fmt_ast ext_fg ~debug:conf.opr_opts.debug.v
                 ext_t.source cmts_t conf ext_t.ast ) )
      in
      (contents, cmts_t)
    in
    ( if conf.opr_opts.debug.v then
        format ~box_debug:true |> fst
        |> dump_formatted ~suffix:"_boxes" ~ext:".html"
        |> function
        | Some file ->
            if i = 1 then Format.eprintf "[DEBUG] Box structure: %s\n" file
        | None -> () ) ;
    let fmted, cmts_t = format ~box_debug:false in
    let conf =
      if conf.opr_opts.debug.v then conf
      else
        { conf with
          opr_opts=
            { conf.opr_opts with
              quiet= {v= true; from= conf.opr_opts.quiet.from} } }
    in
    if String.equal prev_source fmted then (
      if conf.opr_opts.debug.v then
        check_all_locations Format.err_formatter cmts_t ;
      if conf.opr_opts.margin_check.v then
        check_margin conf ~fmted
          ~filename:(Option.value output_file ~default:input_name) ;
      let strlocs = collect_strlocs ext_fg ext_t.ast in
      Ok (strlocs, fmted) )
    else
      let exn_args () =
        [("output file", dump_formatted ~suffix:".invalid-ast" fmted)]
        |> List.filter_map ~f:(fun (s, f_opt) ->
               Option.map f_opt ~f:(fun f -> (s, String.sexp_of_t f)) )
      in
      let* ext_t_new =
        match
          parse (parse_ast conf) ~disable_w50:true ext_fg conf ~input_name
            ~source:fmted
        with
        | exception Sys_error msg -> Error (Error.User_error msg)
        | exception exn -> internal_error [`Cannot_parse exn] (exn_args ())
        | ext_t_new -> Ok ext_t_new
      in
      let* std_t_new =
        match
          parse Std_ast.Parse.ast std_fg conf ~input_name ~source:fmted
        with
        | exception Sys_error msg -> Error (Error.User_error msg)
        | exception Warning50 l ->
            internal_error
              (List.map ~f:(fun x -> `Warning50 x) l)
              (exn_args ())
        | exception exn -> internal_error [`Cannot_parse exn] (exn_args ())
        | std_t_new -> Ok std_t_new
      in
      (* Ast not preserved ? *)
      ( if
          not
            (Normalize_std_ast.equal std_fg conf std_t.ast std_t_new.ast
               ~ignore_doc_comments:(not conf.opr_opts.comment_check.v) )
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
            internal_error
              (List.map ~f:(fun x -> `Comment x) docstrings)
              args
          else
            let args = args ~suffix:".unequal-ast" in
            internal_error [`Ast_changed] args
        else
          dump_ast std_fg ~suffix:""
            (Normalize_std_ast.ast std_fg conf std_t_new.ast)
          |> function
          | Some file ->
              if i = 1 then Format.eprintf "[DEBUG] AST structure: %s\n" file
          | None -> () ) ;
      check_comments conf cmts_t ~old:ext_t ~new_:ext_t_new ;
      (* Too many iteration ? *)
      if i >= conf.opr_opts.max_iters.v then (
        Stdlib.flush_all () ;
        Error
          (Unstable {iteration= i; prev= prev_source; next= fmted; input_name}
          ) )
      else (* All good, continue *)
        print_check ~i:(i + 1) ~conf ~prev_source:fmted ext_t_new std_t_new
  in
  try print_check ~i:1 ~conf ~prev_source ext_parsed std_parsed with
  | Sys_error msg -> Error (User_error msg)
  | exn -> Error (Ocamlformat_bug {exn; input_name})

let parse_result ?disable_w50 f fragment conf ~source ~input_name =
  match parse ?disable_w50 f fragment conf ~input_name ~source with
  | exception exn -> Error (Error.Invalid_source {exn; input_name})
  | parsed -> Ok parsed

let parse_and_format (type ext std) (ext_fg : ext Extended_ast.t)
    (std_fg : std Std_ast.t) ?output_file ~input_name ~source (conf : Conf.t)
    =
  Location.input_name := input_name ;
  let line_endings = conf.fmt_opts.line_endings.v in
  let* ext_parsed =
    parse_result (parse_ast conf) ~disable_w50:true ext_fg conf ~source
      ~input_name
  in
  let* std_parsed =
    parse_result Std_ast.Parse.ast std_fg conf ~source ~input_name
  in
  let+ strlocs, formatted =
    format ext_fg std_fg ?output_file ~input_name ~prev_source:source
      ~ext_parsed ~std_parsed conf
  in
  Eol_compat.normalize_eol ~exclude_locs:strlocs ~line_endings formatted

let parse_and_format syntax =
  let (Extended_ast.Any ext) = Extended_ast.of_syntax syntax in
  let (Std_ast.Any std) = Std_ast.of_syntax syntax in
  parse_and_format ext std

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

open Ocamlformat_lib
open Migrate_ast
open Parse_with_comments

let chop_any_extension s =
  match Filename.chop_extension s with
  | r -> r
  | exception Invalid_argument _ -> s

let exe = chop_any_extension (Filename.basename Caml.Sys.argv.(0))

let ellipsis n msg =
  let msg = String.strip msg in
  if n > 0 && String.length msg > (n * 2) + 10 then
    Format.sprintf "%s ... %s" (String.prefix msg n) (String.suffix msg n)
  else msg

let ellipsis_cmt = ellipsis 50

module Error = struct
  include Translation_unit.Error

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
        if debug then (
          let ext = Filename.extension input_name in
          let input_name =
            Filename.chop_extension (Filename.basename input_name)
          in
          let p =
            Filename.temp_file input_name (Printf.sprintf ".prev%s" ext)
          in
          Out_channel.write_all p ~data:prev ;
          let n =
            Filename.temp_file input_name (Printf.sprintf ".next%s" ext)
          in
          Out_channel.write_all n ~data:next ;
          ignore (Unix.system (Printf.sprintf "diff %S %S 1>&2" p n)) ;
          Unix.unlink p ;
          Unix.unlink n ) ;
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
        | Translation_unit.Internal_error (m, l) ->
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
                  | Normalize.Moved (loc_before, loc_after, msg) ->
                      if Location.compare loc_before Location.none = 0 then
                        Format.fprintf fmt
                          "%!@{<loc>%a@}:@,\
                           @{<error>Error@}: Docstring (** %s *) added.\n\
                           %!"
                          Location.print loc_after (ellipsis_cmt msg)
                      else if Location.compare loc_after Location.none = 0
                      then
                        Format.fprintf fmt
                          "%!@{<loc>%a@}:@,\
                           @{<error>Error@}: Docstring (** %s *) dropped.\n\
                           %!"
                          Location.print loc_before (ellipsis_cmt msg)
                      else
                        Format.fprintf fmt
                          "%!@{<loc>%a@}:@,\
                           @{<error>Error@}: Docstring (** %s *) moved to \
                           @{<loc>%a@}.\n\
                           %!"
                          Location.print loc_before (ellipsis_cmt msg)
                          Location.print loc_after
                  | Normalize.Unstable (loc, s) ->
                      Format.fprintf fmt
                        "%!@{<loc>%a@}:@,\
                         @{<error>Error@}: Formatting of (** %s *) is \
                         unstable (e.g. parses as a list or not depending \
                         on the margin), please tighten up this comment in \
                         the source or disable the formatting using the \
                         option --no-parse-docstrings.\n\
                         %!"
                        Location.print loc (ellipsis_cmt s) )
            | `Comment_dropped l when not quiet ->
                List.iter l ~f:(fun Cmt.{txt= msg; loc} ->
                    Format.fprintf fmt
                      "%!@{<loc>%a@}:@,\
                       @{<error>Error@}: Comment (* %s *) dropped.\n\
                       %!"
                      Location.print loc (ellipsis_cmt msg) )
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

let parse_and_format = Translation_unit.parse_and_format

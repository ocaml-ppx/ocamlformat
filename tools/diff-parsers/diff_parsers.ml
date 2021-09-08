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

let diff d1 d2 f =
  let f1 = Filename.concat d1 f in
  let f2 = Filename.concat d2 f in
  if Sys.file_exists f2 then
    Sys.command
      (Printf.sprintf
         {|diff -U 5 -L %s %s -L %s %s | sed 's/^@@ .* @@$/@@@@/g'|} f1 f1 f2
         f2 )
  else 0

let import version dir f =
  Sys.command
    (Printf.sprintf
       "curl -s -o %s \
        https://raw.githubusercontent.com/ocaml/ocaml/%s/parsing/%s"
       (Filename.concat dir f) version f )

let files =
  [ "ast_helper.ml"
  ; "ast_helper.mli"
  ; "ast_mapper.ml"
  ; "ast_mapper.mli"
  ; "asttypes.mli"
  ; "docstrings.ml"
  ; "docstrings.mli"
  ; "lexer.mll"
  ; "parse.ml"
  ; "parse.mli"
  ; "parser.mly"
  ; "parsetree.mli"
  ; "pprintast.ml"
  ; "pprintast.mli" ]

let usage () =
  let exe = Filename.basename Sys.executable_name in
  Printf.printf "usage:\n\n%s diff DIR1 DIR2\n\n%s import VERSION DIR" exe
    exe ;
  exit 1

let () =
  let l = List.tl (Array.to_list Sys.argv) in
  match l with
  | "diff" :: args -> (
    match args with
    | [d1; d2] -> (
        let codes = List.map (diff d1 d2) files in
        let non_zero =
          List.filter (function 0 | 1 -> false | _ -> true) codes
        in
        match non_zero with [] -> () | first :: _ -> exit first )
    | _ -> usage () )
  | "import" :: args -> (
    match args with
    | [version; dir] -> (
        let codes = List.map (import version dir) files in
        let non_zero =
          List.filter (function 0 -> false | _ -> true) codes
        in
        match non_zero with [] -> () | first :: _ -> exit first )
    | _ -> usage () )
  | _ -> usage ()

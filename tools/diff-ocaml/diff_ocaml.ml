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

let usage () =
  let exe = Filename.basename Sys.executable_name in
  Printf.printf
    "usage:\n\n\
     %s ocaml diff parser DIR1 DIR2\n\n\
     %s ocaml diff stdlib DIR1 DIR2\n\n\
     %s ocaml import VERSION DIR\n\n\
     %s odoc-parser import VERSION DIR\n"
    exe exe exe exe ;
  exit 1

let diff d1 d2 f =
  let f1 = Filename.concat d1 f in
  let f2 = Filename.concat d2 f in
  if Sys.file_exists f2 then
    Sys.command
      (Printf.sprintf
         {|diff -U 5 -L %s %s -L %s %s | sed 's/^@@ .* @@$/@@@@/g'|} f1 f1 f2
         f2 )
  else 0

let import ~org ~prj ~version ~src ~dst f =
  Sys.command
    (Printf.sprintf
       "curl -s -o %s https://raw.githubusercontent.com/%s/%s/%s/%s/%s"
       (Filename.concat dst f) org prj version src f )

let diff files d1 d2 =
  let codes = List.map (diff d1 d2) files in
  List.iter (function 0 | 1 -> () | c -> exit c) codes

module OCaml = struct
  let parser_files =
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
    ; "printast.ml"
    ; "printast.mli" ]

  let stdlib_files = ["format.ml"; "format.mli"]

  let import = import ~org:"ocaml" ~prj:"ocaml"

  let parse args =
    match args with
    | "diff" :: args -> (
      match args with
      | ["parser"; d1; d2] -> diff parser_files d1 d2
      | ["stdlib"; d1; d2] -> diff stdlib_files d1 d2
      | _ -> usage () )
    | "import" :: args -> (
      match args with
      | [version; dst] -> (
          let codes =
            List.map (import ~src:"parsing" ~version ~dst) parser_files
            @ List.map (import ~src:"stdlib" ~version ~dst) stdlib_files
          in
          let non_zero =
            List.filter (function 0 -> false | _ -> true) codes
          in
          match non_zero with [] -> () | first :: _ -> exit first )
      | _ -> usage () )
    | _ -> usage ()
end

module Odoc_parser = struct
  let files =
    [ "ast.ml"
    ; "lexer.mli"
    ; "lexer.mll"
    ; "loc.ml"
    ; "loc.mli"
    ; "odoc_parser.ml"
    ; "odoc_parser.mli"
    ; "parse_error.ml"
    ; "syntax.ml"
    ; "syntax.mli"
    ; "token.ml"
    ; "warning.ml" ]

  let import = import ~org:"ocaml-doc" ~prj:"odoc-parser"

  let parse args =
    match args with
    | "import" :: args -> (
      match args with
      | [version; dst] -> (
          let codes = List.map (import ~src:"src" ~version ~dst) files in
          let non_zero =
            List.filter (function 0 -> false | _ -> true) codes
          in
          match non_zero with [] -> () | first :: _ -> exit first )
      | _ -> usage () )
    | _ -> usage ()
end

let () =
  let l = List.tl (Array.to_list Sys.argv) in
  match l with
  | "ocaml" :: args -> OCaml.parse args
  | "odoc-parser" :: args -> Odoc_parser.parse args
  | _ -> usage ()

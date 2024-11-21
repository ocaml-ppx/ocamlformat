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

{
open Stdlib
open Migrate_ast

let newline lexbuf = Lexing.new_line lexbuf
}

let eol = ('\013'* '\010')
let ws = ' ' | '\t' | eol

rule token = parse
 | eof           { [] }
 | eol           { newline lexbuf;
                   `Output ("", Location.curr lexbuf) :: token lexbuf }
 | "#" ws+       { let pos_start = Lexing.lexeme_end_p lexbuf in
                   let c = phrase (Buffer.create 8) lexbuf in
                   `Command (c, pos_start) :: token lexbuf }
 | ([^'#' '\n'] [^'\n']* as str) eol
                 { newline lexbuf;
                   `Output (str, Location.curr lexbuf) :: token lexbuf }
 | _ as c        {
     let msg =
       Format.sprintf
         "unexpected character '%c'.\n\
          Hint: did you forget a space after the '#' at the start of the line?"
         c
     in
     raise (Syntaxerr.Error (Not_expecting (Location.curr lexbuf, msg))) }

and phrase buf = parse
  | eof      {
     let msg = "a toplevel phrase must end with `;;`." in
     raise (Syntaxerr.Error (Expecting (Location.curr lexbuf, msg))) }
  | ((eol* eol) as nl) ("  " | "\t")
      { for _ = 1 to (Base.String.count ~f:(Char.equal '\n') nl) do
          newline lexbuf;
          Buffer.add_char buf '\n'
        done;
        phrase buf lexbuf }
  | ";;"     { Buffer.add_string buf ";;"; Buffer.contents buf }
  | _ as c   { Buffer.add_char buf c; phrase buf lexbuf }

{
let repl_file ~ocaml_version lx =
  let x = token lx in
  let open Ocamlformat_parser_extended.Parsetree in
  List.fold_left (fun acc -> function
      | `Command (cmd, pos_start) ->
          let cmd_lexbuf = Lexing.from_string cmd in
          let filename = (Location.curr lx).loc_start.pos_fname in
          Lexing.set_filename cmd_lexbuf filename ;
          Lexing.set_position cmd_lexbuf pos_start ;
          { prepl_phrase= Parse.toplevel_phrase ~ocaml_version cmd_lexbuf
          ; prepl_output= "" }
          :: acc
      | `Output ("", _) -> acc
      | `Output (line, loc) -> (
          match acc with
          | [] ->
              let msg =
                Format.sprintf
                  "%S.\n\
                   Hint: A toplevel block must start with a toplevel phrase \
                   starting with `# `." line
              in
              raise (Syntaxerr.Error (Not_expecting (loc, msg)))
          | {prepl_phrase; prepl_output} :: t ->
              {prepl_phrase; prepl_output= prepl_output ^ line}
              :: t )
    ) [] x
  |> List.rev
}

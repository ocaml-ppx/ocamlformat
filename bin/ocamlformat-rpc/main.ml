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

(** OCamlFormat-RPC *)

open Ocamlformat_lib
open Ocamlformat_rpc_lib;;

Caml.at_exit (Format.pp_print_flush Format.err_formatter);;

Caml.at_exit (Format_.pp_print_flush Format_.err_formatter)

let format fg conf source =
  let input_name = "<rpc input>" in
  let opts = Conf.{debug= false; margin_check= false} in
  Translation_unit.parse_and_format fg ~input_name ~source conf opts

let rec rpc_main conf =
  match Command.read_input stdin with
  | `Halt -> Ok ()
  | `Unknown | `Error _ -> rpc_main conf
  | `Format x ->
      List.fold_until ~init:()
        ~finish:(fun () ->
          Command.output stdout (`Error (Format.flush_str_formatter ())) )
        ~f:(fun () try_formatting ->
          match try_formatting conf x with
          | Ok formatted ->
              ignore (Format.flush_str_formatter ()) ;
              Command.output stdout (`Format formatted) ;
              Stop ()
          | Error e ->
              Translation_unit.Error.print Format.str_formatter e ;
              Continue () )
        (* The formatting functions are ordered in such a way that the ones
           expecting a keyword first (like signatures) are placed before the
           more general ones (like toplevel phrases). Parsing a file as
           `--impl` with `ocamlformat` processes it as a use file (toplevel
           phrases) anyway.

           `ocaml-lsp` should use core types, module types and signatures.
           `ocaml-mdx` should use toplevel phrases, expressions and
           signatures. *)
        [ format Core_type
        ; format Signature
        ; format Module_type
        ; format Expression
        ; format Use_file ] ;
      rpc_main conf
  | `Config c -> (
      let rec update conf = function
        | [] -> Ok conf
        | (name, value) :: t -> (
          match Conf.update_value conf ~name ~value with
          | Ok c -> update c t
          | Error e -> Error e )
      in
      match update conf c with
      | Ok conf ->
          Command.output stdout (`Config c) ;
          Out_channel.flush stdout ;
          rpc_main conf
      | Error e ->
          let msg =
            match e with
            | `Bad_value (x, y) ->
                Format.sprintf "Bad configuration value (%s, %s)" x y
            | `Malformed x ->
                Format.sprintf "Malformed configuration value %s" x
            | `Misplaced (x, y) ->
                Format.sprintf "Misplaced configuration value (%s, %s)" x y
            | `Unknown (x, _) ->
                Format.sprintf "Unknown configuration option %s" x
          in
          Command.output stdout (`Error msg) ;
          Out_channel.flush stdout ;
          rpc_main conf )

let rpc_main () = rpc_main Conf.default_profile

open Cmdliner

let info =
  let doc = "RPC mode for OCamlFormat, a tool to format OCaml code." in
  let man =
    [ `S Cmdliner.Manpage.s_description
    ; `P
        "$(tname) listens to RPC requests, provided on the standard input, \
         and prints the response on the standard output."
    ; `S Cmdliner.Manpage.s_commands
    ; `P "The supported RPC commands are:"
    ; `P "- $(b,Halt) to close the connection to the RPC"
    ; `P
        "- $(b,Config) $(i,CSEXP): submits a list of (key, value) pairs (as \
         a canonical s-expression) to update OCamlFormat's configuration \
         (please refer to $(i,ocamlformat --help) to know more about the \
         available options). The accepted configuration is sent as a reply \
         of the same form. The configuration can be reset to its default \
         value by sending the pair $(i,(\"profile\", \"default\"))."
    ; `P
        "- $(b,Format) $(i,CSEXP): submits a canonical s-expression \
         $(i,CSEXP) to be formatted by OCamlFormat, the formatted output is \
         sent as a reply of the same form $(b,Format) $(i,CSEXP)"
    ; `P "Unknown or deprecated commands are ignored." ]
  in
  Term.info "ocamlformat-rpc" ~version:Version.version ~doc ~man

let rpc_main_t = Term.(const rpc_main $ const ())

let () = Term.exit @@ Term.eval (rpc_main_t, info)

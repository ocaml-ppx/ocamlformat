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
open Ocamlformat_rpc_lib

;;
Caml.at_exit (Format.pp_print_flush Format.err_formatter)

;;
Caml.at_exit (Format_.pp_print_flush Format_.err_formatter)

module V = struct
  type t = V1

  let handshake = function
    | "v1" | "V1" -> `Handled V1
    | _ -> `Propose_another V1

  let to_string = function V1 -> "v1"
end

type state = Waiting_for_version | Version_defined of (V.t * Conf.t)

let format fg conf source =
  let input_name = "<rpc input>" in
  let opts =
    Conf.{debug= false; margin_check= false; format_invalid_files= false}
  in
  Translation_unit.parse_and_format fg ~input_name ~source conf opts

let rec rpc_main = function
  | Waiting_for_version -> (
    match Init.read_input stdin with
    | `Halt -> Ok ()
    | `Unknown -> Ok ()
    | `Version vstr -> (
      match V.handshake vstr with
      | `Handled v ->
          Init.output stdout (`Version vstr) ;
          Out_channel.flush stdout ;
          rpc_main (Version_defined (v, Conf.default_profile))
      | `Propose_another v ->
          let vstr = V.to_string v in
          Init.output stdout (`Version vstr) ;
          Out_channel.flush stdout ;
          rpc_main Waiting_for_version ) )
  | Version_defined (v, conf) as state -> (
    match v with
    | V1 -> (
      match V1.Command.read_input stdin with
      | `Halt -> Ok ()
      | `Unknown | `Error _ -> rpc_main state
      | `Format x ->
          List.fold_until ~init:()
            ~finish:(fun () ->
              V1.Command.output stdout
                (`Error (Format.flush_str_formatter ())) )
            ~f:(fun () try_formatting ->
              match try_formatting conf x with
              | Ok formatted ->
                  ignore (Format.flush_str_formatter ()) ;
                  V1.Command.output stdout (`Format formatted) ;
                  Stop ()
              | Error e ->
                  Translation_unit.Error.print Format.str_formatter e ;
                  Continue () )
            (* The formatting functions are ordered in such a way that the
               ones expecting a keyword first (like signatures) are placed
               before the more general ones (like toplevel phrases). Parsing
               a file as `--impl` with `ocamlformat` processes it as a use
               file (toplevel phrases) anyway.

               `ocaml-lsp` should use core types, module types and
               signatures. `ocaml-mdx` should use toplevel phrases,
               expressions and signatures. *)
            [ format Core_type
            ; format Signature
            ; format Module_type
            ; format Expression
            ; format Use_file ] ;
          rpc_main state
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
              V1.Command.output stdout (`Config c) ;
              Out_channel.flush stdout ;
              rpc_main (Version_defined (v, conf))
          | Error e ->
              let msg =
                match e with
                | `Bad_value (x, y) ->
                    Format.sprintf "Bad configuration value (%s, %s)" x y
                | `Malformed x ->
                    Format.sprintf "Malformed configuration value %s" x
                | `Misplaced (x, y) ->
                    Format.sprintf "Misplaced configuration value (%s, %s)" x
                      y
                | `Unknown (x, y) ->
                    Format.sprintf "Unknown configuration value (%s, %s)" x y
              in
              V1.Command.output stdout (`Error msg) ;
              Out_channel.flush stdout ;
              rpc_main state ) ) )

let rpc_main () = rpc_main Waiting_for_version

open Cmdliner

let info =
  let doc = "RPC mode for OCamlFormat, a tool to format OCaml code." in
  let man =
    [ `S Cmdliner.Manpage.s_description
    ; `P
        "$(tname) listens to RPC requests, provided on the standard input, \
         and prints the response on the standard output."
    ; `S Cmdliner.Manpage.s_commands
    ; `P
        "Before the client and the server agree on a common version to use \
         the following commands are available: $(b,Halt) to close the \
         connection to the RPC; $(b,Version) $(i,v) to ask the server to \
         use version $(i,v). If the server agrees upon the version he will \
         send the reply $(b,Version) $(i,v) and the protocol version is set \
         to $(i,v), to use another version later the client has to close \
         the connexion and start a new one. If the server cannot use \
         version $(i,v) he might propose another version $(i,w) by sending \
         the reply $(b,Version) $(i,w) that the client can accept by \
         sending the same request for version $(i,w), or propose another \
         version. If the server cannot propose another version it will \
         close the connection. Unknown commands are ignored."
    ; `P
        "Once the client and the server agree on a common version, the \
         requests you can send may differ from one version to another."
    ; `P "On version $(b,v1), the supported RPC commands are:"
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
    ; `P "Unknown commands are ignored." ]
  in
  Term.info "ocamlformat-rpc" ~version:Version.version ~doc ~man

let rpc_main_t = Term.(const rpc_main $ const ())

let () = Term.exit @@ Term.eval (rpc_main_t, info)

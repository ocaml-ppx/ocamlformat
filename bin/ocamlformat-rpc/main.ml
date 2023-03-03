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

let () =
  Stdlib.at_exit (Format.pp_print_flush Format.err_formatter) ;
  Stdlib.at_exit (Format_.pp_print_flush Format_.err_formatter)

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
    ; `P "All versions support the following commands:"
    ; `P
        "- $(b,Halt) to end the communication with the RPC server. The \
         caller must close the input and output channels."
    ; `P
        "Some RPC versions offer specific commands, that are detailed below."
    ; `P "Specific commands supported on version $(b,v1) are:"
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
    ; `P "Specific commands supported on version $(b,v2) are:"
    ; `P
        "- $(b,Format) $(i,CSEXP): submits a list as canonical s-expression \
         $(i,CSEXP), where the first element of the list is a string to be \
         formatted by OCamlFormat. The other arguments are (key, value) \
         pairs, where key can be either $(i,\"Path\") and/or \
         $(i,\"Config\"). They modify the server's configuration \
         temporarily, for the current request. The formatted output is sent \
         as a reply of the same form."
    ; `P "Unknown commands are ignored." ]
  in
  Cmd.info "ocamlformat-rpc" ~version:Ocamlformat_lib.Version.current ~doc
    ~man

let rpc_main_t = Term.(const Ocamlformat_rpc.run $ const ())

let () = Stdlib.exit @@ Cmd.eval_result (Cmd.v info rpc_main_t)

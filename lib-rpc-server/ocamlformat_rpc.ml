open Ocamlformat
open Ocamlformat_rpc_lib
open Ocamlformat_stdlib

module IO = struct
  type 'a t = 'a

  type ic = In_channel.t

  type oc = Out_channel.t

  let ( >>= ) x f = f x

  let return x = x

  let read ic =
    match Csexp.input ic with
    | Ok x -> return (Some x)
    | Error _ -> return None

  let write oc lx =
    List.iter lx ~f:(Csexp.to_channel oc) ;
    Out_channel.flush oc ;
    return ()
end

module V = struct
  let handshake x =
    match Version.of_string x with
    | Some v -> `Handled v
    | None -> `Propose_another Version.V2
end

type state = Waiting_for_version | Version_defined of (Version.t * Conf.t)

include Make (IO)

let format fg conf source =
  let input_name = "<rpc input>" in
  Translation_unit.parse_and_format fg ~input_name ~source conf

let run_config conf c =
  let rec update conf = function
    | [] -> Ok conf
    | (name, value) :: t -> (
      match Conf.update_value conf ~name ~value with
      | Ok c -> update c t
      | Error e -> Error (`Config_error e) )
  in
  update conf c

let run_path path =
  match
    Ocamlformat.Conf.build_config ~enable_outside_detected_project:false
      ~root:None ~file:path ~is_stdin:false
  with
  | Ok _ as ok -> ok
  | Error e -> Error (`Path_error e)

let run_format conf x =
  List.fold_until ~init:()
    ~finish:(fun () -> Error (`Format_error (Format.flush_str_formatter ())))
    ~f:(fun () try_formatting ->
      match try_formatting conf x with
      | Ok formatted -> Stop (Ok (`Format formatted))
      | Error e ->
          Translation_unit.Error.print Format.str_formatter e ;
          Continue () )
    (* The formatting functions are ordered in such a way that the ones
       expecting a keyword first (like signatures) are placed before the more
       general ones (like toplevel phrases). Parsing a file as `--impl` with
       `ocamlformat` processes it as a use file (toplevel phrases) anyway.

       `ocaml-lsp` should use core types, module types and signatures.
       `ocaml-mdx` should use toplevel phrases, expressions and
       signatures. *)
    [ format Core_type
    ; format Signature
    ; format Module_type
    ; format Expression
    ; format Use_file ]

let run_format_with_args {path; config} conf x =
  let open Result in
  Option.value_map path ~default:(Ok conf) ~f:run_path
  >>= fun conf ->
  Option.value_map config ~default:(Ok conf) ~f:(fun c -> run_config conf c)
  >>= fun conf -> run_format conf x

let handle_format_error e output =
  output stdout (`Error e) ;
  Out_channel.flush stdout

let handle_path_error e output =
  output stdout (`Error e) ;
  Out_channel.flush stdout

let handle_config_error (e : Config_option.Error.t) output =
  let msg =
    match e with
    | Bad_value (x, y) ->
        Format.sprintf "Bad configuration value (%s, %s)" x y
    | Malformed x -> Format.sprintf "Malformed configuration value %s" x
    | Misplaced (x, y) ->
        Format.sprintf "Misplaced configuration value (%s, %s)" x y
    | Unknown (x, _) -> Format.sprintf "Unknown configuration option %s" x
  in
  output stdout (`Error msg) ;
  Out_channel.flush stdout

let handle_error e output =
  match e with
  | `Format_error e -> handle_format_error e output
  | `Config_error e -> handle_config_error e output
  | `Path_error e -> handle_path_error e output

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
          rpc_main (Version_defined (v, Conf.default))
      | `Propose_another v ->
          let vstr = Version.to_string v in
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
          let conf =
            match run_format_with_args empty_args conf x with
            | Ok (`Format formatted) ->
                V1.Command.output stdout (`Format formatted) ;
                conf
            | Error e ->
                handle_error e V1.Command.output ;
                conf
          in
          rpc_main (Version_defined (v, conf))
      | `Config c -> (
        match run_config conf c with
        | Ok conf ->
            V1.Command.output stdout (`Config c) ;
            Out_channel.flush stdout ;
            rpc_main (Version_defined (v, conf))
        | Error (`Config_error e) ->
            handle_config_error e V1.Command.output ;
            rpc_main state ) )
    | V2 -> (
      match V2.Command.read_input stdin with
      | `Halt -> Ok ()
      | `Unknown | `Error _ -> rpc_main state
      | `Format (x, format_args) ->
          let conf =
            match run_format_with_args format_args conf x with
            | Ok (`Format formatted) ->
                V2.Command.output stdout (`Format (formatted, format_args)) ;
                conf
            | Error e ->
                handle_error e V2.Command.output ;
                conf
          in
          rpc_main (Version_defined (v, conf)) ) )

let run () = rpc_main Waiting_for_version

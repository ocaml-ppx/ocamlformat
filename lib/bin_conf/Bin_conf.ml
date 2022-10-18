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

open Ocamlformat
open Conf
open Cmdliner
module C = Ocamlformat.Config_option

let info =
  let doc = "A tool to format OCaml code." in
  let man =
    [ `S Cmdliner.Manpage.s_description
    ; `P "$(tname) automatically formats OCaml code."
    ; `S (C.section_name C.Formatting `Valid)
    ; `P
        "Unless otherwise noted, any option \
         $(b,--)$(i,option)$(b,=)$(i,VAL) detailed in this section can be \
         set in many ways, its value is determined in the following order \
         (of increasing priority): the default value is used if no other \
         value is specified. The value of a boolean option $(b,--foo) or \
         $(b,--no-foo) can be modified in an $(b,.ocamlformat) \
         configuration file with '$(b,foo = ){$(b,true),$(b,false)}', it \
         can be done for any other option with an '$(b,option = )$(i,VAL)' \
         line (*), or using the OCAMLFORMAT environment variable: \
         $(b,OCAMLFORMAT=)$(i,option)$(b,=)$(i,VAL)$(b,,)...$(b,,)$(i,option)$(b,=)$(i,VAL), \
         or as an optional parameter on the command line, or with a global \
         $(b,[@@@ocamlformat \")$(i,option)$(b,=)$(i,VAL)$(b,\"]) attribute \
         in the processed file, or with an $(b,[@@ocamlformat \
         \")$(i,option)$(b,=)$(i,VAL)$(b,\"]) attribute on expression in \
         the processed file."
    ; `P
        "(*) $(b,.ocamlformat) files in current and all ancestor \
         directories for each input file are used, applied from top to \
         bottom, overriding the settings each time a file is applied, \
         stopping at the project root. If no project root and no \
         $(b,ocamlformat) file has been found, and if the option \
         $(b,enable-outside-detected-project) is set, the global \
         $(b,ocamlformat) file defined in $(b,\\$XDG_CONFIG_HOME) (or in \
         $(b,\\$HOME/.config) if $(b,\\$XDG_CONFIG_HOME) is undefined) is \
         used."
    ; `P
        "If the $(b,disable) option is not set, an $(b,.ocamlformat-ignore) \
         file specifies files that OCamlFormat should ignore. Each line in \
         an $(b,.ocamlformat-ignore) file specifies a filename relative to \
         the directory containing the $(b,.ocamlformat-ignore) file. \
         Shell-style regular expressions are supported. Lines starting with \
         $(b,#) are ignored and can be used as comments."
    ; `P
        "If the $(b,disable) option is set, an $(b,.ocamlformat-enable) \
         file specifies files that OCamlFormat should format even when the \
         $(b,disable) option is set. Each line in an \
         $(b,.ocamlformat-enable) file specifies a filename relative to the \
         directory containing the $(b,.ocamlformat-enable) file. \
         Shell-style regular expressions are supported. Lines starting with \
         $(b,#) are ignored and can be used as comments."
    ; `S (C.section_name C.Operational `Valid)
    ; `P
        "Unless mentioned otherwise non-formatting options cannot be set in \
         attributes or $(b,.ocamlformat) files." ]
  in
  Cmd.info "ocamlformat" ~version:Version.current ~doc ~man

let kind = C.Operational

let docs = C.section_name kind `Valid

let enable_outside_detected_project =
  let witness =
    String.concat ~sep:" or "
      (List.map File_system.project_root_witness ~f:(fun name ->
           Format.sprintf "$(b,%s)" name ) )
  in
  let doc =
    Format.sprintf
      "Read $(b,.ocamlformat) config files outside the current project when \
       no project root has been detected for the input file. The project \
       root of an input file is taken to be the nearest ancestor directory \
       that contains a %s file. If $(b,.ocamlformat) config files are \
       located in the same directory or parents they are applied, if no \
       $(b,.ocamlformat) file is found then the global configuration \
       defined in $(b,\\$XDG_CONFIG_HOME/.ocamlformat) (or in \
       $(b,\\$HOME/.config/.ocamlformat) if $(b,\\$XDG_CONFIG_HOME) is \
       undefined) is applied."
      witness
  in
  let default = false in
  mk ~default
    Arg.(value & flag & info ["enable-outside-detected-project"] ~doc ~docs)

let inplace =
  let doc = "Format in-place, overwriting input file(s)." in
  let default = false in
  mk ~default Arg.(value & flag & info ["i"; "inplace"] ~doc ~docs)

(* Other Flags *)

let check =
  let doc =
    "Check whether the input files already are formatted. Mutually \
     exclusive with --inplace and --output."
  in
  mk ~default:false Arg.(value & flag & info ["check"] ~doc ~docs)

type file = Stdin | File of string

let inputs =
  let docv = "SRC" in
  let file_or_dash =
    let parse, print = Arg.non_dir_file in
    let print fmt = function
      | Stdin -> print fmt "<standard input>"
      | File x -> print fmt x
    in
    let parse = function
      | "-" -> `Ok Stdin
      | s -> (
        match parse s with `Ok x -> `Ok (File x) | `Error x -> `Error x )
    in
    (parse, print)
  in
  let doc =
    "Input files. At least one is required, and exactly one without \
     $(b,--inplace). If $(b,-) is passed, will read from stdin."
  in
  let default = [] in
  mk ~default
    Arg.(value & pos_all file_or_dash default & info [] ~doc ~docv ~docs)

let kind : unit -> Syntax.t option =
  let doc = "Parse input as an implementation." in
  let impl = (Some Syntax.Use_file, Arg.info ["impl"] ~doc ~docs) in
  let doc = "Parse input as an interface." in
  let intf = (Some Syntax.Signature, Arg.info ["intf"] ~doc ~docs) in
  let doc = "Deprecated. Same as $(b,impl)." in
  let use_file = (Some Syntax.Use_file, Arg.info ["use-file"] ~doc ~docs) in
  let doc = "Parse input as toplevel phrases with their output." in
  let repl_file =
    (Some Syntax.Repl_file, Arg.info ["repl-file"] ~doc ~docs)
  in
  let doc = "Parse input as an odoc documentation." in
  let doc_file = (Some Syntax.Documentation, Arg.info ["doc"] ~doc ~docs) in
  let default = None in
  mk ~default
    Arg.(value & vflag default [impl; intf; use_file; repl_file; doc_file])

let name =
  let docv = "NAME" in
  let doc =
    "Name of input file for use in error reporting and starting point when \
     searching for '.ocamlformat' files. Defaults to the input file name. \
     Some options can be specified in configuration files named \
     '.ocamlformat' in the same or a parent directory of $(docv), see \
     documentation of other options for details."
  in
  let default = None in
  mk ~default
    Arg.(value & opt (some string) default & info ["name"] ~doc ~docs ~docv)

let numeric =
  let doc =
    "Instead of re-formatting the file, output one integer per line \
     corresponding to the indentation value, printing as many values as \
     lines in the document."
  in
  let default = false in
  mk ~default Arg.(value & flag & info ["numeric"] ~doc ~docs)

let output =
  let docv = "DST" in
  let doc =
    "Output file. Mutually exclusive with --inplace. Write to stdout if \
     omitted."
  in
  let default = None in
  mk ~default
    Arg.(
      value
      & opt (some string) default
      & info ["o"; "output"] ~doc ~docs ~docv )

let print_config =
  let doc =
    "Print the configuration determined by the environment variable, the \
     configuration files, preset profiles and command line. Attributes are \
     not considered. If many input files are specified, only print the \
     configuration for the first file. If no input file is specified, print \
     the configuration for the root directory if specified, or for the \
     current working directory otherwise."
  in
  let default = false in
  mk ~default Arg.(value & flag & info ["print-config"] ~doc ~docs)

let root =
  let docv = "DIR" in
  let doc =
    "Root of the project. If specified, only take into account .ocamlformat \
     configuration files inside $(docv) and its subdirectories."
  in
  let default = None in
  mk ~default
    Arg.(value & opt (some dir) default & info ["root"] ~doc ~docs ~docv)

let kind_of_ext fname =
  match Filename.extension fname with
  | ".ml" | ".mlt" | ".eliom" -> Some Syntax.Use_file
  | ".mli" | ".eliomi" -> Some Syntax.Signature
  | ".mld" -> Some Syntax.Documentation
  | _ -> None

let config =
  let doc =
    "Aggregate options. Options are specified as a comma-separated list of \
     pairs: \
     $(i,option)$(b,=)$(i,VAL)$(b,,)...$(b,,)$(i,option)$(b,=)$(i,VAL)."
  in
  let env = Cmd.Env.info "OCAMLFORMAT" in
  let default = [] in
  let assoc = Arg.(pair ~sep:'=' string string) in
  let list_assoc = Arg.(list ~sep:',' assoc) in
  mk ~default
    Arg.(
      value & opt list_assoc default & info ["c"; "config"] ~doc ~docs ~env )

let disable_conf_files =
  let doc = "Disable .ocamlformat configuration files." in
  mk ~default:false
    Arg.(value & flag & info ["disable-conf-files"] ~doc ~docs)

let ignore_invalid_options =
  let doc = "Ignore invalid options (e.g. in .ocamlformat)." in
  let default = false in
  mk ~default Arg.(value & flag & info ["ignore-invalid-option"] ~doc ~docs)

let ocp_indent_options_doc =
  let alias ocp_indent ocamlformat =
    Printf.sprintf "$(b,%s) is an alias for $(b,%s)." ocp_indent ocamlformat
  in
  let multi_alias ocp_indent l_ocamlformat =
    Format.asprintf "$(b,%s) sets %a." ocp_indent
      (Format.pp_print_list
         ~pp_sep:(fun fs () -> Format.fprintf fs " and ")
         (fun fs x -> Format.fprintf fs "$(b,%s)" x) )
      l_ocamlformat
  in
  [ alias "base" "let-binding-indent"
  ; alias "type" "type-decl-indent"
  ; alias "in" "indent-after-in"
  ; multi_alias "with" ["function-indent"; "match-indent"]
  ; alias "match_clause" "cases-exp-indent"
  ; alias "ppx_stritem_ext" "stritem-extension-indent"
  ; alias "max_indent" "max-indent"
  ; multi_alias "strict_with"
      ["function-indent-nested"; "match-indent-nested"] ]

let ocp_indent_config =
  let doc =
    let open Format in
    let supported =
      match ocp_indent_options_doc with
      | [] -> ""
      | docs ->
          asprintf " %a"
            (pp_print_list
               ~pp_sep:(fun fs () -> fprintf fs "@ ")
               (fun fs s -> fprintf fs "%s" s) )
            docs
    in
    asprintf "Read .ocp-indent configuration files.%s" supported
  in
  let default = false in
  mk ~default Arg.(value & flag & info ["ocp-indent-config"] ~doc ~docs)

(** Do not escape from [build_config] *)
exception Conf_error of string

let failwith_user_errors ~from errors =
  let open Format in
  let pp_error pp e = pp_print_string pp (Config_option.Error.to_string e) in
  let pp_errors = pp_print_list ~pp_sep:pp_print_newline pp_error in
  let msg = asprintf "Error while parsing %s:@ %a" from pp_errors errors in
  raise (Conf_error msg)

let update_from_ocp_indent c loc (oic : IndentConfig.t) =
  let convert_threechoices = function
    | IndentConfig.Always -> `Always
    | Never -> `Never
    | Auto -> `Auto
  in
  let elt v = Conf.Elt.make v (`Updated (`Parsed (`File loc), None)) in
  { c with
    fmt_opts=
      { c.fmt_opts with
        let_binding_indent= elt oic.i_base
      ; type_decl_indent= elt oic.i_type
      ; indent_after_in= elt oic.i_in
      ; function_indent= elt oic.i_with
      ; match_indent= elt oic.i_with
      ; cases_exp_indent= elt oic.i_match_clause
      ; stritem_extension_indent= elt oic.i_ppx_stritem_ext
      ; max_indent= elt oic.i_max_indent
      ; function_indent_nested= elt @@ convert_threechoices oic.i_strict_with
      ; match_indent_nested= elt @@ convert_threechoices oic.i_strict_with }
  }

let read_config_file ?version_check ?disable_conf_attrs conf = function
  | File_system.Ocp_indent file -> (
      let filename = Fpath.to_string file in
      try
        let ocp_indent_conf = IndentConfig.default in
        In_channel.with_file filename ~f:(fun ic ->
            let lines =
              In_channel.input_lines ic
              |> Migrate_ast.Location.of_lines ~filename
            in
            let _ocp_indent_conf, conf, errors =
              List.fold_left lines ~init:(ocp_indent_conf, conf, [])
                ~f:(fun (ocp_indent_conf, conf, errors) {txt= line; loc} ->
                  try
                    let ocp_indent_conf =
                      IndentConfig.update_from_string ocp_indent_conf line
                    in
                    let conf =
                      update_from_ocp_indent conf loc ocp_indent_conf
                    in
                    (ocp_indent_conf, conf, errors)
                  with
                  | Invalid_argument e when ignore_invalid_options () ->
                      warn ~loc "%s" e ;
                      (ocp_indent_conf, conf, errors)
                  | Invalid_argument e ->
                      ( ocp_indent_conf
                      , conf
                      , Config_option.Error.Unknown (e, None) :: errors ) )
            in
            match List.rev errors with
            | [] -> conf
            | l -> failwith_user_errors ~from:filename l )
      with Sys_error _ -> conf )
  | File_system.Ocamlformat file -> (
      let filename = Fpath.to_string file in
      try
        In_channel.with_file filename ~f:(fun ic ->
            let lines =
              In_channel.input_lines ic
              |> Migrate_ast.Location.of_lines ~filename
            in
            let c, errors =
              List.fold_left lines ~init:(conf, [])
                ~f:(fun (conf, errors) {txt= line; loc} ->
                  let from = `File loc in
                  match
                    parse_line ?version_check ?disable_conf_attrs conf ~from
                      line
                  with
                  | Ok conf -> (conf, errors)
                  | Error _ when ignore_invalid_options () ->
                      warn ~loc "ignoring invalid options %S" line ;
                      (conf, errors)
                  | Error e -> (conf, e :: errors) )
            in
            match List.rev errors with
            | [] -> c
            | l -> failwith_user_errors ~from:filename l )
      with Sys_error _ -> conf )

let is_in_listing_file ~listings ~filename =
  let drop_line l = String.is_empty l || String.is_prefix l ~prefix:"#" in
  (* process deeper files first *)
  let listings = List.rev listings in
  List.find_map listings ~f:(fun listing_file ->
      let dir, _ = Fpath.split_base listing_file in
      let listing_filename = Fpath.to_string listing_file in
      try
        In_channel.with_file listing_filename ~f:(fun ch ->
            let lines =
              In_channel.input_lines ch
              |> Migrate_ast.Location.of_lines ~filename:listing_filename
              |> List.filter ~f:(fun Location.{txt= l; _} ->
                     not (drop_line l) )
            in
            List.find_map lines ~f:(fun {txt= line; loc} ->
                match Fpath.of_string line with
                | Ok file_on_current_line -> (
                    let f = Fpath.(dir // file_on_current_line) in
                    if Fpath.equal filename f then Some loc
                    else
                      try
                        let filename = Fpath.to_string filename in
                        let re =
                          let pathname = true and anchored = true in
                          let f = Fpath.to_string f in
                          Re.(Glob.glob ~pathname ~anchored f |> compile)
                        in
                        Option.some_if (Re.execp re filename) loc
                      with Re.Glob.Parse_error ->
                        warn ~loc "pattern %s cannot be parsed." line ;
                        None )
                | Error (`Msg msg) -> warn ~loc "%s." msg ; None ) )
      with Sys_error err ->
        let loc = Location.in_file listing_filename in
        warn ~loc "%s. Ignoring file." err ;
        None )

let update_using_env conf =
  let f (config, errors) (name, value) =
    match C.update ~config ~from:`Env ~name ~value ~inline:false with
    | Ok c -> (c, errors)
    | Error e -> (config, e :: errors)
  in
  let conf, errors = List.fold_left (config ()) ~init:(conf, []) ~f in
  match List.rev errors with
  | [] -> conf
  | l -> failwith_user_errors ~from:"OCAMLFORMAT environment variable" l

let build_config ~enable_outside_detected_project ~root ~file ~is_stdin =
  let vfile = Fpath.v file in
  let file_abs = Fpath.(vfile |> to_absolute |> normalize) in
  let fs =
    File_system.make ~enable_outside_detected_project
      ~disable_conf_files:(disable_conf_files ())
      ~ocp_indent_config:(ocp_indent_config ()) ~root ~file:file_abs
  in
  (* [version-check] can be modified by cmdline (evaluated last) but could
     lead to errors when parsing the .ocamlformat files (evaluated first).
     Similarly, [disable-conf-attrs] could lead to incorrect config. *)
  let forward_conf =
    let read_config_file =
      read_config_file ~version_check:false ~disable_conf_attrs:false
    in
    List.fold fs.configuration_files ~init:default ~f:read_config_file
    |> update_using_env |> C.update_using_cmdline
  in
  let conf =
    let opr_opts =
      { default.opr_opts with
        version_check= forward_conf.opr_opts.version_check
      ; disable_conf_attrs= forward_conf.opr_opts.disable_conf_attrs }
    in
    {default with opr_opts}
  in
  let conf =
    List.fold fs.configuration_files ~init:conf ~f:read_config_file
    |> update_using_env |> C.update_using_cmdline
  in
  if
    (not is_stdin)
    && (not (File_system.has_ocamlformat_file fs))
    && not enable_outside_detected_project
  then (
    (let why =
       match fs.project_root with
       | Some root ->
           Format.sprintf
             "no [.ocamlformat] was found within the project (root: %s)"
             (Fpath.to_string ~relativize:true root)
       | None -> "no project root was found"
     in
     warn ~loc:(Location.in_file file)
       "Ocamlformat disabled because [--enable-outside-detected-project] is \
        not set and %s"
       why ) ;
    Operational.update conf ~f:(fun f ->
        {f with disable= {f.disable with v= true}} ) )
  else
    let listings =
      if conf.opr_opts.disable.v then fs.enable_files else fs.ignore_files
    in
    match is_in_listing_file ~listings ~filename:file_abs with
    | Some loc ->
        let status =
          if conf.opr_opts.disable.v then "enabled" else "ignored"
        in
        if conf.opr_opts.debug.v then
          warn ~loc "%a is %s." Fpath.pp file_abs status ;
        Operational.update conf ~f:(fun f ->
            {f with disable= {f.disable with v= not f.disable.v}} )
    | None -> conf

let build_config ~enable_outside_detected_project ~root ~file ~is_stdin =
  try
    let conf, warn_now =
      collect_warnings (fun () ->
          build_config ~enable_outside_detected_project ~root ~file ~is_stdin )
    in
    if not conf.opr_opts.quiet.v then warn_now () ;
    Ok conf
  with Conf_error msg -> Error msg

type input = {kind: Syntax.t; name: string; file: file; conf: t}

type action =
  | In_out of input * string option
  | Inplace of input list
  | Check of input list
  | Print_config of t
  | Numeric of input

let make_action ~enable_outside_detected_project ~root action inputs =
  let make_file ?name kind file =
    let name = Option.value ~default:file name in
    let+ conf =
      build_config ~enable_outside_detected_project ~root ~file:name
        ~is_stdin:false
    in
    Ok {kind; name; file= File file; conf}
  in
  let make_stdin ?(name = "<standard input>") kind =
    let+ conf =
      build_config ~enable_outside_detected_project ~root ~file:name
        ~is_stdin:false
    in
    Ok {kind; name; file= Stdin; conf}
  in
  let make_input = function
    | `Single_file (kind, name, f) -> make_file ?name kind f
    | `Stdin (name, kind) -> make_stdin ?name kind
  in
  let make_inputs = function
    | (`Single_file _ | `Stdin _) as inp ->
        let+ inp = make_input inp in
        Ok [inp]
    | `Several_files files ->
        let+ inputs =
          List.fold_left files ~init:(Ok []) ~f:(fun acc (kind, file) ->
              let+ acc = acc in
              let+ file = make_file kind file in
              Ok (file :: acc) )
        in
        Ok (List.rev inputs)
  in
  match (action, inputs) with
  | `Print_config, inputs ->
      let file, is_stdin =
        match inputs with
        | `Stdin _ -> ("-", true)
        | `Single_file (_, _, f) -> (f, false)
        | `Several_files ((_, f) :: _) -> (f, false)
        | `Several_files [] | `No_input ->
            (File_system.root_ocamlformat_file ~root |> Fpath.to_string, true)
      in
      let+ conf =
        build_config ~enable_outside_detected_project ~root ~file ~is_stdin
      in
      Ok (Print_config conf)
  | (`No_action | `Output _ | `Inplace | `Check | `Numeric), `No_input ->
      Error "Must specify at least one input file, or `-` for stdin"
  | (`No_action | `Output _ | `Numeric), `Several_files _ ->
      Error
        "Must specify exactly one input file without --inplace or --check"
  | `Inplace, `Stdin _ ->
      Error "Cannot specify stdin together with --inplace"
  | `No_action, ((`Single_file _ | `Stdin _) as inp) ->
      let+ inp = make_input inp in
      Ok (In_out (inp, None))
  | `Output output, ((`Single_file _ | `Stdin _) as inp) ->
      let+ inp = make_input inp in
      Ok (In_out (inp, Some output))
  | `Inplace, ((`Single_file _ | `Several_files _) as inputs) ->
      let+ inputs = make_inputs inputs in
      Ok (Inplace inputs)
  | `Check, ((`Single_file _ | `Several_files _ | `Stdin _) as inputs) ->
      let+ inputs = make_inputs inputs in
      Ok (Check inputs)
  | `Numeric, ((`Stdin _ | `Single_file _) as inp) ->
      let+ inp = make_input inp in
      Ok (Numeric inp)

let validate_inputs () =
  match (inputs (), kind (), name ()) with
  | [], _, _ -> Ok `No_input
  | [Stdin], None, None ->
      Error
        "Must specify at least one of --name, --impl or --intf when reading \
         from stdin"
  | [Stdin], Some kind, name -> Ok (`Stdin (name, kind))
  | [Stdin], None, Some name -> (
    match kind_of_ext name with
    | Some kind -> Ok (`Stdin (Some name, kind))
    | None ->
        Error
          "Cannot deduce file kind from passed --name. Please specify \
           --impl or --intf" )
  | [File f], Some kind, name -> Ok (`Single_file (kind, name, f))
  | [File f], None, name ->
      let kind =
        Option.value ~default:f name
        |> kind_of_ext
        |> Option.value ~default:Syntax.Use_file
      in
      Ok (`Single_file (kind, name, f))
  | _ :: _ :: _, Some _, _ ->
      Error "Cannot specify --impl or --intf with multiple inputs"
  | _ :: _ :: _, _, Some _ ->
      Error "Cannot specify --name with multiple inputs"
  | (_ :: _ :: _ as inputs), None, None ->
      List.map inputs ~f:(function
        | Stdin -> Error "Cannot specify stdin together with other inputs"
        | File f ->
            let kind = Option.value ~default:Use_file (kind_of_ext f) in
            Ok (kind, f) )
      |> Result.all
      |> Result.map ~f:(fun files -> `Several_files files)

let validate_action () =
  match
    List.filter_map
      ~f:(fun s -> s)
      [ Option.map ~f:(fun o -> (`Output o, "--output")) (output ())
      ; Option.some_if (inplace ()) (`Inplace, "--inplace")
      ; Option.some_if (check ()) (`Check, "--check")
      ; Option.some_if (print_config ()) (`Print_config, "--print-config")
      ; Option.some_if (numeric ()) (`Numeric, "--numeric") ]
  with
  | [] -> Ok `No_action
  | [(action, _)] -> Ok action
  | (_, a1) :: (_, a2) :: _ ->
      Error (Printf.sprintf "Cannot specify %s with %s" a1 a2)

let validate () =
  let root =
    Option.map (root ()) ~f:Fpath.(fun x -> v x |> to_absolute |> normalize)
  in
  let enable_outside_detected_project =
    enable_outside_detected_project () && Option.is_none root
  in
  match
    let+ action = validate_action () in
    let+ inputs = validate_inputs () in
    make_action ~enable_outside_detected_project ~root action inputs
  with
  | Error e -> `Error (false, e)
  | Ok action -> `Ok action

let action () = parse info validate

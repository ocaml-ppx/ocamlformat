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

let project_root_witness = [".git"; ".hg"; "dune-project"]

let is_project_root ~root dir =
  match root with
  | Some root -> Fpath.equal dir root
  | None ->
      List.exists project_root_witness ~f:(fun name ->
          Fpath.(exists (dir / name)) )

let dot_ocp_indent = ".ocp-indent"

let dot_ocamlformat = ".ocamlformat"

let dot_ocamlformat_ignore = ".ocamlformat-ignore"

let dot_ocamlformat_enable = ".ocamlformat-enable"

type configuration_file = Ocamlformat of Fpath.t | Ocp_indent of Fpath.t

let root_ocamlformat_file ~root =
  let root = Option.value root ~default:(Fpath.cwd ()) in
  Fpath.(root / dot_ocamlformat)

let xdg_config () =
  let xdg_config_home =
    match Stdlib.Sys.getenv_opt "XDG_CONFIG_HOME" with
    | None | Some "" -> (
      match Stdlib.Sys.getenv_opt "HOME" with
      | None | Some "" -> None
      | Some home -> Some Fpath.(v home / ".config") )
    | Some xdg_config_home -> Some (Fpath.v xdg_config_home)
  in
  match xdg_config_home with
  | Some xdg_config_home ->
      let filename = Fpath.(xdg_config_home / "ocamlformat") in
      if Fpath.exists filename then Some filename else None
  | None -> None

type t =
  { ignore_files: Fpath.t list
  ; enable_files: Fpath.t list
  ; configuration_files: configuration_file list
  ; project_root: Fpath.t option }

let make ~enable_outside_detected_project ~disable_conf_files
    ~ocp_indent_config ~root ~file =
  let dir = Fpath.(file |> split_base |> fst) in
  let volume, dir = Fpath.split_volume dir in
  let segs = Fpath.segs dir |> List.rev in
  let rec aux fs ~segs =
    match segs with
    | [] | [""] ->
        (* Outside of a detected project, only apply the global config file
           when [--enable-outside-detected-project] is set and no
           [.ocamlformat] file has been found. *)
        assert (Option.is_none fs.project_root) ;
        if
          List.is_empty fs.configuration_files
          && enable_outside_detected_project
        then
          match xdg_config () with
          | Some xdg -> {fs with configuration_files= [Ocamlformat xdg]}
          | None -> fs
        else fs
    | "" :: upper_segs -> aux fs ~segs:upper_segs
    | _ :: upper_segs ->
        let sep = Fpath.dir_sep in
        let dir = Fpath.v (volume ^ String.concat ~sep (List.rev segs)) in
        let fs =
          { fs with
            ignore_files=
              (let filename = Fpath.(dir / dot_ocamlformat_ignore) in
               if Fpath.exists filename then filename :: fs.ignore_files
               else fs.ignore_files )
          ; enable_files=
              (let filename = Fpath.(dir / dot_ocamlformat_enable) in
               if Fpath.exists filename then filename :: fs.enable_files
               else fs.enable_files )
          ; configuration_files=
              ( if disable_conf_files then []
                else
                  let f_1 = Fpath.(dir / dot_ocamlformat) in
                  let files =
                    if Fpath.exists f_1 then
                      Ocamlformat f_1 :: fs.configuration_files
                    else fs.configuration_files
                  in
                  if ocp_indent_config then
                    let f_2 = Fpath.(dir / dot_ocp_indent) in
                    if Fpath.exists f_2 then Ocp_indent f_2 :: files
                    else files
                  else files ) }
        in
        (* Inside a detected project, configs are applied in top-down
           starting from the project root (i.e. excluding the global config
           file). *)
        if is_project_root ~root dir then {fs with project_root= Some dir}
        else aux fs ~segs:upper_segs
  in
  aux ~segs
    { ignore_files= []
    ; enable_files= []
    ; configuration_files= []
    ; project_root= None }

let has_ocamlformat_file fs =
  List.exists fs.configuration_files ~f:(function
    | Ocamlformat _ -> true
    | Ocp_indent _ -> false )

(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2019-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

module type CONFIG = sig
  type config

  val profile_option_names : string list
end

module Make (C : CONFIG) = struct
  open Cmdliner

  type config = C.config

  type parsed_from = [`File of Fpath.t * int | `Attribute]

  type updated_from = [`Env | `Commandline | `Parsed of parsed_from]

  type from =
    [`Default | `Profile of string * updated_from | `Updated of updated_from]

  type 'a t =
    { names: string list
    ; parse: string -> ('a, [`Msg of string]) Result.t
    ; update: config -> 'a -> config
    ; allow_inline: bool
    ; cmdline_get: unit -> 'a option
    ; to_string: 'a -> string
    ; default: 'a
    ; get_value: config -> 'a
    ; from: from
    ; deprecated: bool }

  type 'a option_decl =
       names:string list
    -> doc:string
    -> section:[`Formatting | `Operational]
    -> ?allow_inline:bool
    -> ?deprecated:bool
    -> (config -> 'a -> config)
    -> (config -> 'a)
    -> 'a t

  type pack = Pack : 'a t -> pack

  let store = ref []

  let in_attributes ~section cond =
    if cond || Poly.(section = `Operational) then ""
    else " Cannot be set in attributes."

  let deprecated_doc ~deprecated =
    if deprecated then " Warning: This option is deprecated." else ""

  let generated_flag_doc ~allow_inline ~doc ~section ~deprecated =
    Format.sprintf "%s%s%s" doc
      (in_attributes ~section allow_inline)
      (deprecated_doc ~deprecated)

  let generated_doc conv ~allow_inline ~doc ~section ~default ~deprecated =
    let default = Format.asprintf "%a" (Arg.conv_printer conv) default in
    let default = if String.is_empty default then "none" else default in
    Format.sprintf "%s The default value is $(b,%s).%s%s" doc default
      (in_attributes ~section allow_inline)
      (deprecated_doc ~deprecated)

  let section_name = function
    | `Formatting -> Cmdliner.Manpage.s_options ^ " (CODE FORMATTING STYLE)"
    | `Operational -> Cmdliner.Manpage.s_options

  let from = `Default

  let flag ~default ~names ~doc ~section
      ?(allow_inline = Poly.(section = `Formatting)) ?(deprecated = false)
      update get_value =
    let open Cmdliner in
    let invert_flag = default in
    let names_for_cmdline =
      if invert_flag then
        List.filter_map names ~f:(fun n ->
            if String.length n = 1 then None else Some ("no-" ^ n))
      else names
    in
    let doc = generated_flag_doc ~allow_inline ~doc ~section ~deprecated in
    let docs = section_name section in
    let term = Arg.(value & flag & info names_for_cmdline ~doc ~docs) in
    let parse = Arg.(conv_parser bool) in
    let r = mk ~default term in
    let to_string = Bool.to_string in
    let cmdline_get () = if !r then Some (not invert_flag) else None in
    let opt =
      { names
      ; parse
      ; update
      ; cmdline_get
      ; allow_inline
      ; default
      ; to_string
      ; get_value
      ; from
      ; deprecated }
    in
    store := Pack opt :: !store ;
    opt

  let any converter ~default ~docv ~names ~doc ~section
      ?(allow_inline = Poly.(section = `Formatting)) ?(deprecated = false)
      update get_value =
    let open Cmdliner in
    let doc =
      generated_doc converter ~allow_inline ~doc ~section ~default
        ~deprecated
    in
    let docs = section_name section in
    let term =
      Arg.(value & opt (some converter) None & info names ~doc ~docs ~docv)
    in
    let parse s = Arg.conv_parser converter s in
    let r = mk ~default:None term in
    let to_string x =
      let s = Format.asprintf "%a%!" (Arg.conv_printer converter) x in
      if String.is_empty s then "none" else s
    in
    let cmdline_get () = !r in
    let opt =
      { names
      ; parse
      ; update
      ; cmdline_get
      ; allow_inline
      ; default
      ; to_string
      ; get_value
      ; from
      ; deprecated }
    in
    store := Pack opt :: !store ;
    opt

  let choice ~all ~names ~doc ~section
      ?(allow_inline = Poly.(section = `Formatting)) ?(deprecated = false) =
    let _, default, _ = List.hd_exn all in
    let opt_names = List.map all ~f:(fun (x, y, _) -> (x, y)) in
    let conv = Arg.enum opt_names in
    let doc =
      let open Format in
      asprintf "%s %a" doc
        (pp_print_list
           ~pp_sep:(fun fs () -> fprintf fs "@,")
           (fun fs (_, _, d) -> fprintf fs "%s" d))
        all
    in
    let docv =
      let open Format in
      asprintf "@[<1>{%a}@]"
        (pp_print_list
           ~pp_sep:(fun fs () -> fprintf fs "@,|")
           (fun fs (v, _, _) -> fprintf fs "%s" v))
        all
    in
    any conv ~default ~docv ~names ~doc ~section ~allow_inline ~deprecated

  let update_from config name from =
    let is_profile_option_name x =
      List.exists C.profile_option_names ~f:(String.equal x)
    in
    let on_pack (Pack {names; get_value; to_string; _}) =
      if is_profile_option_name (List.hd_exn names) then
        Some (to_string (get_value config))
      else None
    in
    let on_pack (Pack ({names; deprecated; _} as p)) =
      if is_profile_option_name name then
        if is_profile_option_name (List.hd_exn names) then
          (* updating --profile option *)
          Pack {p with from= `Updated from}
        else
          let profile_name = List.find_map_exn !store ~f:on_pack in
          (* updating other options when --profile is set *)
          Pack {p with from= `Profile (profile_name, from)}
      else if List.exists names ~f:(String.equal name) then (
        (* updating a single option (without setting a profile) *)
        if deprecated then
          Format.fprintf Format.err_formatter
            "Warning: option %s is deprecated\n%!" name ;
        Pack {p with from= `Updated from} )
      else Pack p
    in
    store := List.map !store ~f:on_pack

  let update ~config ~from ~name ~value ~inline =
    List.find_map !store
      ~f:(fun (Pack {names; parse; update; allow_inline; _}) ->
        if List.exists names ~f:(String.equal name) then
          if inline && not allow_inline then
            Some (Error (`Misplaced (name, value)))
          else
            match parse value with
            | Ok packed_value ->
                let config = update config packed_value in
                update_from config name from ;
                Some (Ok config)
            | Error (`Msg error) -> Some (Error (`Bad_value (name, error)))
        else None)
    |> Option.value ~default:(Error (`Unknown (name, value)))

  let default {default; _} = default

  let update_using_cmdline config =
    let on_pack config (Pack {cmdline_get; update; names; _}) =
      match cmdline_get () with
      | None -> config
      | Some x ->
          let config = update config x in
          update_from config (List.hd_exn names) `Commandline ;
          config
    in
    List.fold !store ~init:config ~f:on_pack

  let print_config c =
    let longest =
      let compare x y = compare (String.length x) (String.length y) in
      List.max_elt ~compare
    in
    let on_pack (Pack {names; to_string; get_value; from; _}) =
      let name = Option.value_exn (longest names) in
      let value = to_string (get_value c) in
      let aux_from = function
        | `Parsed (`File (p, i)) ->
            Format.sprintf " (file %s:%i)"
              (Fpath.to_string ~relativize:true p)
              i
        | `Parsed `Attribute -> " (attribute)"
        | `Env -> " (environment variable)"
        | `Commandline -> " (command line)"
      in
      let aux_from = function
        | `Default -> ""
        | `Profile (s, p) -> " (profile " ^ s ^ aux_from p ^ ")"
        | `Updated x -> aux_from x
      in
      Format.eprintf "%s=%s%s\n%!" name value (aux_from from)
    in
    List.iter !store ~f:on_pack
end

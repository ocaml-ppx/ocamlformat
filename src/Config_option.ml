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
    ; parse: string -> ('a, string) Result.t
    ; update: config -> 'a -> config
    ; allow_inline: bool
    ; cmdline_get: unit -> 'a option
    ; to_string: 'a -> string
    ; default: 'a
    ; get_value: config -> 'a
    ; from: from }

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

  let generated_choice_doc ~allow_inline ~all ~doc ~section ~has_default
      ~deprecated =
    let open Format in
    let default =
      if has_default then
        asprintf "The default value is $(b,%a)."
          (fun fs (v, _, _) -> fprintf fs "%s" v)
          (List.hd_exn all)
      else ""
    in
    asprintf "%s %a %s%s%s" doc
      (pp_print_list
         ~pp_sep:(fun fs () -> fprintf fs "@,")
         (fun fs (_, _, d) -> fprintf fs "%s" d))
      all default
      (in_attributes ~section allow_inline)
      (deprecated_doc ~deprecated)

  let generated_choice_docv ~all =
    let open Format in
    asprintf "@[<1>{%a}@]"
      (pp_print_list
         ~pp_sep:(fun fs () -> fprintf fs "@,|")
         (fun fs (v, _, _) -> fprintf fs "%s" v))
      all

  let generated_flag_doc ~allow_inline ~doc ~section ~deprecated =
    Format.sprintf "%s%s%s" doc
      (in_attributes ~section allow_inline)
      (deprecated_doc ~deprecated)

  let generated_int_doc ~allow_inline ~doc ~section ~default ~deprecated =
    let default = Format.sprintf "The default value is $(b,%i)." default in
    Format.sprintf "%s %s%s%s" doc default
      (in_attributes ~section allow_inline)
      (deprecated_doc ~deprecated)

  let generated_opt_doc ~allow_inline ~doc ~section ~deprecated =
    let default = "The default value is $(b,none)." in
    Format.sprintf "%s %s%s%s" doc default
      (in_attributes ~section allow_inline)
      (deprecated_doc ~deprecated)

  let section_name = function
    | `Formatting -> Cmdliner.Manpage.s_options ^ " (CODE FORMATTING STYLE)"
    | `Operational -> Cmdliner.Manpage.s_options

  let from = `Default

  let choice ?(has_default = true) ~all ~names ~doc ~section
      ?(allow_inline = Poly.(section = `Formatting)) ?(deprecated = false)
      update get_value =
    let _, default, _ = List.hd_exn all in
    let doc =
      generated_choice_doc ~allow_inline ~all ~doc ~section ~has_default
        ~deprecated
    in
    let docv = generated_choice_docv ~all in
    let opt_names = List.map all ~f:(fun (x, y, _) -> (x, y)) in
    let to_string v' =
      List.find_map_exn all ~f:(fun (str, v, _) ->
          if Poly.(v = v') then Some str else None)
    in
    let docs = section_name section in
    let term =
      Arg.(
        value
        & opt (some (enum opt_names)) None
        & info names ~doc ~docv ~docs)
    in
    let parse s =
      match
        List.find_map all ~f:(fun (n, v, _) ->
            Option.some_if (String.equal n s) v)
      with
      | Some v -> Ok v
      | None ->
          Error
            (Printf.sprintf "Invalid value '%s', expecting %s" s
               ( List.map all ~f:(fun (s, _, _) -> Format.sprintf "'%s'" s)
               |> String.concat ~sep:" or " ))
    in
    let r = mk ~default:None term in
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
      ; from }
    in
    store := Pack opt :: !store ;
    opt

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
    let parse s =
      try Ok (Bool.of_string s)
      with _ ->
        Error
          (Format.sprintf "invalid value '%s', expecting 'true' or 'false'"
             s)
    in
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
      ; from }
    in
    store := Pack opt :: !store ;
    opt

  let int ~default ~docv ~names ~doc ~section
      ?(allow_inline = Poly.(section = `Formatting)) ?(deprecated = false)
      update get_value =
    let open Cmdliner in
    let doc =
      generated_int_doc ~allow_inline ~doc ~section ~default ~deprecated
    in
    let docs = section_name section in
    let term =
      Arg.(value & opt (some int) None & info names ~doc ~docs ~docv)
    in
    let parse s =
      try Ok (Int.of_string s)
      with _ ->
        Error (Format.sprintf "invalid value '%s', expecting an integer" s)
    in
    let r = mk ~default:None term in
    let to_string = Int.to_string in
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
      ; from }
    in
    store := Pack opt :: !store ;
    opt

  let opt ~type_ ~type_name ~of_string ~to_string ~docv ~names ~doc ~section
      ?(allow_inline = Poly.(section = `Formatting)) ?(deprecated = false)
      update get_value =
    let open Cmdliner in
    let default = None in
    let doc = generated_opt_doc ~allow_inline ~doc ~section ~deprecated in
    let docs = section_name section in
    let term =
      Arg.(
        value & opt (some (some type_)) None & info names ~doc ~docs ~docv)
    in
    let parse s =
      if String.equal s "none" then Ok None
      else
        try Ok (Some (of_string s))
        with _ ->
          Error
            (Format.sprintf "invalid value '%s', expecting %s" s type_name)
    in
    let r = mk ~default term in
    let to_string = Option.value_map ~default:"none" ~f:to_string in
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
      ; from }
    in
    store := Pack opt :: !store ;
    opt

  let int_opt =
    opt ~type_:Arg.int ~type_name:"integer" ~of_string:Int.of_string
      ~to_string:Int.to_string

  let update_from config name from =
    let is_profile_option_name x =
      List.exists C.profile_option_names ~f:(String.equal x)
    in
    let on_pack (Pack {names; get_value; to_string; _}) =
      if is_profile_option_name (List.hd_exn names) then
        Some (to_string (get_value config))
      else None
    in
    let on_pack (Pack ({names; _} as p)) =
      if is_profile_option_name name then
        if is_profile_option_name (List.hd_exn names) then
          (* updating --profile option *)
          Pack {p with from= `Updated from}
        else
          let profile_name = List.find_map_exn !store ~f:on_pack in
          (* updating other options when --profile is set *)
          Pack {p with from= `Profile (profile_name, from)}
      else if List.exists names ~f:(String.equal name) then
        (* updating a single option (without setting a profile) *)
        Pack {p with from= `Updated from}
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
            | Error error -> Some (Error (`Bad_value (name, error)))
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

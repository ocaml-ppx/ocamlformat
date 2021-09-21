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

module type CONFIG = sig
  type config

  val profile_option_names : string list

  val warn : config -> ('a, Format.formatter, unit, unit) format4 -> 'a
end

module Make (C : CONFIG) = struct
  open Cmdliner

  type config = C.config

  type kind = Formatting | Operational

  type parsed_from = [`File of Fpath.t * int | `Attribute]

  type updated_from = [`Env | `Commandline | `Parsed of parsed_from]

  type from =
    [`Default | `Profile of string * updated_from | `Updated of updated_from]

  type deprecated = {dmsg: string; dversion: string}

  type removed = {rmsg: string; rversion: string}

  type status = [`Valid | `Deprecated of deprecated | `Removed of removed]

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
    ; status: status }

  type 'a option_decl =
       names:string list
    -> doc:string
    -> kind:kind
    -> ?allow_inline:bool
    -> ?status:[`Valid | `Deprecated of deprecated]
    -> (config -> 'a -> config)
    -> (config -> 'a)
    -> 'a t

  type pack = Pack : 'a t -> pack

  let store = ref []

  let deprecated ~since_version:dversion dmsg = {dmsg; dversion}

  let removed ~since_version:rversion rmsg = {rmsg; rversion}

  let in_attributes cond = function
    | Operational -> ""
    | Formatting -> if cond then "" else " Cannot be set in attributes."

  let pp_deprecated ppf {dmsg; dversion= v} =
    Format.fprintf ppf "This option is deprecated since version %s. %s" v
      dmsg

  let pp_removed ppf {rmsg; rversion= v} =
    Format.fprintf ppf "This option has been removed in version %s. %s" v
      rmsg

  let status_doc ppf = function
    | `Valid -> ()
    | `Deprecated x -> Format.fprintf ppf " Warning: %a" pp_deprecated x
    | `Removed _ -> ()

  let generated_flag_doc ~allow_inline ~doc ~kind ~default ~status =
    let default = if default then "set" else "unset" in
    Format.asprintf "%s The flag is $(b,%s) by default.%s%a" doc default
      (in_attributes allow_inline kind)
      status_doc status

  let generated_doc ?default_doc conv ~allow_inline ~doc ~kind ~default
      ~status =
    let default_doc =
      match default_doc with
      | Some x -> x
      | None -> Format.asprintf "%a" (Arg.conv_printer conv) default
    in
    let default =
      if String.is_empty default_doc then "none" else default_doc
    in
    Format.asprintf "%s The default value is $(b,%s).%s%a" doc default
      (in_attributes allow_inline kind)
      status_doc status

  let section_name kind status =
    match status with
    | `Valid -> (
      match kind with
      | Formatting -> Cmdliner.Manpage.s_options ^ " (CODE FORMATTING STYLE)"
      | Operational -> Cmdliner.Manpage.s_options )
    | `Deprecated _ -> Cmdliner.Manpage.s_options ^ " (DEPRECATED)"
    | `Removed _ -> Cmdliner.Manpage.s_options ^ " (REMOVED)"

  let from = `Default

  let longest =
    let compare x y = compare (String.length x) (String.length y) in
    List.max_elt ~compare

  (* somehow necessary *)
  let map_status : [`Valid | `Deprecated of deprecated] -> status = function
    | `Valid -> `Valid
    | `Deprecated x -> `Deprecated x

  let flag ~default ~names ~doc ~kind
      ?(allow_inline = Poly.(kind = Formatting)) ?(status = `Valid) update
      get_value =
    let open Cmdliner in
    let invert_names =
      List.filter_map names ~f:(fun n ->
          if String.length n = 1 then None else Some ("no-" ^ n) )
    in
    let doc = generated_flag_doc ~allow_inline ~doc ~kind ~default ~status in
    let invert_doc = "Unset $(b," ^ List.last_exn names ^ ")." in
    let docs = section_name kind status in
    let term =
      Arg.(
        value
        & vflag None
            [ (Some true, info names ~doc ~docs)
            ; (Some false, info invert_names ~doc:invert_doc ~docs) ])
    in
    let parse = Arg.conv_parser Arg.bool in
    let r = mk ~default:None term in
    let to_string = Bool.to_string in
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
      ; status= map_status status }
    in
    store := Pack opt :: !store ;
    opt

  let any ?default_doc converter ~default ~docv ~names ~doc ~kind
      ?(allow_inline = Poly.(kind = Formatting)) ?(status = `Valid) update
      get_value =
    let open Cmdliner in
    let doc =
      generated_doc converter ?default_doc ~allow_inline ~doc ~kind ~default
        ~status
    in
    let docs = section_name kind status in
    let term =
      Arg.(value & opt (some converter) None & info names ~doc ~docs ~docv)
    in
    let parse = Arg.conv_parser converter in
    let r = mk ~default:None term in
    let to_string = Format.asprintf "%a%!" (Arg.conv_printer converter) in
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
      ; status= map_status status }
    in
    store := Pack opt :: !store ;
    opt

  module Value = struct
    type 'a t = string * 'a * string * [`Valid | `Deprecated of deprecated]

    let make ?deprecated ~name value doc =
      match deprecated with
      | None -> (name, value, doc, `Valid)
      | Some x -> (name, value, doc, `Deprecated x)

    let pp_deprecated s ppf {dmsg= msg; dversion= v} =
      Format.fprintf ppf "Value `%s` is deprecated since version %s. %s" s v
        msg

    let pp_deprecated_with_name ~opt ~val_ ppf {dmsg= msg; dversion= v} =
      Format.fprintf ppf
        "option `%s`: value `%s` is deprecated since version %s. %s" opt val_
        v msg

    let status_doc s ppf = function
      | `Valid -> ()
      | `Deprecated x ->
          Format.fprintf ppf " Warning: %a" (pp_deprecated s) x

    let warn_if_deprecated conf opt (s, _, _, status) =
      match status with
      | `Valid -> ()
      | `Deprecated d ->
          C.warn conf "%a" (pp_deprecated_with_name ~opt ~val_:s) d
  end

  module Value_removed = struct
    type t = {name: string; version: string; msg: string}

    let make ~name ~version ~msg = {name; version; msg}

    let make_list ~names ~version ~msg =
      List.map names ~f:(fun name -> make ~name ~version ~msg)

    let add_parse_errors values conv =
      let parse s =
        match List.find values ~f:(fun {name; _} -> String.equal name s) with
        | Some {name; version; msg} ->
            Format.kasprintf
              (fun s -> Error (`Msg s))
              "value `%s` has been removed in version %s. %s" name version
              msg
        | None -> Arg.conv_parser conv s
      in
      Arg.conv (parse, Arg.conv_printer conv)
  end

  let choice ~all ?(removed_values = []) ~names ~doc ~kind
      ?(allow_inline = Poly.(kind = Formatting)) ?status update =
    let _, default, _, _ = List.hd_exn all in
    let name = Option.value_exn (longest names) in
    let opt_names = List.map all ~f:(fun (x, y, _, _) -> (x, y)) in
    let conv =
      Value_removed.add_parse_errors removed_values (Arg.enum opt_names)
    in
    let doc =
      let open Format in
      asprintf "%s %a" doc
        (pp_print_list
           ~pp_sep:(fun fs () -> fprintf fs "@,")
           (fun fs (s, _, d, st) ->
             fprintf fs "%s%a" d (Value.status_doc s) st ) )
        all
    in
    let docv =
      let open Format in
      asprintf "@[<1>{%a}@]"
        (pp_print_list
           ~pp_sep:(fun fs () -> fprintf fs "@,|")
           (fun fs (v, _, _, _) -> fprintf fs "%s" v) )
        all
    in
    let update conf x =
      ( match List.find all ~f:(fun (_, v, _, _) -> Poly.(x = v)) with
      | Some value -> Value.warn_if_deprecated conf name value
      | None -> () ) ;
      update conf x
    in
    any conv ~default ~docv ~names ~doc ~kind ~allow_inline ?status update

  let removed_option ~names ~version ~msg =
    let removed = {rversion= version; rmsg= msg} in
    let status = `Removed removed in
    let msg = Format.asprintf "%a" pp_removed removed in
    let parse _ = Error (`Msg msg) in
    let converter = Arg.conv (parse, fun _ () -> ()) in
    let update conf _ = conf and get_value _ = () in
    let kind = (* not used *) Formatting in
    let docs = section_name kind status in
    let term =
      Arg.(value & opt (some converter) None & info names ~doc:msg ~docs)
    in
    let r = mk ~default:None term in
    let to_string _ = "" in
    let cmdline_get () = !r in
    let opt =
      { names
      ; parse
      ; update
      ; cmdline_get
      ; allow_inline= true
      ; default= ()
      ; to_string
      ; get_value
      ; from
      ; status }
    in
    store := Pack opt :: !store

  let update_from config name from =
    let is_profile_option_name x =
      List.exists C.profile_option_names ~f:(String.equal x)
    in
    let on_pack (Pack {names; get_value; to_string; _}) =
      if is_profile_option_name (List.hd_exn names) then
        Some (to_string (get_value config))
      else None
    in
    let on_pack (Pack ({names; status; _} as p)) =
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
        ( match status with
        | `Deprecated d -> C.warn config "%s: %a" name pp_deprecated d
        | _ -> () ) ;
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
        else None )
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
    let on_pack (Pack {names; to_string; get_value; from; status; _}) =
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
      match status with
      | `Valid | `Deprecated _ ->
          Format.eprintf "%s=%s%s\n%!" name value (aux_from from)
      | `Removed _ -> ()
    in
    List.iter !store ~f:on_pack
end

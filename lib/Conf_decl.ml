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

module Error = Conf_t.Error

let ocaml_version_conv =
  let parse x =
    match Ocaml_version.of_string x with
    | Ok x -> `Ok x
    | Error (`Msg x) -> `Error x
  in
  (parse, Ocaml_version.pp)

type typ = Int | Bool | Ocaml_version | Choice of string list

module UI = struct
  type 'config t =
    { names: string list
    ; values: typ
    ; doc: string
    ; update: 'config -> string -> Conf_t.updated_from -> 'config }
end

open Cmdliner

type kind = Formatting | Operational
(* type from = [ `Default | `Profile of string * updated_from | `Updated of
   updated_from * from option (* when redundant definition *) ] *)

type deprecated = {dmsg: string; dversion: Version.t}

type removed = {rmsg: string; rversion: Version.t}

type status = [`Valid | `Deprecated of deprecated | `Removed of removed]

type 'a t =
  { names: string list
  ; values: typ
  ; doc: string
  ; parse: string -> ('a, [`Msg of string]) Result.t
  ; update: Conf_t.t -> 'a Conf_t.elt -> Conf_t.t
  ; allow_inline: bool
  ; term: 'a option Term.t
  ; to_string: 'a -> string
  ; default: 'a
  ; get_value: Conf_t.t -> 'a Conf_t.elt (* ; from: from *)
  ; status: status }

let update_from_elt ~redundant elt updated_from =
  let from = Conf_t.Elt.from elt in
  let from =
    match from with
    | `Profile _ ->
        `Updated (updated_from, if redundant then Some from else None)
    | _ -> `Updated (updated_from, None)
  in
  let v = Conf_t.Elt.v elt in
  Conf_t.Elt.make v from

let update_v_elt elt v =
  let from = Conf_t.Elt.from elt in
  Conf_t.Elt.make v from

let update_elt ~redundant elt new_v updated_from =
  update_v_elt (update_from_elt ~redundant elt updated_from) new_v

let to_ui option =
  let update conf str from =
    match option.parse str with
    | Ok x ->
        let elt = option.get_value conf in
        let redundant =
          String.equal
            (option.to_string (Conf_t.Elt.v elt))
            (option.to_string x)
        in
        option.update conf (update_elt ~redundant elt x from)
    | Error _ -> conf
  in
  UI.{names= option.names; values= option.values; doc= option.doc; update}

type 'a declarator =
     names:string list
  -> default:Conf_t.t
  -> doc:string
  -> kind:kind
  -> ?allow_inline:bool
  -> ?status:[`Valid | `Deprecated of deprecated]
  -> (Conf_t.t -> 'a Conf_t.elt -> Conf_t.t)
  -> (Conf_t.t -> 'a Conf_t.elt)
  -> 'a t

type pack = Pack : 'a t -> pack

module Store = struct
  type elt = pack

  type t = elt list

  type store = t

  let elt opt = Pack opt

  let empty = []

  let add store ele = Pack ele :: store

  let merge s1 s2 = s1 @ s2

  let to_ui = List.map ~f:(fun (Pack opt) -> to_ui opt)

  let to_term store =
    let compose_terms (t1 : ('a -> 'b) Term.t) (t2 : ('b -> 'c) Term.t) :
        ('a -> 'c) Term.t =
      let open Term in
      const (fun f1 f2 a -> f2 (f1 a)) $ t1 $ t2
    in
    let compose (acc : (Conf_t.t -> Conf_t.t) Term.t)
        (Pack {term; update; get_value; to_string; _}) =
      let open Term in
      let update_term =
        const (fun x ->
            match x with
            | None -> fun config -> config
            | Some x ->
                let a config =
                  let redundant =
                    String.equal (to_string x)
                      (config |> get_value |> Conf_t.Elt.v |> to_string)
                  in
                  let elt = get_value config in
                  let new_elt = update_elt ~redundant elt x `Commandline in
                  let config = update config new_elt in
                  config
                in
                a )
        $ term
      in
      compose_terms update_term acc
    in
    let term =
      List.fold_left ~init:(Term.const (fun x -> x)) ~f:compose store
    in
    term
end

let deprecated ~since:dversion dmsg = {dmsg; dversion}

let removed ~since:rversion rmsg = {rmsg; rversion}

let in_attributes cond = function
  | Operational -> ""
  | Formatting -> if cond then "" else " Cannot be set in attributes."

let maybe_empty = function "" -> "" | x -> " " ^ x

let pp_deprecated ppf {dmsg; dversion= v} =
  Format.fprintf ppf "This option is deprecated since version %a.%s"
    Version.pp v (maybe_empty dmsg)

let pp_removed ppf {rmsg; rversion= v} =
  Format.fprintf ppf "This option has been removed in version %a.%s"
    Version.pp v (maybe_empty rmsg)

let pp_from_src fs = function
  | `Parsed pf ->
      let kind, Location.{loc_start= {pos_fname; pos_lnum; _}; _} =
        match pf with
        | `File loc -> ("file", loc)
        | `Attribute loc -> ("attribute", loc)
      in
      let fname = Fpath.to_string ~relativize:true (Fpath.v pos_fname) in
      Format.fprintf fs " (%s %s:%i)" kind fname pos_lnum
  | `Env -> Format.fprintf fs " (environment variable)"
  | `Commandline -> Format.fprintf fs " (command line)"

let rec pp_from fs = function
  | `Default -> ()
  | `Profile (s, p) -> Format.fprintf fs " (profile %s%a)" s pp_from_src p
  | `Updated (x, None) -> pp_from_src fs x
  | `Updated (x, Some r) ->
      Format.fprintf fs "%a -- Warning (redundant): %a" pp_from_src x pp_from
        r

let loc_udapted_from = function
  | `Commandline -> Location.in_file "<command-line>"
  | `Env -> Location.in_file "<env>"
  | `Parsed (`File loc) -> loc
  | `Parsed (`Attribute loc) -> loc

let loc_from = function
  | `Default -> Location.in_file "<default>"
  | `Profile (_, updated_from) | `Updated (updated_from, _) ->
      loc_udapted_from updated_from

let status_doc ppf = function
  | `Valid -> ()
  | `Deprecated x -> Format.fprintf ppf " Warning: %a" pp_deprecated x
  | `Removed _ -> ()

let generated_flag_doc ~allow_inline ~doc ~kind ~default ~status =
  let default = if default then "set" else "unset" in
  Format.asprintf "%s The flag is $(b,%s) by default.%s%a" doc default
    (in_attributes allow_inline kind)
    status_doc status

let generated_doc conv ~allow_inline ~doc ~kind ~default ~status =
  let default_doc = Format.asprintf "%a" (Arg.conv_printer conv) default in
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

let longest =
  let compare x y = compare (String.length x) (String.length y) in
  List.max_elt ~compare

(* somehow necessary *)
let map_status : [`Valid | `Deprecated of deprecated] -> status = function
  | `Valid -> `Valid
  | `Deprecated x -> `Deprecated x

let flag ~names ~default ~doc ~kind
    ?(allow_inline = Poly.(kind = Formatting)) ?(status = `Valid) update
    get_value =
  let open Cmdliner in
  let invert_names =
    List.filter_map names ~f:(fun n ->
        if String.length n = 1 then None else Some ("no-" ^ n) )
  in
  let default = default |> get_value |> Conf_t.Elt.v in
  let doc = generated_flag_doc ~allow_inline ~doc ~kind ~default ~status in
  let invert_doc = "Unset $(b," ^ List.last_exn names ^ ")." in
  let docs = section_name kind status in
  let term =
    Arg.(
      value
      & vflag None
          [ (Some true, info names ~doc ~docs)
          ; (Some false, info invert_names ~doc:invert_doc ~docs) ] )
  in
  let parse = Arg.conv_parser Arg.bool in
  let to_string = Bool.to_string in
  let opt =
    { names
    ; values= Bool
    ; doc
    ; parse
    ; update
    ; term
    ; allow_inline
    ; default
    ; to_string
    ; get_value
    ; status= map_status status }
  in
  opt

let any converter ~values ~docv ~names ~default ~doc ~kind
    ?(allow_inline = Poly.(kind = Formatting)) ?(status = `Valid) update
    get_value =
  let open Cmdliner in
  let default = default |> get_value |> Conf_t.Elt.v in
  let doc =
    generated_doc converter ~allow_inline ~doc ~kind ~default ~status
  in
  let docs = section_name kind status in
  let term =
    Arg.(value & opt (some converter) None & info names ~doc ~docs ~docv)
  in
  let parse = Arg.conv_parser converter in
  let to_string = Format.asprintf "%a%!" (Arg.conv_printer converter) in
  let opt =
    { names
    ; values
    ; doc
    ; parse
    ; update
    ; term
    ; allow_inline
    ; default
    ; to_string
    ; get_value (* ; from *)
    ; status= map_status status }
  in
  opt

let int = any ~values:Int Arg.int

let ocaml_version : _ declarator =
  any ~values:Ocaml_version ocaml_version_conv ~docv:"V"

let warn_deprecated (config : Conf_t.t) loc fmt =
  Format.kasprintf
    (fun s ->
      if not Conf_t.(config.opr_opts.quiet.v) then
        Location.deprecated loc ~use:loc ?def:None s )
    fmt

module Value = struct
  type 'a t = string * 'a * string * [`Valid | `Deprecated of deprecated]

  let make ?deprecated ~name value doc =
    match deprecated with
    | None -> (name, value, doc, `Valid)
    | Some x -> (name, value, doc, `Deprecated x)

  let pp_deprecated s ppf {dmsg= msg; dversion= v} =
    Format.fprintf ppf "Value `%s` is deprecated since version %a. %s" s
      Version.pp v msg

  let pp_deprecated_with_name ~opt ~val_ ppf {dmsg= msg; dversion= v} =
    Format.fprintf ppf
      "option `%s`: value `%s` is deprecated since version %a. %s" opt val_
      Version.pp v msg

  let status_doc s ppf = function
    | `Valid -> ()
    | `Deprecated x -> Format.fprintf ppf " Warning: %a" (pp_deprecated s) x

  let warn_if_deprecated conf from opt (s, _, _, status) =
    match status with
    | `Valid -> ()
    | `Deprecated d ->
        warn_deprecated conf (loc_from from) "%a"
          (pp_deprecated_with_name ~opt ~val_:s)
          d
end

module Value_removed = struct
  type t = {name: string; version: Version.t; msg: string}

  let make ~name ~since ~msg = {name; version= since; msg}

  let make_list ~names ~since ~msg =
    List.map names ~f:(fun name -> make ~name ~since ~msg)

  let add_parse_errors values conv =
    let parse s =
      match List.find values ~f:(fun {name; _} -> String.equal name s) with
      | Some {name; version; msg} ->
          Format.kasprintf
            (fun s -> Error (`Msg s))
            "value `%s` has been removed in version %a.%s" name Version.pp
            version (maybe_empty msg)
      | None -> Arg.conv_parser conv s
    in
    Arg.conv (parse, Arg.conv_printer conv)
end

let choice ~all ?(removed_values = []) ~names ~default ~doc ~kind
    ?(allow_inline = Poly.(kind = Formatting)) ?status update get_value =
  let default_v = default |> get_value |> Conf_t.Elt.v in
  let _, default', _, _ = List.hd_exn all in
  assert (Stdlib.(default_v = default')) ;
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
         (fun fs (s, _, d, st) -> fprintf fs "%s%a" d (Value.status_doc s) st) )
      all
  in
  let values = List.map all ~f:(fun (v, _, _, _) -> v) in
  let docv =
    let open Format in
    asprintf "@[<1>{%a}@]"
      (pp_print_list
         ~pp_sep:(fun fs () -> fprintf fs "@,|")
         pp_print_string )
      values
  in
  let update conf elt =
    let x = Conf_t.Elt.v elt in
    let from = Conf_t.Elt.from elt in
    ( match List.find all ~f:(fun (_, v, _, _) -> Poly.(x = v)) with
    | Some value -> Value.warn_if_deprecated conf from name value
    | None -> () ) ;
    update conf elt
  in
  any conv ~default ~docv ~names ~doc ~kind ~allow_inline ?status update
    ~values:(Choice values) get_value

let removed_option ~names ~since ~msg =
  let removed = {rversion= since; rmsg= msg} in
  let status = `Removed removed in
  let doc = Format.asprintf "%a" pp_removed removed in
  let parse _ = Error (`Msg doc) in
  let converter = Arg.conv (parse, fun _ () -> ()) in
  let update conf _ = conf and get_value _ = Conf_t.Elt.make () `Default in
  let kind = (* not used *) Formatting in
  let docs = section_name kind status in
  let term =
    Arg.(value & opt (some converter) None & info names ~doc ~docs)
  in
  let to_string _ = "" in
  let opt =
    { names
    ; values= Choice []
    ; doc
    ; parse
    ; update
    ; term
    ; allow_inline= true
    ; default= ()
    ; to_string
    ; get_value
    ; status }
  in
  opt

let update store ~config ~from:new_from ~name ~value ~inline =
  List.find_map store
    ~f:(fun
        (Pack {names; parse; update; allow_inline; get_value; to_string; _})
      ->
      if List.exists names ~f:(String.equal name) then
        if inline && not allow_inline then
          Some (Error (Error.Misplaced (name, value)))
        else
          match parse value with
          | Ok packed_value ->
              let elt = get_value config in
              let redundant =
                let old_value =
                  to_string @@ Conf_t.Elt.v @@ get_value config
                in
                let new_value = to_string @@ packed_value in
                String.equal old_value new_value
              in
              let new_elt =
                update_elt ~redundant elt packed_value new_from
              in
              let new_config = update config new_elt in
              (* update_from new_config name new_from redundant ; *)
              Some (Ok new_config)
          | Error (`Msg error) -> Some (Error (Error.Bad_value (name, error)))
      else
        match
          List.find names ~f:(fun x -> String.equal ("no-" ^ x) name)
        with
        | Some valid_name ->
            let error =
              Format.sprintf
                "%S is the short form for \"%s=false\". It is only accepted \
                 on command line, please use \"%s=false\" or \"%s=true\" \
                 instead."
                name valid_name valid_name valid_name
            in
            Some (Error (Error.Unknown (name, Some (`Msg error))))
        | None -> None )
  |> Option.value ~default:(Error (Error.Unknown (name, None)))

let default {default; _} = default

let print_config store c =
  let on_pack (Pack {names; to_string; get_value; status; _}) =
    let name = Option.value_exn (longest names) in
    let value = c |> get_value |> Conf_t.Elt.v |> to_string in
    let from = c |> get_value |> Conf_t.Elt.from in
    match status with
    | `Valid | `Deprecated _ ->
        Format.eprintf "%s=%s%a\n%!" name value pp_from from
    | `Removed _ -> ()
  in
  List.iter store ~f:on_pack

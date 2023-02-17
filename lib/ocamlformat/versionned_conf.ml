type mversion = (module Ocamlformat_version.S)

type t = {version: Version.t; version_module: mversion; conf: Conf.t}

let latest : mversion = (module Ocamlformat_lib_latest)

let oldest = latest

let m_of_version version =
  Version.(
    if version = current then Some latest
    else (
      assert (not (available version)) ;
      None ) )

let mversion_of_string v =
  let open Option.O in
  let* v = Version.of_string v in
  m_of_version v

let default =
  {version= Version.current; version_module= latest; conf= Conf.default}

let parse_line (config : t)
    ?(version_check = config.conf.Conf.opr_opts.version_check.v)
    ?(disable_conf_attrs = config.conf.opr_opts.disable_conf_attrs.v) ~from s
    =
  let update ~config ~from ~name ~value =
    match (name, from) with
    | "version", `File _ | "use-version", _ -> (
      match mversion_of_string value with
      | Some version_module -> Ok {config with version_module}
      | None ->
          if not version_check then Ok config
          else
            Error
              (Conf.Error.Version_mismatch
                 {read= value; installed= Version.(current |> to_string)} ) )
    | name, from ->
        let open Ocamlformat_result.Let_syntax in
        let* conf =
          Conf.update_of_string ~config:config.conf ~disable_conf_attrs ~from
            ~name ~value
        in
        {config with conf}
  in
  let s =
    match String.index s '#' with
    | Some i -> String.sub s ~pos:0 ~len:i
    | None -> s
  in
  let s = String.strip s in
  match String.split ~on:'=' s with
  | [] | [""] -> Ok config
  | [name; value] ->
      let name = String.strip name in
      let value = String.strip value in
      update ~config ~from ~name ~value
  | [s] -> (
    match String.strip s with
    | "" -> impossible "previous match"
    (* special case for disable/enable *)
    | "enable" -> update ~config ~from ~name:"disable" ~value:"false"
    | name -> update ~config ~from ~name ~value:"true" )
  | _ -> Error (Conf.Error.Malformed s)

let kind = Conf.Decl.Operational

let docs = Conf.Decl.section_name kind `Valid

let version_term =
  let open Cmdliner in
  let docv = "VERSION" in
  let doc = "Version of OCamlformat to be used" in
  let default = (Version.current, latest) in
  let set (version, version_module) conf =
    {conf with version; version_module}
  in
  let arg =
    Arg.conv ~docv
      ( (fun v ->
          match Version.of_string v with
          | None ->
              Error
                (`Msg
                  (Format.asprintf
                     "%S is not a valid version number. Version numbers \
                      should be of the shape MAJOR.MINOR[.PATCH]."
                     v ) )
          | Some v -> (
            match m_of_version v with
            | None ->
                let conf_error =
                  Conf.Error.Version_mismatch
                    { read= Version.to_string v
                    ; installed= Version.(current |> to_string) }
                in
                Error (`Msg (Conf.Error.to_string conf_error))
            | Some r -> Ok (v, r) ) )
      , fun fmt (v, _module) -> Version.pp fmt v )
  in
  let term =
    Arg.(value & opt arg default & info ["use-version"] ~doc ~docs ~docv)
  in
  Term.(const set $ term)

let terms =
  let open Cmdliner in
  [ Term.(
      const (fun conf_modif vconf ->
          {vconf with conf= conf_modif vconf.conf} )
      $ Conf.term )
  ; version_term ]

let term =
  let open Cmdliner in
  let compose (t1 : ('a -> 'b) Term.t) (t2 : ('b -> 'c) Term.t) :
      ('a -> 'c) Term.t =
    let open Term in
    const (fun f1 f2 a -> f2 (f1 a)) $ t1 $ t2
  in
  let term =
    List.fold_left ~init:(Term.const (fun x -> x)) ~f:compose terms
  in
  term

let update ?(quiet = false) c
    Parsetree.{attr_name= {txt; loc}; attr_payload; _} =
  let result =
    match txt with
    | "ocamlformat" -> (
      match attr_payload with
      | PStr
          [ { pstr_desc=
                Pstr_eval
                  ( { pexp_desc=
                        Pexp_constant
                          {pconst_desc= Pconst_string (str, strloc, None); _}
                    ; pexp_attributes= []
                    ; _ }
                  , [] )
            ; _ } ] ->
          parse_line ~from:(`Attribute strloc) c str
          |> Result.map_error ~f:Conf.Error.to_string
      | _ -> Error "Invalid format: String expected" )
    | _ when String.is_prefix ~prefix:"ocamlformat." txt ->
        Error
          (Format.sprintf "Invalid format: Unknown suffix %S"
             (String.chop_prefix_exn ~prefix:"ocamlformat." txt) )
    | _ -> Ok c
  in
  match result with
  | Ok conf -> conf
  | Error error ->
      let w = Warnings.Attribute_payload (txt, error) in
      if Conf.(not c.conf.opr_opts.quiet.v) && not quiet then
        Warning.print_warning loc w ;
      c

let m_of_version v =
  match m_of_version v with
  | None ->
      let conf_error =
        Conf.Error.Version_mismatch
          { read= Version.to_string v
          ; installed= Version.(current |> to_string) }
      in
      Error
        (Translation_unit_error.User_error (Conf.Error.to_string conf_error))
  | Some r -> Ok r


let map_conf ~f vconf = 
  {vconf with conf = f vconf.conf}
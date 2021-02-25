open! Dune_engine
open! Stdune
open Import
module SC = Super_context

module Processed = struct
  (* The actual content of the merlin file as built by the [Unprocessed.process]
     function from the unprocessed info gathered through [gen_rules]. The first
     three fields map directly to Merlin's B, S and FLG directives and the last
     one represents a list of preprocessors described by a preprocessing flag
     and its arguments. *)

  type pp_flag =
    { flag : string
    ; args : string
    }

  (* Most of the configuration is shared accros a same lib/exe... *)
  type config =
    { obj_dirs : Path.Set.t
    ; src_dirs : Path.Set.t
    ; flags : string list
    ; extensions : string Ml_kind.Dict.t list
    }

  (* ...but modules can have different preprocessing specifications*)
  type t =
    { config : config
    ; modules : Module_name.t list
    ; pp_config : pp_flag option Module_name.Per_item.t
    }

  module D = struct
    type nonrec t = t

    let name = "merlin-conf"

    let version = 1
  end

  module Persist = Dune_util.Persistent.Make (D)

  let load_file = Persist.load

  let to_sexp ~pp { obj_dirs; src_dirs; flags; extensions } =
    let serialize_path = Path.to_absolute_filename in
    let to_atom s = Sexp.Atom s in
    let make_directive tag value = Sexp.List [ Atom tag; value ] in
    let make_directive_of_path tag path =
      make_directive tag (Sexp.Atom (serialize_path path))
    in
    let exclude_query_dir = [ Sexp.List [ Atom "EXCLUDE_QUERY_DIR" ] ] in
    let obj_dirs =
      Path.Set.to_list obj_dirs |> List.map ~f:(make_directive_of_path "B")
    in
    let src_dirs =
      Path.Set.to_list src_dirs |> List.map ~f:(make_directive_of_path "S")
    in
    let flags =
      let flags =
        match flags with
        | [] -> []
        | flags ->
          [ make_directive "FLG" (Sexp.List (List.map ~f:to_atom flags)) ]
      in
      match pp with
      | None -> flags
      | Some { flag; args } ->
        make_directive "FLG" (Sexp.List [ Atom flag; Atom args ]) :: flags
    in
    let suffixes =
      List.map extensions ~f:(fun { Ml_kind.Dict.impl; intf } ->
          make_directive "SUFFIX" (Sexp.Atom (Printf.sprintf "%s %s" impl intf)))
    in
    Sexp.List
      (List.concat [ exclude_query_dir; obj_dirs; src_dirs; flags; suffixes ])

  let get { modules; pp_config; config } ~filename =
    let fname = Filename.remove_extension filename |> String.lowercase in
    List.find_opt modules ~f:(fun name ->
        let fname' = Module_name.to_string name |> String.lowercase in
        String.equal fname fname')
    |> Option.map ~f:(fun name ->
           let pp = Module_name.Per_item.get pp_config name in
           to_sexp ~pp config)

  let print_file path =
    match load_file path with
    | None -> Printf.eprintf "No merlin config found"
    | Some { modules; pp_config; config } ->
      let pp_one module_ =
        let pp = Module_name.Per_item.get pp_config module_ in
        let sexp = to_sexp ~pp config in
        let open Pp.O in
        Pp.vbox (Pp.text (Module_name.to_string module_))
        ++ Pp.newline
        ++ Pp.vbox (Sexp.pp sexp)
        ++ Pp.newline
      in
      Format.printf "%a%!" Pp.to_fmt (Pp.concat_map modules ~f:pp_one)
end

module Unprocessed = struct
  (* We store separate information for each "module". These informations do not
     reflect the actual content of the Merlin configuration yet but are needed
     for it's elaboration via the function [process : Unprocessed.t ... ->
     Processed.t] *)
  type config =
    { requires : Lib.Set.t
    ; flags : string list Build.t
    ; preprocess :
        Preprocess.Without_instrumentation.t Preprocess.t Module_name.Per_item.t
    ; libname : Lib_name.Local.t option
    ; source_dirs : Path.Source.Set.t
    ; objs_dirs : Path.Set.t
    ; extensions : string Ml_kind.Dict.t list
    }

  type t =
    { ident : Merlin_ident.t
    ; config : config
    ; modules : Modules.t
    }

  let make ?(requires = Ok []) ~flags
      ?(preprocess = Preprocess.Per_module.no_preprocessing ()) ?libname
      ?(source_dirs = Path.Source.Set.empty) ~modules ~obj_dir ~dialects ~ident
      () =
    (* Merlin shouldn't cause the build to fail, so we just ignore errors *)
    let requires =
      match requires with
      | Ok l -> Lib.Set.of_list l
      | Error _ -> Lib.Set.empty
    in
    let objs_dirs =
      Obj_dir.byte_dir obj_dir |> Path.build |> Path.Set.singleton
    in
    let flags =
      match Modules.alias_module modules with
      | None -> Ocaml_flags.common flags
      | Some m ->
        Ocaml_flags.prepend_common
          [ "-open"; Module_name.to_string (Module.name m) ]
          flags
        |> Ocaml_flags.common
    in
    let extensions = Dialect.DB.extensions_for_merlin dialects in
    let config =
      { requires
      ; flags = Build.catch flags ~on_error:[]
      ; preprocess
      ; libname
      ; source_dirs
      ; objs_dirs
      ; extensions
      }
    in
    { ident; config; modules }

  let quote_if_needed s =
    if String.need_quoting s then
      Filename.quote s
    else
      s

  let pp_flag_of_action ~expander ~loc ~action :
      Processed.pp_flag option Build.With_targets.t =
    match (action : Action_dune_lang.t) with
    | Run (exe, args) -> (
      let args =
        let open Option.O in
        let* args, input_file = List.destruct_last args in
        if String_with_vars.is_var input_file ~name:"input-file" then
          Some args
        else
          None
      in
      match args with
      | None -> Build.With_targets.return None
      | Some args ->
        let action =
          let targets_dir = Expander.dir expander in
          let targets : Targets.Or_forbidden.t =
            Forbidden "preprocessing actions"
          in
          let action = Preprocessing.chdir (Run (exe, args)) in
          Action_unexpanded.expand ~loc ~expander ~dep_kind:Optional ~targets
            ~targets_dir action
            (Build.return Bindings.empty)
        in
        let pp_of_action exe args =
          match exe with
          | Error _ -> None
          | Ok exe ->
            let args =
              Path.to_absolute_filename exe :: args
              |> List.map ~f:quote_if_needed
              |> String.concat ~sep:" "
            in
            Some Processed.{ flag = "-pp"; args }
        in
        Build.With_targets.map action ~f:(function
          | Run (exe, args) -> pp_of_action exe args
          | Chdir (_, Run (exe, args)) -> pp_of_action exe args
          | Chdir (_, Chdir (_, Run (exe, args))) -> pp_of_action exe args
          | _ -> None) )
    | _ -> Build.With_targets.return None

  let pp_flags sctx ~expander libname preprocess :
      Processed.pp_flag option Build.With_targets.t =
    let scope = Expander.scope expander in
    match
      Preprocess.remove_future_syntax preprocess ~for_:Merlin
        (Super_context.context sctx).version
    with
    | Action (loc, (action : Action_dune_lang.t)) ->
      pp_flag_of_action ~expander ~loc ~action
    | No_preprocessing -> Build.With_targets.return None
    | Pps { loc; pps; flags; staged = _ } -> (
      match
        Preprocessing.get_ppx_driver sctx ~loc ~expander ~lib_name:libname
          ~flags ~scope pps
      with
      | Error _exn -> Build.With_targets.return None
      | Ok (exe, flags) ->
        let args =
          Path.to_absolute_filename (Path.build exe) :: "--as-ppx" :: flags
          |> List.map ~f:quote_if_needed
          |> String.concat ~sep:" "
        in
        Build.With_targets.return (Some Processed.{ flag = "-ppx"; args }) )

  let process
      { modules
      ; ident = _
      ; config =
          { extensions
          ; flags
          ; objs_dirs
          ; source_dirs
          ; requires
          ; preprocess
          ; libname
          }
      } sctx ~more_src_dirs ~expander =
    let open Build.With_targets.O in
    let+ config =
      let+ flags = Build.with_no_targets flags in
      let src_dirs, obj_dirs =
        Lib.Set.fold requires
          ~init:(Path.set_of_source_paths source_dirs, objs_dirs)
          ~f:(fun (lib : Lib.t) (src_dirs, obj_dirs) ->
            let more_src_dirs = Lib.src_dirs lib in
            ( Path.Set.union src_dirs more_src_dirs
            , let public_cmi_dir = Obj_dir.public_cmi_dir (Lib.obj_dir lib) in
              Path.Set.add obj_dirs public_cmi_dir ))
      in
      let src_dirs =
        Path.Set.union src_dirs
          (Path.Set.of_list_map ~f:Path.source more_src_dirs)
      in
      { Processed.src_dirs; obj_dirs; flags; extensions }
    and+ pp_config =
      Module_name.Per_item.map_with_targets preprocess
        ~f:(pp_flags sctx ~expander libname)
    in
    let modules =
      (* And copy for each module the resulting pp flags *)
      Modules.fold_no_vlib modules ~init:[] ~f:(fun m acc ->
          Module.name m :: acc)
    in
    { Processed.modules; pp_config; config }
end

let dot_merlin sctx ~dir ~more_src_dirs ~expander (t : Unprocessed.t) =
  let open Build.With_targets.O in
  let merlin_exist = Merlin_ident.merlin_exists_path dir t.ident in
  let merlin_file = Merlin_ident.merlin_file_path dir t.ident in

  (* We make the compilation of .ml/.mli files depend on the existence of
     .merlin so that they are always generated, however the command themselves
     don't read the merlin file, so we don't want to declare a dependency on the
     contents of the .merlin file.

     Currently dune doesn't support declaring a dependency only on the existence
     of a file, so we have to use this trick. *)
  SC.add_rule sctx ~dir
    ( Build.with_no_targets (Build.path (Path.build merlin_file))
    >>> Build.create_file merlin_exist );

  Path.Set.singleton (Path.build merlin_file)
  |> Rules.Produce.Alias.add_deps (Alias.check ~dir);

  let merlin = Unprocessed.process t sctx ~more_src_dirs ~expander in
  let action =
    Build.With_targets.write_file_dyn merlin_file
      (Build.With_targets.map ~f:Processed.Persist.to_string merlin)
  in
  SC.add_rule sctx ~dir action

let add_rules sctx ~dir ~more_src_dirs ~expander merlin =
  if (SC.context sctx).merlin then
    dot_merlin sctx ~more_src_dirs ~expander ~dir merlin

include Unprocessed

type t

val workspace_file : t -> Arg.Path.t option

val x : t -> Dune_engine.Context_name.t option

val profile : t -> Dune_rules.Profile.t option

val capture_outputs : t -> bool

val root : t -> Workspace_root.t

val config : t -> Dune_engine.Config.t

module Only_packages : sig
  type t = private
    { names : Dune_engine.Package.Name.Set.t
    ; command_line_option : string
          (** Which of [-p], [--only-packages], ... was passed *)
    }
end

val only_packages : t -> Only_packages.t option

val watch : t -> bool

val default_target : t -> Arg.Dep.t

val prefix_target : t -> string -> string

val instrument_with : t -> Dune_engine.Lib_name.t list option

(** [set_common ?log common ~targets ~external_lib_deps_mode] is
    [set_dirs common] followed by [set_common_other common ~targets]. In
    general, [set_common] executes sequence of side-effecting actions to
    initialize Dune's working environment based on the options determined in a
    [Common.t] record.contents. *)
val set_common :
     ?log_file:Dune_util.Log.File.t
  -> ?external_lib_deps_mode:bool
  -> t
  -> targets:Arg.Dep.t list
  -> unit

(** [set_common_other common ~targets] sets all stateful values dictated by
    [common], except those accounted for by [set_dirs]. [targets] are used to
    obtain external library dependency hints, if needed. *)
val set_common_other :
  ?log_file:Dune_util.Log.File.t -> t -> targets:Arg.Dep.t list -> unit

(** [set_dirs common] sets the workspace root and build directories, and makes
    the root the current working directory *)
val set_dirs : t -> unit

val examples : (string * string) list -> Cmdliner.Manpage.block

val help_secs : Cmdliner.Manpage.block list

val footer : Cmdliner.Manpage.block

val term : t Cmdliner.Term.t

val debug_backtraces : bool Cmdliner.Term.t

val config_term : Dune_engine.Config.t Cmdliner.Term.t

val display_term : Dune_engine.Config.Display.t option Cmdliner.Term.t

val context_arg : doc:string -> Dune_engine.Context_name.t Cmdliner.Term.t

(** A [--build-info] command line argument that print build information
    (included in [term]) *)
val build_info : unit Cmdliner.Term.t

val default_build_dir : string

module Let_syntax : sig
  val ( let+ ) : 'a Cmdliner.Term.t -> ('a -> 'b) -> 'b Cmdliner.Term.t

  val ( and+ ) :
    'a Cmdliner.Term.t -> 'b Cmdliner.Term.t -> ('a * 'b) Cmdliner.Term.t
end

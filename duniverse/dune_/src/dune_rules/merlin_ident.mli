open! Stdune

(** Merlin identifiers allow the unique identification of a merlin file attached
    to a specific [library] or [executable] stanza. *)
type t

val for_lib : Dune_engine.Lib_name.t -> t

val for_exes : names:string list -> t

(** Merlin config folder name *)
val merlin_folder_name : string

(** Return the path of the merlin_exist file for a given stanza *)
val merlin_exists_path : Path.Build.t -> t -> Path.Build.t

(** Return the path of the merlin file for a given stanza *)
val merlin_file_path : Path.Build.t -> t -> Path.Build.t

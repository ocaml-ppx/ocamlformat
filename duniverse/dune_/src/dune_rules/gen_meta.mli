(** Generate a META file *)
open! Dune_engine

open! Import

val requires : ?preds:Meta.predicate list -> Lib_name.Set.t -> Meta.entry

(** Generate the meta for a package containing some libraries *)
val gen :
     package:Package.t
  -> add_directory_entry:bool
  -> Super_context.Lib_entry.t list
  -> Meta.t

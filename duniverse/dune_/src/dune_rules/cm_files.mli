(** This module encapsulates the trick of speeding up builds by providing an
    unsorted list of module dependencies statically and only using the topsorted
    list of deps for the order when passing to ocamlopt *)
open! Dune_engine

open Stdune

type t

val make :
     obj_dir:Path.Build.t Obj_dir.t
  -> modules:Modules.t
  -> top_sorted_modules:Module.t list Build.t
  -> ext_obj:string
  -> t

val unsorted_objects_and_cms : t -> mode:Mode.t -> Path.t list

val top_sorted_cms : t -> mode:Mode.t -> Path.t list Build.t

val top_sorted_objects_and_cms : t -> mode:Mode.t -> Path.t list Build.t

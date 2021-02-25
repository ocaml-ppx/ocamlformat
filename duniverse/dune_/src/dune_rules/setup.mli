(** Setup of dune *)
open! Dune_engine

(** These parameters are set by [ocaml configure.ml] or copied from
    [setup.defaults.ml]. *)

(** Where to find installed libraries for the default context. If [None],
    auto-detect it using standard tools such as [ocamlfind]. *)
val library_path : string list option

(** Where to install libraries for the default context. *)
val library_destdir : string option

(** Where to install manpages for the default context. *)
val mandir : string option

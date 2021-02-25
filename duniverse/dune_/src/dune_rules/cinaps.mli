(** Cinaps integration *)
open! Dune_engine

(** Cinaps is a small tool that allows to auto-generate part of ml or other
    source files and keep them in sync:

    https://github.com/janestreet/cinaps

    This module implements the [cinaps] stanza that integrate cinaps nicely with
    dune. *)

open Stdune

type t

type Stanza.t += T of t

(** Generate the rules to handle this cinaps stanza *)
val gen_rules :
  Super_context.t -> t -> dir:Path.Build.t -> scope:Scope.t -> unit

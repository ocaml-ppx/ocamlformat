(* For the purpose of avoiding dependency cycles between the rules and the
   engine.

   This name could probably be chosen to be a bit more informative. *)

open! Stdune
open! Import

type t = private
  { name : Context_name.t
  ; build_dir : Path.Build.t
  ; env : Env.t
  ; host : t option
  ; stdlib_dir : Path.t
  ; default_ocamlpath : Path.t list
  }

val create :
     name:Context_name.t
  -> build_dir:Path.Build.t
  -> env:Env.t
  -> host:t option
  -> stdlib_dir:Path.t
  -> default_ocamlpath:Path.t list
  -> t

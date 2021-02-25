(** This module contains a small database of flags that is used when compiling C
    and C++ stubs. *)
open! Stdune

open Dune_engine

(** The name of the file created in the .dune folder after calling the C
    preprocessor *)
val preprocessed_filename : string

(** [get_flags c_compiler] returns the necessary flags to turn this compiler
    into a c++ compiler for some of the most common compilers *)
val get_flags : Path.Build.t -> string list Build.t

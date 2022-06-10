(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

val project_root_witness : string list

type configuration_file = Ocamlformat of Fpath.t | Ocp_indent of Fpath.t

val root_ocamlformat_file : root:Fpath.t option -> Fpath.t

type t =
  { ignore_files: Fpath.t list
  ; enable_files: Fpath.t list
  ; configuration_files: configuration_file list
  ; project_root: Fpath.t option }

val make :
     enable_outside_detected_project:bool
  -> disable_conf_files:bool
  -> ocp_indent_config:bool
  -> root:Fpath.t option
  -> file:Fpath.t (** Absolute path of the file to format. *)
  -> t

val has_ocamlformat_file : t -> bool

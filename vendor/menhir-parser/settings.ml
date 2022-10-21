(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let trace =
  ref false

let noprefix =
  ref false

type infer_mode =
    (* Perform no type inference. This is the default mode. *)
  | IMNone
    (* Perform type inference by invoking ocamlc directly. *)
  | IMInfer                (* --infer *)
  | IMDependRaw            (* --raw-depend *)
  | IMDependPostprocess    (* --depend *)
    (* Perform type inference by writing a mock .ml file and
       reading the corresponding inferred .mli file. *)
  | IMWriteQuery of string (* --infer-write-query <filename> *)
  | IMReadReply of string  (* --infer-read-reply <filename> *)

let infer =
  ref IMNone

let logG, logA, logC =
  ref 0, ref 0, ref 0

let strict =
  ref false

type dollars =
  | DollarsDisallowed
  | DollarsAllowed

let dollars =
  ref DollarsAllowed

(* ------------------------------------------------------------------------- *)

(* Decide which back-end is used. *)

let extension = ".mly"

(* ------------------------------------------------------------------------- *)
(* Export the settings. *)

let stdlib_filename =
  "<standard.mly>"

let trace =
  !trace

let noprefix =
  !noprefix

let logG, logA, logC =
  !logG, !logA, !logC

let strict =
  !strict

let dollars =
  !dollars

let infer =
  !infer

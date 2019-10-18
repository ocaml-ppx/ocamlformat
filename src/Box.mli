(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

(** {1 Type and evaluator} *)

type t
(** A box is something that runs before and after a [Fmt.t]. This ensures
    that the corresponding "open" and "close" part are properly balanced by
    always maintaining them together. *)

val wrap : t -> Fmt.t -> Fmt.t
(** Evaluate a box: wrap the output between the box's open and close parts. *)

(** {1 Safe constructors and combinators} *)

val noop : t
(** Do nothing. *)

val vbox : int -> t
(** Build a box using [Fmt.vbox]. *)

val hvbox : int -> t
(** Build a box using [Fmt.hvbox]. *)

val hovbox : int -> t
(** Build a box using [Fmt.hovbox]. *)

val compose : inside:t -> outside:t -> t
(** Wrap a box within a second one: the open parts will run in order, and the
    close parts will run in reverse. *)

(** {1 Unsafe API} *)

val unsafe_opn : t -> Fmt.t
(** Just run the open part. *)

val unsafe_cls : t -> Fmt.t
(** Just run the close part. *)

val unsafe_prepend_to_cls : Fmt.t -> t -> t
(** Prepend a formatter only to the close part. *)

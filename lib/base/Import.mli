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

(** Opened in each source module to establish global namespace *)

include module type of (
  Base :
    sig
      [@@@warning "-3"]

      include
        module type of Base
          (* [Filename], [Format], [Scanf] are all deprecated in [Base],
             erase them and use the ones from the stdlib. *)
        with module Filename := Base.Filename
         and module Format := Base.Format
         and module Scanf := Base.Scanf
    end )

include module type of Option.Monad_infix

include module type of Stdio

external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"
(** Function application: [g @@ f @@ x] is exactly equivalent to
    [g (f (x))]. Right associative. *)

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Composition of functions: [(f >> g) x] is exactly equivalent to
    [g (f (x))]. Left associative. *)

val ( $ ) : ('a -> unit) -> ('a -> 'b) -> 'a -> 'b
(** Sequential composition of functions: [(f $ g) x] is exactly equivalent
    to [(f x) ; (g x)]. Left associative. *)

val impossible : string -> _
(** Indicate why the call is expected to be impossible. *)

val user_error : string -> (string * Sexp.t) list -> _
(** Raise a user error (e.g. due to malformed or illegal input). *)

val check : ('a -> _) -> 'a -> 'a
(** Asserting identity: [check f x] asserts that [f x] does not raise and
    returns [x]. *)

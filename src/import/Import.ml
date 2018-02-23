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

(** Import into each source to establish global namespace: [open! Import] *)

include (
  Base :
    module type of Base
    with module Filename := Caml.Filename
     and module Format = Caml.Format
     and module Scanf = Caml.Scanf )

include Option.Monad_infix
include Stdio

let ( >> ) f g x = g (f x)

let ( $ ) f g x = f x ; g x

let impossible msg = failwith msg

let internal_error msg kvs =
  Error.raise_s (Sexp.message ("Internal Error: " ^ msg) kvs)


let user_error msg kvs =
  Error.raise_s (Sexp.message ("User Error: " ^ msg) kvs)


let check f x =
  assert (
    ignore (f x) ;
    true ) ;
  x

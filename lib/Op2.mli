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

type infix_kind =
  | Hash_get
      (** e.g. [#**#] or [#**.] where [**] can be 0 or more operator chars. *)
  | Monad  (** e.g. [*>>=], [+>>>], [/>>|], etc. *)
  | Other

type kind =
  | Monad_bind
      (** e.g. [let**] or [and**] where [**] can be 1 or more operator chars. *)
  | Infix of infix_kind

module String_id : sig
  val kind : string -> kind option

  val prec : child:Assoc.t -> string -> Prec.t * Assoc.t
end

module Exp : sig
  val kind : Extended_ast.expression -> kind option
end

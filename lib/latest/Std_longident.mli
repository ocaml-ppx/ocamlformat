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

module String_id : sig
  val is_prefix : string -> bool
  (** Holds for prefix symbols. *)

  val is_infix : string -> bool
  (** Holds for infix symbols. *)

  val is_symbol : string -> bool
  (** Holds for prefix or infix symbols. *)

  val is_hash_getter : string -> bool
  (** [is_hash_getter id] returns whether [id] is considered a hash-getter
      operator, of the form [#**#] or [#**.] where [**] can be 0 or more
      operator chars. *)

  val is_monadic_binding : string -> bool
  (** [is_monadic_binding id] returns whether [id] is a monadic binding
      operator of the form [let**] or [and**] where [**] can be 1 or more
      operator chars. *)
end

val is_infix : Longident.t -> bool
(** Holds for infix identifiers. *)

val is_prefix : Longident.t -> bool

val is_index_op : Longident.t -> bool

val is_symbol : Longident.t -> bool

val is_hash_getter : Longident.t -> bool
(** [is_hash_getter id] returns whether [id] is considered a hash-getter
    operator, of the form [#**#] or [#**.] where [**] can be 0 or more
    operator chars. *)

val is_monadic_binding : Longident.t -> bool
(** [is_monadic_binding id] returns whether [id] is a monadic binding
    operator of the form [let**] or [and**] where [**] can be 1 or more
    operator chars. *)

val field_alias_str : field:Longident.t -> string -> bool

val field_alias : field:Longident.t -> Longident.t -> bool

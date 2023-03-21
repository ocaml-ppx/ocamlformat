(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Auxiliary AST types used by parsetree and typedtree.

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

type rec_flag = Nonrecursive | Recursive

type direction_flag = Upto | Downto

(* Order matters, used in polymorphic comparison *)
type private_flag = Private of Location.t | Public

type mutable_flag = Immutable | Mutable of Location.t

type virtual_flag = Virtual of Location.t | Concrete

type private_virtual = {pv_priv: Location.t option; pv_virt: Location.t option}

type mutable_virtual = {mv_mut: Location.t option; mv_virt: Location.t option}

type override_flag = Override | Fresh

type closed_flag = Closed | Open

type obj_closed_flag =
  | OClosed
  | OOpen of Location.t

type label = string

type label_info = {
  name: string loc;
  extra_info: [ `Single_token | `Previous_token of Location.t ];
}

type arg_label =
    Nolabel
  | Labelled of label_info (*  label:T -> ... *)
  | Optional of label_info (* ?label:T -> ... *)

type 'a loc = 'a Location.loc = {
  txt : 'a;
  loc : Location.t;
}

type variant_var = string loc loc  (** [`A] *)

type variance_and_injectivity = string loc list

(* For Pexp_indexop_access *)
type paren_kind = Paren | Brace | Bracket

type and_or_with = And | With

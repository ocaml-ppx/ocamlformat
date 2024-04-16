(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Attribute
open Syntax

(* This is the abstract syntax for an unparameterized grammar, that is, a
   grammar that does not have any parameterized nonterminal symbols. Such a
   grammar is obtained as the result of an expansion phase, which takes
   place in two steps, implemented in [SelectiveExpansion] and [Drop]. *)

(* In an unparameterized grammar, %attribute declarations can be desugared
   away. This is also done during the above-mentioned expansion phase.

   Thus, in an unparameterized grammar, attributes can be attached in the
   following places:

   - with the grammar:          field [gr_attributes] of [grammar]
   - with a terminal symbol:    field [tk_attributes] of [token_properties]
   - with a nonterminal symbol: field [attributes] of [rule]
   - with a producer:           field [producer_attributes] of [producer]
   - with a branch:             field [br_attributes] of [branch]           *)

(* ------------------------------------------------------------------------ *)

(* A producer is a pair of a (located) identifier and a symbol. In concrete
   syntax, it could be [e = expr], for instance. It carries a number of
   attributes. *)

type producer =
  {
    producer_identifier : identifier located;
    producer_symbol     : symbol;
    producer_attributes : attributes;
  }

type producers =
  producer list

(* ------------------------------------------------------------------------ *)

(* A branch contains a series of producers and a semantic action. It is the
   same as in the surface syntax; see [Syntax]. *)

type branch =
  {
    branch_position  : Positions.t;
    producers        : producers;
    action           : action;
    prec_annotation  : prec_annotation;
    production_level : production_level;
    br_attributes    : attributes;
  }

type branches =
  branch list

(* ------------------------------------------------------------------------ *)

(* A rule consists mainly of several branches. In contrast with the surface
   syntax, it has no parameters. *)

(* The [%inline] flag is no longer relevant after [NonTerminalInlining]. *)

type rule =
  {
    branches    : branches;
    positions   : Positions.t list;
    inline_flag : bool;
    attributes  : attributes;
  }

(* ------------------------------------------------------------------------ *)

(* A grammar is essentially the same as in the surface syntax; see [Syntax].
   The main difference is that [%attribute] declarations, represented by
   the field [p_symbol_attributes] in the surface syntax, have disappeared. *)

type grammar =
  {
    preludes        : Stretch.t list;
    postludes       : Syntax.postlude list;
    parameters      : Stretch.t list;
    start_symbols   : StringSet.t;
    types           : Stretch.ocamltype StringMap.t;
    tokens          : Syntax.token_properties StringMap.t;
    on_error_reduce : on_error_reduce_level StringMap.t;
    gr_attributes   : attributes;
    rules           : rule StringMap.t;
  }

(* -------------------------------------------------------------------------- *)

(* Accessors for the type [producer]. *)

let producer_identifier { producer_identifier } : identifier =
  Positions.value producer_identifier

let producer_identifier_located { producer_identifier } : identifier located =
  producer_identifier

let producer_symbol { producer_symbol } =
  producer_symbol

let producer_attributes { producer_attributes } =
  producer_attributes

(* -------------------------------------------------------------------------- *)

(* A getter and a transformer for the field [branches] of the type [rule]. *)

let get_branches rule =
  rule.branches

let transform_branches f rule =
  { rule with branches = f rule.branches }

(* -------------------------------------------------------------------------- *)

(* Terminal symbols. *)

(* [tokens grammar] is a list of all (real) tokens in the grammar
   [grammar]. The special tokens "#" and "error" are not included.
   Pseudo-tokens (used in %prec declarations, but never declared
   using %token) are filtered out. *)

let tokens grammar =
  StringMap.fold (fun token properties tokens ->
    if properties.tk_is_declared then token :: tokens else tokens
  ) grammar.tokens []

(* [typed_tokens grammar] is analogous, but includes the OCaml type
   of each token. *)

let typed_tokens grammar =
  StringMap.fold (fun token properties tokens ->
    if properties.tk_is_declared then (token, properties.tk_ocamltype) :: tokens else tokens
  ) grammar.tokens []

(* [ocamltype_of_token grammar symbol] produces the OCaml type
   of the terminal symbol [symbol] in the grammar [grammar].
   This terminal symbol must exist and must not be a pseudo-token.
   [None] is returned if and only if this token does not carry a
   semantic value. *)

let ocamltype_of_token grammar symbol : Stretch.ocamltype option =
  try
    let properties = StringMap.find symbol grammar.tokens in
    assert properties.tk_is_declared;
    properties.tk_ocamltype
  with Not_found ->
    assert false

(* [alias grammar symbol] returns the token alias of the terminal symbol
   [symbol], if there is one. This terminal symbol must exist and must not
   be a pseudo-token. *)

let alias grammar symbol : alias =
  try
    let properties = StringMap.find symbol grammar.tokens in
    assert properties.tk_is_declared;
    properties.tk_alias
  with Not_found ->
    assert false

let unquoted_alias grammar symbol : alias =
  alias grammar symbol
  |> Option.map Misc.unquote

(* -------------------------------------------------------------------------- *)

(* Nonterminal symbols. *)

(* [nonterminals grammar] is a list of all nonterminal symbols in the
   grammar [grammar]. It does not include the artificial start symbols
   [S']. *)

let nonterminals grammar : nonterminal list =
  StringMap.fold (fun nt _ rules -> nt :: rules) grammar.rules []

(* [is_nonterminal grammar symbol] tests whether the symbol [symbol]
   is a nonterminal symbol. It is assumed that this symbol exists,
   that is, it is either a terminal symbol or a nonterminal symbol. *)

let is_nonterminal grammar symbol =
  StringMap.mem symbol grammar.rules

(* [ocamltype_of_symbol grammar symbol] produces the OCaml type
   of the nonterminal symbol [symbol] in the grammar [grammar],
   if it is known. *)

let ocamltype_of_symbol grammar symbol : Stretch.ocamltype option =
  try
    Some (StringMap.find symbol grammar.types)
  with Not_found ->
    None

(* [ocamltype_of_start_symbol grammar symbol] produces the OCaml type
   of the start symbol [symbol] in the grammar [grammar]. *)

let ocamltype_of_start_symbol grammar symbol : Stretch.ocamltype =
  try
    StringMap.find symbol grammar.types
  with Not_found ->
    (* Every start symbol should have a type. *)
    assert false

(* [is_inline_symbol grammar symbol] tells whether [symbol] is a nonterminal
   symbol (as opposed to a terminal symbol) and is marked %inline. *)

let is_inline_symbol grammar symbol : bool =
  match StringMap.find symbol grammar.rules with
  | rule ->
      (* This is a nonterminal symbol. Test its %inline flag. *)
      rule.inline_flag
  | exception Not_found ->
      (* This is a terminal symbol. *)
      false

(* [is_inline_symbol grammar producer] tells whether [producer] represents a
   nonterminal symbol (as opposed to a terminal) and is marked %inline. *)

let is_inline_producer grammar producer =
  is_inline_symbol grammar (producer_symbol producer)

(* -------------------------------------------------------------------------- *)

(* [names producers] is the set of names of the producers [producers]. The
   name of a producer is the OCaml variable that is used to name its semantic
   value. *)

(* This function checks on the fly that no two producers carry the same name.
   This check should never fail if we have performed appropriate renamings.
   It is a debugging aid. *)

let names (producers : producers) : StringSet.t =
  List.fold_left (fun ids producer ->
    let id = producer_identifier producer in
    assert (not (StringSet.mem id ids));
    StringSet.add id ids
  ) StringSet.empty producers

(* -------------------------------------------------------------------------- *)

(* Productions. *)

(* [print_production nt branch] prints the production whose left-hand side
   is the nonterminal symbol [nt] and whose right-hand side is [branch]. *)

let rec print_production nt branch =
  Printf.sprintf "%s ->%s" nt (print_production_rhs branch.producers)

and print_production_rhs producers =
  let b = Buffer.create 80 in
  List.iter (fun producer ->
    Printf.bprintf b " %s" producer.producer_symbol
  ) producers;
  Buffer.contents b

(* [error_free branch] determines whether the branch [branch] does *not*
   contain the [error] token. *)

let rec error_free branch =
  List.for_all error_free_producer branch.producers

and error_free_producer producer =
  producer.producer_symbol <> "error"

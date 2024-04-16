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
open IL

(* This module generates the code and the signature of the unparsing API. *)

module Code (G : sig
  module Terminal : sig
    type t
    val t2i: t -> int
    val print: t -> string
    val sharp: t
    val real: t -> bool
    val unquoted_alias: t -> string option
    val ocamltype: t -> Stretch.ocamltype option
    val map_real: (t -> 'a) -> 'a list
  end
  module Nonterminal : sig
    type t
    val print: bool -> t -> string
    val mapx: (t -> 'a) -> 'a list
  end
  module Symbol : sig
    type t =
      | N of Nonterminal.t
      | T of Terminal.t
  end
  module Production : sig
    type index
    val p2i: index -> int
    val rhs: index -> Symbol.t array
    val attributes: index -> attributes
    val positions: index -> Positions.t list
    val print: index -> string
    val mapnt:  Nonterminal.t -> (index -> 'a) -> 'a list
    val mapx: (index -> 'a) -> 'a list
    val error_free: index -> bool
  end
end)
(A : sig
  (* A list of the start symbols and corresponding start states. *)
  val entry: (G.Nonterminal.t * int) list
end)
(N : sig
  val tables: string
end)
: sig
  val unparsing_API : unit -> structure
end

module Interface (G : sig
  module Terminal : sig
    type t
    val print: t -> string
    val real: t -> bool
    val unquoted_alias: t -> string option
    val ocamltype: t -> Stretch.ocamltype option
    val map_real: (t -> 'a) -> 'a list
  end
  module Nonterminal : sig
    type t
    val print: bool -> t -> string
    val mapx: (t -> 'a) -> 'a list
  end
  module Symbol : sig
    type t =
      | N of Nonterminal.t
      | T of Terminal.t
  end
  module Production : sig
    type index
    val print: index -> string
    val rhs: index -> Symbol.t array
    val attributes: index -> attributes
    val error_free: index -> bool
    val mapnt:  Nonterminal.t -> (index -> 'a) -> 'a list
    val mapx: (index -> 'a) -> 'a list
  end
end)
(A : sig
  val entry: G.Nonterminal.t list
end)
: sig
  val unparsing_API : unit -> interface
end

val unparsing_API : BasicSyntax.grammar -> interface

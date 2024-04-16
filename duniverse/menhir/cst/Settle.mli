(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(**This module offers a type of concrete syntax trees (CSTs), a type of
   disjunctive concrete syntax trees (DCSTs), and a resolution algorithm,
   which converts a DCST to a viable CST.

   The disjunction nodes in DCSTs allow the user to express where parentheses
   or other disambiguation tokens {i may} be inserted. Within this space, the
   resolution algorithm chooses a specific point: that is, it decides where to
   insert parentheses so as to produce a viable CST. A CST [t] is viable if the
   parser's round-trip property holds: [grow (fringe t) = Some t]. *)

(**The functor [Make] must be applied to a description of the LR(1) automaton.
   Its input signature is a super-signature of the signature
   [MenhirLib.EngineTypes.TABLE]. *)
module Make (A : sig

  (**A type of terminal symbols. *)
  type terminal

  (**A type of nonterminal symbols. *)
  type nonterminal

  (**A type of productions. *)
  type production = int

  (**A type of LR(1) states. *)
  type state

  (**An injective mapping of states to integers. *)
  val number: state -> int

  (**A type of tokens. *)
  type token

  (**A token is conceptually a pair of a terminal symbol and a semantic value.
     [token2terminal] is the first pair projection: it maps a token to the
     corresponding terminal symbol. *)
  val token2terminal : token -> terminal

  (**[lhs prod] returns the left-hand side of the production [prod]. *)
  val lhs : production -> nonterminal

  (**[maybe_shift_t s t] determines whether there exists a transition out of
     the state [s], labeled with the terminal symbol [t], to some state
     [s']. If so, it returns [Some s']. Otherwise, it returns [None]. *)
  val maybe_shift_t : state -> terminal -> state option

  (**[maybe_goto_nt s nt] determines whether there exists a transition out of
     the state [s], labeled with the nonterminal symbol [nt], to some state
     [s']. If so, it returns [Some s']. Otherwise, it returns [None]. *)
  val maybe_goto_nt : state -> nonterminal -> state option

  (**[may_reduce_prod s t prod] determines whether in the state [s], with
     lookahead symbol [t], the automaton reduces production [prod]. This test
     accounts for the possible existence of a default reduction. *)
  val may_reduce_prod : state -> terminal -> production -> bool

end) : sig
  open A

  (**The module [CST] offers an algebraic data type [cst] of concrete syntax
     trees. This definition is generic, that is, grammar-independent. This
     module is unsafe: the data constructor [NonTerminal] has an invariant
     that is not enforced. A safe, non-generic API {i can} be constructed
     a posteriori on top of this unsafe, generic API.

     The {i fringe} of a CST is a sequence of tokens.

     A CST is {i viable} if the parser accepts its fringe and produces the
     exact same CST again. In general, not every CST is viable: as a typical
     example, in a grammar of arithmetic expressions where there is a single
     nonterminal symbol for expressions and where priority declarations are
     used to resolve shift/reduce conflicts, a CST where an addition node is a
     child of a multiplication node is not viable. If the grammar is LR(1) then
     every CST is viable. *)
  module CST : sig

    (**A concrete syntax tree (CST) is either a terminal node [Terminal tok]
       or a nonterminal node [NonTerminal (prod, csts)].

       A terminal node carries just a token.

       A nonterminal node carries a production index [prod] and an immutable
       array of subtrees [csts]. The length of the array [csts] must be the
       length of the right-hand side of the production [prod]. The sequence of
       the head symbols of the subtrees [csts] must match the right-hand side
       of the production [prod]. The production [prod] must not be a start
       production. *)
    type cst =
      | Terminal    of token
      | NonTerminal of production * cst array

  end

  (**The module [DCST] offers an abstract data type [dcst] of disjunctive
     concrete syntax trees. This definition is generic, that is,
     grammar-independent. This module offers an unsafe API: the smart
     constructors [nonterminal] and [choice] have preconditions whose
     validity is not tested at runtime. A safe, non-generic API {i can} be
     constructed a posteriori on top of this unsafe, generic API. *)
  module DCST : sig

    (**A disjunctive concrete syntax tree (DCST) is a terminal node, a
       nonterminal node, or a disjunction node, also known as a choice node.

       A disjunction node offers a choice between two subtrees. This is
       typically used to express a choice between two ways of displaying
       a piece of syntax: e.g., with or without enclosing parentheses.
       A DCST is typically a DAG: its subtrees can be shared.

       Thanks to disjunction nodes, an (exponentially large) set of CSTs can
       be represented as a single DCST.

       The type [dcst] is presented as an abstract type equipped with three
       constructors, namely {!terminal}, {!nonterminal}, and {!choice}. *)
    type dcst

    (**[terminal tok] constructs a terminal node. *)
    val terminal : token -> dcst

    (**[nonterminal prod dcsts] constructs a nonterminal node. It assumes (but
       does not check) that [prod] is not a start production, that the number
       of subtrees is the length of the right-hand side of production [prod],
       and that the sequence of the head symbols of the subtrees forms the
       right-hand side of production [prod]. *)
    val nonterminal : production -> dcst array -> dcst

    (**[choice dcst1 dcst2] constructs a choice node. It assumes (but does not
       check) that the subtrees [dcst1] and [dcst2] have the same head
       symbol.

       The head symbol of a DCST is defined as follows. The head symbol of
       [terminal tok] is the terminal symbol associated with the token [tok].
       The head symbol of [nonterminal prod dcsts] is the left-hand side of
       the production [prod]. The head symbol of [choice dcst1 dcst2] is the
       common head symbol of [dcst1] and [dcst2]. *)
    val choice : dcst -> dcst -> dcst

  end

  (**[settle (dcst, s, t')] attempts to convert the DCST [dcst] into some CST
     [cst] such that the parser, beginning in state [s], when fed with the
     input sequence [fringe cst] followed with the terminal symbol [t'],
     consumes the sequence [fringe cst], leaves the symbol [t'] unconsumed,
     and constructs the CST [cst]. If this is possible, then this function
     call returns [Some cst]. Otherwise, it returns [None]. *)
  val settle : DCST.dcst * state * terminal -> CST.cst option

end

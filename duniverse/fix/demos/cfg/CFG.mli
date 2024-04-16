open Sigs

(* Provided we have an implementation [M] of imperative maps over
   nonterminal symbols, and an implementaiton [TSet] of sets of
   terminal symbols, we can analyze a context-free grammar [G] as
   follows. *)

module Analyze
  (G : GRAMMAR)
  (M : Fix.IMPERATIVE_MAPS with type key = G.nonterminal)
  (TSet : Set.S with type elt = G.terminal)
: sig

  open G

  (* Which symbols are inhabited (i.e., generate a nonempty language)? *)

  val inhabited_nt: nonterminal -> bool
  val inhabited_prod: production -> bool

  (* Which symbols are nullable (i.e., generate the empty word)? *)

  val nullable_nt: nonterminal -> bool
  val nullable_prod: production -> bool

  (* Computation of FIRST sets. *)

  val first: nonterminal -> TSet.t

end

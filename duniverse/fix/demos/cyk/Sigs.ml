(* A context-free grammar in relaxed Chomsky normal form. *)

(* We relax Chomsky normal form by allowing productions of the form
   N -> epsilon. They are normally disallowed because the CYK algorithm
   does not support them: the algorithm relies on the fact that no symbol
   is nullable to ensure that a query is decomposed into strictly smaller
   queries. In our case, though, Fix supports cyclic dependencies, so it
   is possible to formulate a slightly generalized version of CYK that
   accepts cyclic queries. *)

module type CNF = sig

  type terminal =
    private int

  type nonterminal =
    private int

  val start: nonterminal

  type production =
    | E     (* epsilon *)
    | T  of terminal
    | NN of nonterminal * nonterminal

  val productions: nonterminal -> production list

end

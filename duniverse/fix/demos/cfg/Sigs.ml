(* A signature for a context-free grammar. *)

(* We require a type of terminal symbols, a type of nonterminal symbols, and a
   mapping of nonterminal symbols to productions. The syntax of productions is
   flexible: we allow arbitrary combinations of sequences and choices, whereas
   in traditionally all choices must be hoisted out of sequences. *)

module type GRAMMAR = sig

  type terminal

  type nonterminal

  type production =
    | Epsilon
    | T of terminal
    | N of nonterminal
    | Seq of production * production
    | Alt of production * production

  val productions: nonterminal -> production

end

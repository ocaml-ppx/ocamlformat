open Sigs

module Make (G : CNF) : sig

  val generate: G.nonterminal -> int -> G.terminal array

end

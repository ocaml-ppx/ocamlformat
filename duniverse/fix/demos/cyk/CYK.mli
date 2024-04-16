open Sigs

(* This is the CYK algorithm, formulated as an instance of Fix.
   Thus, scheduling is performed internally by Fix, whereas in
   a traditional dynamic-programming formulation it is explicit.
   In our formulation, cyclic dependencies are permitted, so we
   are able to accept grammars in a relaxed Chomsky normal form
   where productions of the form N -> epsilon are allowed. *)

module Make (G : CNF) : sig

  val parse: G.terminal array -> bool

end

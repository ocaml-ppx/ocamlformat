(* In its simplest form, the grammar only has one production:

     E -> (E) | EE | epsilon

   In relaxed Chomsky normal form, we add two nonterminal symbols L and R
   to stand for the parentheses and one nonterminal symbol M to stand for
   the concatenation LE. This yields

     E -> MR | EE | epsilon
     M -> LE
     L -> (
     R -> )

   In strict Chomsky normal form, the production E -> epsilon would be
   disallowed, so we would have to further transform the grammar by
   adding a start symbol S and by modifying E and M, as follows (I think):

     S -> E | epsilon
     E -> MR | EE
     M -> LE | (
     L -> (
     R -> )

*)

type terminal = int

let lpar : terminal = 0
let rpar : terminal = 1

let print_terminal = function
  | 0 -> print_string "("
  | 1 -> print_string ")"
  | _ -> assert false

let toggle t =
  1 - t

type nonterminal =
    int

let e : nonterminal = 0
let m : nonterminal = 1
let l : nonterminal = 2
let r : nonterminal = 3

type production =
  | E     (* epsilon *)
  | T  of terminal
  | NN of nonterminal * nonterminal

let productions = function
  | 0 ->
      [ NN (m, r); NN (e, e); E ]
  | 1 ->
      [ NN (l, e) ]
  | 2 ->
      [ T lpar ]
  | 3 ->
      [ T rpar ]
  | _ ->
      assert false

let start =
  e

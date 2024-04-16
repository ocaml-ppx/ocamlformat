(* Unit tests for Fix.Minimize. *)

(* At the time of writing, these are NOT serious tests. *)

open Printf
open Fix.Enum
open Fix.Indexing
open Fix.Minimize

module Label = struct
  type t = int
  let compare = compare
  let print = sprintf "%d"
end

(* -------------------------------------------------------------------------- *)

(* An automaton with zero states. The accepted language is empty. *)

include struct

module A = struct
  (* Number of states. *)
  module N = Const(struct let cardinal = 0 end)
  type states = N.n
  let states = N.n
  type state = states index
  (* Number of transitions. *)
  module M = Const(struct let cardinal = 0 end)
  type transitions = M.n
  let transitions = M.n
  type transition = transitions index
  (* Description of transitions. *)
  let source _t = assert false
  let target _t = assert false
  let label _t = assert false
  (* Initial and final states. *)
  let initials = empty
  let finals = empty
  (* Extra parameters. *)
  let debug = true
  let groups = empty
end

module A' = Minimize(Label)(A)

let () =
  assert (cardinal A'.states = 0);
  assert (cardinal A'.transitions = 0);
  assert (length A'.initials = 0);
  assert (length A'.finals = 0);
  ()

end

(* -------------------------------------------------------------------------- *)

(* An automaton with one state and no transition. The accepted
   language contains just the empty string. *)

include struct

module A = struct
  (* Number of states. *)
  module N = Const(struct let cardinal = 1 end)
  type states = N.n
  let states = N.n
  type state = states index
  let state (i : int) : state = Index.of_int states i
  (* Number of transitions. *)
  module M = Const(struct let cardinal = 0 end)
  type transitions = M.n
  let transitions = M.n
  type transition = transitions index
  (* Description of transitions. *)
  let source _t = assert false
  let target _t = assert false
  let label _t = assert false
  (* Initial and final states. *)
  let initials = singleton (state 0)
  let finals = singleton (state 0)
  (* Extra parameters. *)
  let debug = true
  let groups = empty
end

module A' = Minimize(Label)(A)

let () =
  assert (cardinal A'.states = 1);
  assert (cardinal A'.transitions = 0);
  assert (length A'.initials = 1);
  assert (length A'.finals = 1);
  ()

end

(* -------------------------------------------------------------------------- *)

(* An automaton with one state, which is not final, and no transition.
   The accepted language is empty. *)

include struct

module A = struct
  (* Number of states. *)
  module N = Const(struct let cardinal = 1 end)
  type states = N.n
  let states = N.n
  type state = states index
  let state (i : int) : state = Index.of_int states i
  (* Number of transitions. *)
  module M = Const(struct let cardinal = 0 end)
  type transitions = M.n
  let transitions = M.n
  type transition = transitions index
  (* Description of transitions. *)
  let source _t = assert false
  let target _t = assert false
  let label _t = assert false
  (* Initial and final states. *)
  let initials = singleton (state 0)
  let finals = empty
  (* Extra parameters. *)
  let debug = true
  let groups = empty
end

module A' = Minimize(Label)(A)

let () =
  assert (cardinal A'.states = 0);
  assert (cardinal A'.transitions = 0);
  assert (length A'.initials = 0);
  assert (length A'.finals = 0);
  ()

end

(* -------------------------------------------------------------------------- *)

(* An automaton with two states and one transition between them. *)

include struct

module A = struct
  (* Number of states. *)
  module N = Const(struct let cardinal = 2 end)
  type states = N.n
  let states = N.n
  type state = states index
  let state (i : int) : state = Index.of_int states i
  (* Number of transitions. *)
  module M = Const(struct let cardinal = 1 end)
  type transitions = M.n
  let transitions = M.n
  type transition = transitions index
  (* Description of transitions. *)
  let source (t : transition) = assert ((t :> int) = 0); state 0
  let target (t : transition) = assert ((t :> int) = 0); state 1
  let label  (t : transition) = assert ((t :> int) = 0); 0
  (* Initial and final states. *)
  let initials = singleton (state 0)
  let finals = singleton (state 1)
  (* Extra parameters. *)
  let debug = true
  let groups = empty
end

module A' = Minimize(Label)(A)

let () =
  assert (cardinal A'.states = 2);
  assert (cardinal A'.transitions = 1);
  assert (length A'.initials = 1);
  assert (length A'.finals = 1);
  ()

end

(* -------------------------------------------------------------------------- *)

(* An automaton with two states, both of which are initial and final,
   and transitions between them. The accepted language is 0*. *)

include struct

module A = struct
  (* Number of states. *)
  module N = Const(struct let cardinal = 2 end)
  type states = N.n
  let states = N.n
  type state = states index
  let state (i : int) : state = Index.of_int states i
  (* Number of transitions. *)
  module M = Const(struct let cardinal = 2 end)
  type transitions = M.n
  let transitions = M.n
  type transition = transitions index
  (* Description of transitions. *)
  let source (t : transition) =
    match (t :> int) with 0 -> state 1 | 1 -> state 0 | _ -> assert false
  let target (t : transition) =
    match (t :> int) with 0 -> state 0 | 1 -> state 1 | _ -> assert false
  let label  (_ : transition) = 0
  (* Initial and final states. *)
  let initials = map state (list [0; 1])
  let finals = map state (list [0; 1])
  (* Extra parameters. *)
  let debug = true
  let groups = empty
end

module A' = Minimize(Label)(A)

let () =
  assert (cardinal A'.states = 1);
  assert (cardinal A'.transitions = 1);
  assert (length A'.initials = 1);
  assert (length A'.finals = 1);
  ()

end

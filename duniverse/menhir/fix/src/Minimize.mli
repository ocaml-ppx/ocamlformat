(******************************************************************************)
(*                                                                            *)
(*                                    Fix                                     *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

open Indexing
open Enum

(**This module offers a {b minimization} algorithm for {b deterministic
   finite automata} (DFAs).

   The algorithm is based on Antti Valmari's 2012 paper, "Fast brief
   practical DFA minimization". Our implementation differs from Valmari's
   in the use of an abstraction barrier between the partition refinement
   data structure and the refinement algorithm. *)

(**The signature [DFA] describes a deterministic finite-state automaton. *)
module type DFA = sig

  type states
  (**The type [states] is a type-level representation of the number of
     states of the automaton. Thus, a state number has type [states index].
     See {!Indexing} for more information about type-level indices. *)

  val states : states cardinal
  (**[states] is the number of states of the automaton. *)

  type state = states index
  (**A state number is an integer in the range [\[0, cardinal states)]. *)

  type transitions
  (**The type [transitions] is a type-level representation of the number of
     transitions of the automaton. Thus, a transition number has type
     [transitions index]. See {!Indexing} for more information about
     type-level indices. *)

  val transitions : transitions cardinal
  (**[transitions] is the number of transitions of the automaton. *)

  type transition = transitions index
  (**A transition number is an integer in the range
     [\[0, cardinal transitions)]. *)

  type label
  (**[label] is the type of transition labels. *)

  val label  : transition -> label
  (**[label t] returns the label of the transition [t]. *)

  val source : transition -> state
  (**[source t] returns the source state of the transition [t]. *)

  val target : transition -> state
  (**[target t] returns the target state of the transition [t]. *)

  val initials : state enum
  (**[initials] is an enumeration of the initial states. *)

  val finals : state enum
  (**[finals] is an enumeration of the initial states. *)

end

(**The functor application [Minimize(Label)(A)] applies a partition refinement
   algorithm to minimize the deterministic finite automaton [A]. The time
   complexity of this operation is {i O(n + m.log m)}, where {i n} is the
   number of states of the automaton [A] and {i m} is the number of its
   transitions. This bound is independent of the size of the alphabet of the
   transition labels. *)
module Minimize
  (Label : sig

     type t
     (**The type of transition labels. *)

     val compare : t -> t -> int
     (**A total order on transition labels. *)

     val print : t -> string
     (**[print] is used for debugging purposes only. *)

  end)
  (A: sig

    (** @closed *)
    include DFA with type label := Label.t
    (**[A] is the automaton that must be minimized. *)

    val debug : bool
    (**If [debug] is true then the algorithm checks that the automaton is
       deterministic, that is, no two transitions have the same source
       state and label. *)

    val groups : state enum enum
    (**[groups] is a possibly empty enumeration of groups, where a group is
       a possibly empty set of states. The minimization algorithm must
       respect every group in this enumeration. That is, for each group,
       two states can be regarded as equivalent only if they both lie
       outside this group or both lie inside this group. *)

   end)
: sig

    (** @closed *)
    include DFA with type label = Label.t
    (**A description of the minimized automaton [A'].

      A state of the original automaton {!A} is retained only if it is {b
      accessible} and {b co-accessible}, that is, only if this state lies
      on a path from some initial state to some final state.

      The states that are retained are then {b grouped into equivalence
      classes}; these equivalence classes form the states of the minimal
      automaton [A'].

      The equivalence classes are made {b as coarse as possible} (that is, as
      large as possible), while obeying the following constraints:

      - The equivalence classes must respect the group of the final states,
      {!A.finals}. That is, if two states are deemed equivalent, then they
      must be both final or both non-final.

      - Every group supplied by the user via {!A.groups} must similarly be
      respected.

      - For every label [l], the equivalence classes must be compatible with
      the transitions that carry the label [l]. That is, if two states [s1]
      and [s2] are deemed equivalent, and if the state [s1] has a transition
      labeled [l] towards some state [s'1], then the state [s2] must have a
      transition labeled [l] towards some state [s'2] such that [s'1] and
      [s'2] are also deemed equivalent. *)

  val transport_state : A.state -> state option
  (**The function [transport_state] maps a state of the original automaton {!A}
     to the corresponding state of the minimal automaton [A']. It is a partial
     function: [transport_state s] is [Some _] if and only if the state [s]
     lies on a path from some initial state to some final state. *)

  val transport_transition : A.transition -> transition option
  (**The function [transport_transition] maps a transition of the original
     automaton {!A} to the corresponding transition of the minimal automaton
     [A']. It is a partial function: [transport_transition t] is [Some _] if
     and only if the transition [t] lies on a path from some initial state to
     some final state. *)

  val backport_state_one : state -> A.state
  (**The function [backport_state_one] maps a state [s'] of the minimal
     automaton [A'] to a state [s] of the original automaton {!A} such that
     [transport_state s = Some s'] holds. There may exist several states [s]
     such that this equation holds. A representative state in each equivalence
     class is chosen, and [backport_state_one] always returns a representative
     state. *)

  val backport_state_all: state -> A.state enum
  (**The function [backport_state_all] maps a state [s'] of the minimal
     automaton [A'] to an enumeration of all states [s] of the original
     automaton {!A} such that [transport_state s = Some s'] holds. There exists
     at least one such state: this enumeration is never empty. *)

  val backport_transition : transition -> A.transition
  (**The function [backport_transition] maps a transition [t'] of the minimal
     automaton [A'] to a transition [t] of the original automaton {!A} such
     that [transport_transition t = Some t'] holds. There may exist several
     transitions [t] such that this equation holds; [backport_transition]
     returns the transition whose source state is a representative state. *)

end

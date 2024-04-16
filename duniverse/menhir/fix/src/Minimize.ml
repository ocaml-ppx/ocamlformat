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

open Printf
open Indexing
open Enum

module type DFA = sig
  type states
  val states : states cardinal
  type state = states index
  type transitions
  val transitions : transitions cardinal
  type transition = transitions index
  type label
  val label  : transition -> label
  val source : transition -> state
  val target : transition -> state
  val initials : state enum
  val finals : state enum
end

module Minimize
  (Label : sig
     type t
     val compare : t -> t -> int
     val print : t -> string
  end)
  (A: sig
     include DFA with type label := Label.t
     val debug : bool
     val groups : state enum enum
   end)
= struct

  type label = Label.t

  (* For convenience, let [m] and [n] be the number of transitions
     and the number of states. *)

  let m, n =
    cardinal A.transitions,
    cardinal A.states

  (* If [A.debug] is set, check that the automaton is deterministic.
     No two transitions can have the same source state and label. *)

  let () =
    if A.debug then begin
      (* Build an array of all transitions. *)
      let transitions = Vector.init A.transitions (fun t -> t) in
      (* Sort this array. *)
      let cmp t1 t2 =
        let s1, s2 = A.source t1, A.source t2 in
        let c = compare s1 s2 in
        if c <> 0 then c
        else Label.compare (A.label t1) (A.label t2)
      in
      Vector.sort cmp transitions;
      (* Look for adjacent duplicates. *)
      let ok = ref true in
      for i = 0 to m - 2 do
        let t1 = Vector.get transitions (Index.of_int A.transitions i)
        and t2 = Vector.get transitions (Index.of_int A.transitions (i + 1)) in
        if cmp t1 t2 = 0 then begin
          eprintf "Minimize: invalid input data.\n\
                   The automaton is not deterministic.\n\
                   Transitions %d and %d\n\
                   both have source state %d and label %s.\n%!"
            (t1 :> int) (t2 :> int)
            (A.source t1 :> int) (Label.print (A.label t1));
          ok := false
        end
      done;
      assert !ok
    end

  (* [--f.(s)]. *)

  let[@inline] predecrement (f : int array) (s : int) =
    assert (f.(s) > 0);
    let j = f.(s) - 1 in
    f.(s) <- j;
    j

  (* [adjacent_transitions endpoint] computes and returns a function that maps
     each state in the automaton A to an enumeration of its adjacent
     transitions, where "adjacent" means either incoming or outgoing. If
     [endpoint] is [A.target], then we get the incoming transitions; if
     [endpoint] is [A.source], then we get the outgoing transitions. *)

  (* This function corresponds to [make_adjacent], plus a few bits of
     [rem_unreachable], in Valmari's paper. The arrays [f] and [a] below
     correspond to [F] and [A] in Valmari's code. The function [endpoint]
     corresponds to [K] in Valmari's code. *)

  let adjacent_transitions (endpoint : A.transition -> A.state)
  : A.state -> A.transition enum =

    (* Build a table [f] that maps each state to the number of its adjacent
       transitions. Allow for one extra slot at the end (see below). *)
    let f : int array = Array.make (n + 1) 0 in
    (* By iterating over all transitions [t], count how many transitions are
       adjacent to each state [s]. *)
    Index.iter A.transitions begin fun t ->
      let s = (endpoint t :> int) in
      f.(s) <- f.(s) + 1
    end;
    (* Now compute cumulative sums, so as to assign a unique index to each
       transition. The extra slot at the end of [f] becomes useful now. *)
    for s = 0 to n - 1 do
      f.(s + 1) <- f.(s + 1) + f.(s)
    done;
    (* For each state [s], the range [\[f.(s-1), f.(s))] is the set of
       the indices of the adjacent transitions of the state [s]. *)
    (* When [s] is zero, [f.(s-1)] in the previous sentence stands for zero. *)
    (* Build a table [a] that maps transition indices back to transitions. *)
    let a : A.transition array =
      if m = 0 then [||]
      else Array.make m (Index.of_int A.transitions 0)
    in
    Index.rev_iter A.transitions begin fun t ->
      let s = (endpoint t :> int) in
       (* Allocate a transition index by decrementing [f.(s)].
          Record that this transition index corresponds to transition [t]. *)
      a.(predecrement f s) <- t
    end;
    (* Due to the manner in which [f] has been modified by the previous loop,
       the content of the array [f] has now been shifted towards the right by
       one. Thus, we have: *)
    (* For each state [s], the range [\[f.(s), f.(s+1))] is the set of
       the indices of the adjacent transitions of the state [s]. *)
    assert (f.(n) = m);
    (* The tables [f] and [a] can now be exploited to build a function that
       maps a state [s] to an enumeration of its adjacent transitions. *)
    let adjacent_transitions (s : A.state) : A.transition enum =
      enum @@ fun yield ->
        let s = (s :> int) in
        for i = f.(s) to f.(s + 1) - 1 do yield a.(i) done
    in
    adjacent_transitions

  (* Compute the incoming and outgoing transitions of every state. *)

  (* Valmari refers to the target of a transition as its "head" and
     to its source as its "tail". This is consistent with a vision
     of each transition as an arrow. *)

  let outgoing_transitions, incoming_transitions =
    adjacent_transitions A.source,
    adjacent_transitions A.target

  (* Create a partition of the states, where all states are placed
     in a single block. *)

  (* It is named B in Valmari's paper, Fig. 2. *)

  let blocks =
    Partition.create A.states

  (* [discard_unreachable_states roots endpoint adjacent_transitions] performs
     a graph traversal to discover which states are reachable from the states
     [roots] by taking transitions in the direction indicated by [endpoint].
     The parameter [adjacent_transitions] must be obtained by the application
     of [adjacent_transitions] (above) to [endpoint]. *)

  (* This corresponds roughly to the first half of [rem_unreachable] in
     Valmari's paper. *)

  let discard_unreachable_states
    (roots : A.state enum)
    (endpoint : A.transition -> A.state)
    (adjacent_transitions : A.state -> A.transition enum)
  =
    (* If [n] is zero, there is nothing to do. If [n] is nonzero then the
       partition has exactly one block, whose index is 0. *)
    if n > 0 then begin
      let blk = 0 in
      (* Mark every root. *)
      foreach roots begin fun s ->
        Partition.mark blocks s
      end;
      (* As long as there exist an unprocessed marked state [s], *)
      Partition.exhaust_marked_elements blocks blk begin fun s ->
        (* For every transition [t] that is adjacent to [s], *)
        foreach (adjacent_transitions s) begin fun t ->
          (* Mark the state that is at the other end of this transition. *)
          Partition.mark blocks (endpoint t)
        end
      end;
      (* All reachable states are now marked. *)
      (* Discard the unmarked states, which must be unreachable. *)
      Partition.discard_unmarked blocks
    end

  (* Determine which states are accessible (that is, reachable from an initial
     state by following transitions forward). Discard those that are not. *)

  let () =
    discard_unreachable_states A.initials A.target outgoing_transitions

  (* Determine which states are co-accessible (that is, reachable from a final
     state by following transitions backward). Discard those that are not. *)

  let () =
    discard_unreachable_states A.finals A.source incoming_transitions

  (* [discarded_state s] determines whether a state [s] has been discarded. *)

  let discarded_state (s : A.state) : bool =
    Partition.find blocks s = -1

  (* [respect group], where [group] is a possibly empty set of states, refines
     the current partition so as to be compatible with [group]. That is, every
     block must lie either entirely outside or entirely inside [group]. If a
     a block does not respect this constraint, is is split now. *)

  let respect (group : A.state enum) =
    (* Mark every state in the group. *)
    foreach group begin fun s ->
      Partition.mark blocks s
    end;
    (* Split every block that contains both marked and unmarked states. *)
    Partition.split blocks

  (* Impose the constraint that the set of final states must be respected.
     That is, no block is allowed to contain a final state and a non-final
     state. *)

  (* Impose any additional constraints that the user wishes to enforce. *)

  let () =
    foreach (cons A.finals A.groups) respect

  (* Create a partition of the transitions, where transitions are grouped
     per label -- that is, there is at most one block per label. Once the
     transitions have been separated in this way, their labels are not
     consulted any more during the refinement process. *)

  (* The word "cord" designates a block of the transition partition. *)

  let cords =
    let partition t1 t2 = Label.compare (A.label t1) (A.label t2) in
    Partition.create A.transitions ~partition

  (* Discard every transition whose source or target state has been
     discarded. *)

  let () =
    Partition.discard cords (fun t ->
      discarded_state (A.source t) || discarded_state (A.target t)
    )

  (* [discarded_transition t] determines whether a transition [t] has been
     discarded. *)

  let discarded_transition (t : A.transition) : bool =
    Partition.find cords t = -1

  (* The main refinement loop. *)

  (* The variable [b] keeps track of the number of ready blocks. A block is
     ready if its index is less than [!b], and unready otherwise. Similarly,
     a cord is ready if its index is less than [!c], unready otherwise. The
     algorithm relies on the fact that when a block is split in two parts,
     the larger part retains the existing block index, which is thereafter
     considered ready, while the smaller part receives a new block index,
     which is considered unready. This scheme removes the need to explicitly
     maintain a queue of unready block indices, and removes the need for the
     function [Partition.split] to explicitly insert newly created blocks
     into such a queue. *)

  (* [b] is initially set to 1, which means that the block number 0 is
     initially considered ready. (See Valmari's Lemma 2.) *)

  (* The structure of the two nested loops follows Valmari's paper. It is
     slightly dissymetric: e.g., it processes one cord, then processes as
     many blocks as possible, then processes one cord, etc. Presumably, any
     strategy is valid: at any time, if there is both an unready block and
     an unready cord, then one can in principle choose to process either of
     them. *)

  let () =
    let b = ref 1 in
    let c = ref 0 in
    (* While there exists an unready cord [!c]: *)
    while !c < Partition.block_count cords do
      (* Process cord [!c]. *)
      (* Mark the source state of every transition in this cord. *)
      Partition.iter_block_elements cords !c begin fun t ->
        Partition.mark blocks (A.source t)
      end;
      (* Split the blocks according to these marks. Thus, two states become
         distinguished if one of them is the source of some transition in
         cord [!c], while the other is not. In other words, after splitting,
         every block is compatible with the cord [!c]. *)
      Partition.split blocks;
      (* While there exists an unready block [!b]: *)
      while !b < Partition.block_count blocks do
        (* Mark every incoming transition of every state in this block. *)
        Partition.iter_block_elements blocks !b begin fun s ->
          foreach (incoming_transitions s) begin fun t ->
            Partition.mark cords t
          end
        end;
        (* Split the cords according to these marks. Thus, two transitions
           become distinguished if one of them has a target state in block
           [!b], while the other does not. In other words, after splitting,
           every cord is compatible with the block [!b]. *)
        Partition.split cords;
        (* Mark the block [!b] as ready. *)
        incr b
      done;
      (* Mark the cord [!c] as ready. *)
      incr c
    done

  (* The partition refinement process is now complete. There remains to
     publish its result. *)

  (* Publish the number of states of the minimized DFA. *)

  module States = Const(struct let cardinal = Partition.block_count blocks end)
  type states = States.n
  let states = States.n
  type state = states index

  (* Count and index the transitions of the new DFA. *)

  (* Two transitions in the same cord (that is, two equivalent transitions)
     must have the same label and equivalent target states. They do *not*
     necessarily have equivalent source states. Thus, one should not expect
     that each cord gives rise to only one transition in the new DFA. *)

  (* A transition of the original DFA is representative if it has not been
     discarded and its source state is the representative of its block. By
     keeping just the representative transitions of the original DFA, we get
     the correct number of transitions in the new DFA, and we are able to
     construct these new transitions. *)

  let is_representative (t : A.transition) =
    not (discarded_transition t) &&
    Partition.is_chosen blocks (A.source t)

  module RepresentativeTransitions =
    Vector.Of_array (struct
      let representative_transitions : A.transition enum =
        enum @@ fun yield ->
          Index.iter A.transitions @@ fun t ->
            if is_representative t then yield t
      type a = A.transition
      let array = enum_to_reversed_array representative_transitions
        (* The order of the transitions in the array does not matter. *)
    end)

  (* Publish the number of transitions of the minimized DFA. *)

  type transitions = RepresentativeTransitions.n
  let transitions = Vector.length RepresentativeTransitions.vector
  type transition = transitions index

  (* Debugging output: print the transitions in each cord. *)

  let () =
    if false then
      for c = 0 to Partition.block_count cords - 1 do
        eprintf "Cord %d:\n" c;
        Partition.iter_block_elements cords c begin fun (t : A.transition) ->
          eprintf "  Transition %d (source %d, target %d, label %s)%s\n"
            (t :> int)
            (A.source t :> int) (A.target t :> int)
            (Label.print (A.label t))
            (if is_representative t then " (*)" else "")
        end
      done

  (* Set up functions that map a state of the original DFA to a state of the
     new DFA (if possible). *)

  let transport_state_unsafe (s : A.state) : int =
    Partition.find blocks s
      (* This index can be -1 if the state [s] has been discarded. *)

  let transport_undiscarded_state (s : A.state) : state =
    assert (not (discarded_state s));
    let s' = transport_state_unsafe s in
    assert (s' <> -1);
    Index.of_int states s'

  let transport_state (s : A.state) : state option =
    match transport_state_unsafe s with
    | -1 -> None
    | n -> Some (Index.of_int states n)

  (* Set up functions that map a state of the new DFA back to either one or
     all of the corresponding states of the original DFA. *)

  let backport_state_one (s' : state) : A.state =
    Partition.choose blocks (s' :> int)
      (* A state index in the new DFA is a block index. *)

  let backport_state_all (s' : state) : A.state enum =
    enum @@ Partition.iter_block_elements blocks (s' :> int)

  (* Set up functions that map a transition of the new DFA to a transition
     of the original DFA, to a label, and to source and target states in the
     new DFA. *)

  let backport_transition (t' : transition) : A.transition =
    Vector.get RepresentativeTransitions.vector t'
      (* A transition index in the new DFA is an index into the transition
         vector that was built above. *)

  let label (t' : transition) : Label.t =
    A.label (backport_transition t')

  let source (t' : transition) : state =
    let t = backport_transition t' in
    assert (not (discarded_transition t));
    transport_undiscarded_state (A.source t)

  let target (t' : transition) : state =
    let t = backport_transition t' in
    assert (not (discarded_transition t));
    transport_undiscarded_state (A.target t)

  (* Set up enumerations of the initial states and of the final states. *)

  let transport_states (ss : A.state enum) : state enum =
    (* We must filter out discarded states, transport states from the
       original DFA to the new DFA, and eliminate duplicate elements
       in the enumeration. Marks can be used (abused?) to achieve this. *)
    foreach ss (fun s -> Partition.mark blocks s);
    let ss' =
      Partition.get_marked_blocks blocks
      |> Array.map (Index.of_int states)
      |> array
    in
    Partition.clear_marks blocks;
    ss'

  let initials : state enum =
    transport_states A.initials

  let finals : state enum =
    transport_states A.finals

  (* Set up a function that maps a transition of the original DFA to a
     transition of the new DFA (if possible). *)

  (* It is the inverse of [backport_transition], which is injective. *)

  let table : (A.transitions, transition option) vector =
    Vector.invert A.transitions RepresentativeTransitions.vector

  let transport_transition (t : A.transition) : transition option =
    Vector.get table t

end

(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU Library General Public License version 2, with a    *)
(*   special exception on linking, as described in the file LICENSE.          *)
(*                                                                            *)
(******************************************************************************)

(* This flags enables runtime assertion checking. *)
let debug =
  false

(* This flag controls backtracking. *) (* TODO *)
let backtracking =
  true

(* We need the types [state] and [production] to support equality and
   hashing, because we build a hash table whose keys are pairs of a state
   [s] and a production [prod]. We could use OCaml's generic equality and
   hashing functions. We prefer to explicitly require that both states and
   productions be convertible to integers. For productions, this is done
   via a subtyping relation [production :> int]. For states, this is done
   via a function [number : state -> int]. *)

module[@inline] Make (A : sig
  type terminal
  type nonterminal
  type production = int
  type state
  val number: state -> int
  type token
  val token2terminal: token -> terminal
  val lhs : production -> nonterminal
  val maybe_shift_t : state -> terminal -> state option
  val maybe_goto_nt : state -> nonterminal -> state option
  val may_reduce_prod : state -> terminal -> production -> bool
end) = struct
open A

(* -------------------------------------------------------------------------- *)

(* States. *)

module State = struct
  let[@inline] equal (s1 : state) (s2 : state) = number s1 = number s2
  let[@inline] hash (s : state) = number s
end

(* Productions. *)

module Production = struct
  let[@inline] equal (prod1 : production) (prod2 : production) =
    (prod1 :> int) = (prod2 :> int)
  let[@inline] hash (prod : production) = (prod :> int)
end

(* State-production pairs. *)

module SP = struct
  type t = state * production
  let hash (s, prod) =
    (State.hash s, Production.hash prod) |> Hashtbl.hash
  let equal (s1, prod1) (s2, prod2) =
    State.equal s1 s2 &&
    Production.equal prod1 prod2
end

(* -------------------------------------------------------------------------- *)

module CST = struct

type cst =
  | Terminal    of token
  | NonTerminal of production * cst array

let dummy : cst =
  NonTerminal ((0 : production), [||])

end

type cst =
  CST.cst

(* -------------------------------------------------------------------------- *)

module DCST = struct

type dcst =
  | Choice      of dcst * dcst
  | Terminal    of token
  | NonTerminal of production * dcst array

(* -------------------------------------------------------------------------- *)

(* The head symbol of a DCST. *)

(* We could give separate definitions of the type [symbol], of the function
   [head], which returns the head symbol of a DCST, and of the function
   [transition], which takes a symbol as an argument.

   Instead, we give a direct definition of the function [transition_head],
   which is the composition of [head] and [transition]. This removes the
   need to define the type [symbol] and saves one memory allocation. *)

let rec transition_head (s : state) (dcst : dcst) : state option =
  match dcst with
  | Choice (dcst1, _dcst2) ->
      transition_head s dcst1
  | Terminal tok ->
      maybe_shift_t s (token2terminal tok)
  | NonTerminal (prod, _) ->
      maybe_goto_nt s (lhs prod)

(* -------------------------------------------------------------------------- *)

(* Smart constructors for DCSTs. *)

(* The smart constructor [nonterminal] could check that [prod] is not a
   start production, that the number of subtrees is correct, and that the
   head symbols of the subtrees are correct. The smart constructor [choice]
   could check that both branches of a choice have the same head symbol.
   Fortunately, if a strongly-typed view of DCSTs and CSTs is offered to the
   end user, then these checks are unnecessary. Such a strongly-typed view
   is *not* offered by the API of this module (so, this API is unsafe). A
   safe API *can* be constructed a posteriori on top of this unsafe API. *)

(* The constructor of [terminal] DCSTs. *)

let[@inline] terminal tok =
  Terminal tok

(* The constructor of [nonterminal] DCSTs. *)

let[@inline] nonterminal prod dcsts =
  (* Omitted check:
  assert (not (is_start prod));
  let rhs = Production.rhs prod in
  assert (Array.length rhs = Array.length dcsts);
  assert (for i = 0 to Array.length rhs - 1 do
    let symbol, _, _ = rhs.(i) in
    assert (Symbol.equal symbol (head dcsts.(i)));
  done; true);
   *)
  NonTerminal (prod, dcsts)

(* The smart constructor of [choice] DCSTs. *)

(* This smart constructor guarantees that a choice is never nested inside the
   left branch of a choice. It does this by re-balancing choices on the fly so
   that they lean towards the right. The order of the branches is preserved. *)

(* This is potentially inefficient, as it is analogous to list concatenation.
   We do not expect this to be a problem in practice, as most choices should
   have few branches. *)

let rec choice dcst1 dcst2 =
  (* Omitted check:
  assert (Symbol.equal (head dcst1) (head dcst2));
   *)
  match dcst1 with
  | Choice (dcst1a, dcst1b) ->
      Choice (dcst1a, choice dcst1b dcst2)
  | Terminal _
  | NonTerminal _ ->
      Choice (dcst1, dcst2)

end (* DCST *)

type dcst =
  DCST.dcst

type dcsts =
  dcst array

(* -------------------------------------------------------------------------- *)

(* The following functions test whether a certain path in the automaton exists,
   and if so, what state this path leads to. *)

(* [follow_suffix s dcsts i] follows the path that begins at the state [s] and
   whose edges are labeled with the head symbols of the trees found at offset
   [i] in the array [dcsts]. It returns [None] if this path does not exist.
   It returns [Some s'] if this path exists and leads to the state [s']. *)

let rec follow_suffix (s : state) (dcsts : dcsts) (i : int) : state option =
  let n = Array.length dcsts in
  if debug then assert (0 <= i && i <= n);
  if i = n then
    Some s
  else
    match DCST.transition_head s dcsts.(i) with
    | None    -> None
    | Some s' -> follow_suffix s' dcsts (i+1)

(* [follow] specializes [follow_suffix] with the offset 0. *)

let[@inline] follow (s : state) (dcsts : dcsts) : state option =
  follow_suffix s dcsts 0

(* Under the assumption that the trees [dcsts] form a valid right-hand side
   for the production [prod], this new definition of [follow prod s dcsts]
   returns the same result as [follow s dcsts] above. It is memoized, using
   a hash table whose keys are pairs [(s, prod)]. *)

(* Intuitively, it may seem that the parameter [dcsts] should not be needed,
   as the sequence of the head symbols of [dcsts] is the right-hand side of
   the production [prod], so it is determined by [prod]. However, we do not
   have a table that maps a production to its right-hand side. This is why
   [dcsts] is needed here (and it is fortunately at hand). *)

let follow : state -> production -> dcsts -> state option =
  let module T = Hashtbl.Make(SP) in
  let table = T.create 1023 in
  fun s prod dcsts ->
    try
      T.find table (s, prod)
    with Not_found ->
      let o = follow s dcsts in
      T.add table (s, prod) o;
      o

(* -------------------------------------------------------------------------- *)

(* [apparently_viable prod dcsts s t'] assumes that the state [s] has an
   outgoing transition whose symbol is [lhs prod]. It also assumes that the
   head symbols of the trees [dcsts] correspond to the right-hand side of
   production [prod].

   It tests whether, with respect to state [s] and lookahead symbol [t'],
   the production [prod] is "apparently viable".

   By definition, this means that there exists a path that begins at state
   [s], follows a series of edges whose labels correspond to the right-hand
   side of production [prod], and ends at a state [s'] where reducing [prod],
   with lookahead symbol [t'], is permitted. *)

let[@inline] apparently_viable_production prod dcsts s t' : bool =
  if debug then assert (maybe_goto_nt s (lhs prod) <> None);
  (* Test whether this path exists and leads to a state [s'] . *)
  match follow s prod dcsts with
  | None ->
      false
  | Some s' ->
      may_reduce_prod s' t' prod

(* [apparently_viable dcst s t'] assumes that the state [s] has an outgoing
   transition labeled with the head symbol of [dcst]. It also assumes that
   the tree [dcst] is not [Choice _].

   It tests whether, with respect to state [s] and lookahead symbol [t'],
   the tree [dcst] is "apparently viable".

   If [dcst] is a terminal node, then it is apparently viable. If it is a
   nonterminal node [NonTerminal (prod, _)], then, by definition, it is
   apparently viable if, with respect to [s] and [t'], the production [prod]
   is apparently viable. *)

(* If [apparently_viable dcst s t'] is false then the tree [dcst] definitely
   cannot be settled (printed) in the context represented by the state [s]
   and by the lookahead symbol [t'].

   If [apparently_viable dcst s t'] is true, however, then we cannot know for
   sure whether [dcst] can be settled in this context. This tree looks good at
   depth 1, but perhaps, by inspecting it at a greater depth, a problem would
   be discovered. *)

let[@warning "-32"] apparently_viable dcst s t' : bool =
  if debug then assert (DCST.transition_head s dcst <> None);
  match dcst with
  | Choice _ ->
      assert false
  | Terminal _ ->
      true
  | NonTerminal (prod, dcsts) ->
      apparently_viable_production prod dcsts s t'

(* The function [apparently_viable] is in fact not used, because we manually
   inline it at its call site inside [settle]. *)

(* -------------------------------------------------------------------------- *)

(* [settle dcst s t'] assumes that the state [s] has an outgoing transition
   labeled with the head symbol of [dcst].

   It attempts to "settle" the tree [dcst], that is, to convert [dcst] into
   a concrete syntax tree (CST), in the context represented by the state [s]
   and by the lookahead symbol [t'].

   The state [s] encodes information about the left context (the text that
   appears to the left of the tree [dcst]), whereas the lookahead symbol [t']
   represents the right context: it is the first symbol that will appear to
   the right of the tree [dcst].

   If the tree [dcst] can be settled in this context then [settle] returns a
   pair [(t, cst)] where [cst] is a viable concrete syntax tree and [t], a
   terminal symbol, is the first symbol of the sequence [fringe t @ [t']].

   If the tree cannot be settled in this context then [settle] raises the
   exception [CouldNotSettle].

   [settle] is correct: if it succeeds, then [cst] is indeed a correct
   rendition of [dcst] in this context. That is to say, the parser, beginning
   in state [s] and fed with the input sequence [fringe cst ++ [t']], will
   recognize [cst], will leave [t'] unconsumed, and will follow one edge
   (labeled with the symbol [head dcst]) from state [s] to some state [s'].

   When [backtracking] is false, then [settle] is not complete: it can raise
   [CouldNotSettle] even though [dcst] can be settled. Indeed, at a choice
   node, if the left branch is apparently viable, then the algorithm commits
   to this branch. If this branch later turns out to be unsolvable then the
   algorithm fails; it does not come back to the choice point to examine the
   other branch.

   When [backtracking] is true, [settle] is complete: if [dcst] can be settled,
   then the algorithm must succeed, and cannot raise [CouldNotSettle]. This is
   achieved by backtracking at choice points.

   Setting [backtracking] to [true] has a certain cost in stack space: the
   exception handlers take up stack space, even if they are never used.
   Furthermore, when [backtracking] is true, the algorithm could have very
   bad (exponential?) complexity. If a subtree, deep down, cannot be settled,
   then the complexity is expected to be Omega(2^n) where [n] is the number
   of choice points above the problematic subtree. *)

(* [settle] is written in destination-passing style, so, instead of returning
   a pair [(t, cst)], as suggested above, it actually returns just [t] and
   writes [cst] into the array slot [csts.(i)]. The array [csts] and the
   offset [i] are extra parameters; together they form a "destination". This
   style reduces memory allocation ([settle] no longer needs to return a
   pair) and stack allocation (the call from [settle_seq_step] to [settle]
   becomes a tail call). *)

exception CouldNotSettle

let rec settle (dcst : dcst) (s : state) (t' : terminal) csts i : terminal =
  if debug then assert (DCST.transition_head s dcst <> None);
  match dcst with

  (* When [backtracking] is false, the case of [Choice] nodes can be
     written as follows:

  | DCST.Choice (dcst1, dcst2) ->
      if apparently_viable dcst1 s t' then
        settle dcst1 s t'
      else
        settle dcst2 s t'

     If [apparently_viable] returns [false] then we are happy: we are certain
     that the left branch cannot succeed, so we commit to the right branch.

     If [apparently_viable] returns [true] then we do not know for sure that
     the left branch will succeed. Nevertheless, we optimistically commit to
     this branch. This makes the algorithm very simple and very fast.

     When [backtracking] is true, the recursive call [settle dcst1 s t'] is
     wrapped in an exception handler, so if the left branch fails, we try
     the second branch.

     Since the left branch of a choice cannot be a choice, [dcst1] must be a
     terminal or nonterminal node. We expand these two subcases and inline away
     the call [settle dcst1 s t']. This allows us to avoid a redundant call to
     [apparently_viable]. We end up with the following cases: *)

  | DCST.Choice (DCST.Choice _, _) ->
      assert false

  | DCST.Choice (DCST.Terminal tok, _)
  | DCST.Terminal tok ->
      csts.(i) <- CST.Terminal tok;
      token2terminal tok

  | DCST.Choice (DCST.NonTerminal (prod, dcsts), dcst2) ->
      if apparently_viable_production prod dcsts s t' then
        (* The left branch is apparently viable. If [backtracking] is false,
           commit to this branch. If [backtracking] is true, try this branch;
           if it fails, try the second branch. *)
        if not backtracking then
          settle_production prod dcsts s t' csts i
        else
          try
            settle_production prod dcsts s t' csts i
          with CouldNotSettle ->
            settle dcst2 s t' csts i
      else
        (* The left branch is definitely not viable: commit to the other
           branch. *)
        settle dcst2 s t' csts i

  | DCST.NonTerminal (prod, dcsts) ->
      (* This case looks like the previous one. However, here, we are not in a
         choice node. If this production is not viable then we are definitely
         in a dead end: we must fail. (Either we mistakenly committed earlier
         to an unsolvable branch; or the user actually gave us an unsolvable
         tree, to begin with.) If this production is viable then all is well,
         for now. *)
      if apparently_viable_production prod dcsts s t' then
        settle_production prod dcsts s t' csts i
      else
        (* Since we know that a handler is installed, there is no point in
           constructing a backtrace, so we use [raise_notrace]. However a
           quick benchmark suggests that this is not more efficient in time
           or space than [raise]. *)
        raise_notrace CouldNotSettle

(* [settle_production] is the special case of [settle] where the tree has the
   form [NonTerminal (prod, dcsts)] and this tree is apparently viable. *)

and settle_production prod dcsts s t' csts i : terminal =
  if debug then assert (apparently_viable_production prod dcsts s t');
  let n = Array.length dcsts in
  (* We allocate an array [csts'] of length [n], initialize it with a dummy
     value (ouch), and pass it to [settle_seq] so that it writes its results
     into this array. *)
  let csts' = Array.make n CST.dummy in
  let t = settle_seq dcsts 0 n s t' csts' in
  (* Then, we write our own result into [csts.(i)]. *)
  csts.(i) <- CST.NonTerminal (prod, csts');
  t

(* [settle_seq dcsts i n s t' csts] settles the sequence of subtrees that begins
   at offset [i] in the array [dcsts]. [n] is the length of this array. The
   resulting trees are written to the array [csts], beginning at offset [i],
   so [settle_seq] returns just a terminal symbol.

   We assume that this subsequence is apparently viable; that is, a path from
   state [s], labeled with the head symbols of the tree sequence defined by
   the array [dcsts] and the offset [i], exists and leads to a state where
   reducing production [prod] with lookahead symbol [t'] is permitted.

   We do not write a runtime assertion to verify this precondition because
   that would be a little awkward; we would need to transport [prod]. *)

and settle_seq dcsts i n s t' csts : terminal =
  if debug then assert (Array.length dcsts = n);
  if debug then assert (0 <= i && i <= n);
  if i = n then
    (* This is the base case. As the sequence is empty, the lookahead symbol
       [t'] falls through. *)
    t'
  else
    settle_seq_step dcsts i n s t' csts

(* [settle_seq_step] is the step case of [settle_seq]. One subtree, [dcsts.(i)],
   is followed by a sequence of subtrees.

   We proceed as follows:

   1. Follow one edge in the automaton to find out what intermediate state
      [is] will be reached after we settle the subtree [dcsts.(i)].
      A key fact is that we [is] can be computed without traversing this
      subtree: the head symbol of this subtree suffices to determine [is].

   2. Based on [is] and on the lookahead symbol [t'], settle the remaining
      subtrees. As a result, we get an intermediate symbol [it], which is
      the first symbol of the remaining subtrees (followed with [t']).

   3. Settle the subtree [dcst.(i)], with [it] as the lookahead symbol.

   As a result of this organization, the start states are computed in a
   left-to-right sweep, while going down into the recursive calls to
   [settle_seq], while the lookahead symbols are computed in a right-to-left
   sweep, while coming out of the recursive calls. The subtrees are settled
   from right to left. *)

and settle_seq_step dcsts i n s t' csts : terminal =
  if debug then assert (Array.length dcsts = n);
  if debug then assert (0 <= i && i < n);
  let dcst = dcsts.(i) in
  (* 1. Per our precondition, an edge from state [s], labeled with the head
        symbol of [dcst], towards some state [is], must exist. *)
  let is = DCST.transition_head s dcst |> Option.get in
  (* 2. We are now in a position to settle the remaining subtrees. *)
  let it = settle_seq dcsts (i+1) n is t' csts in
  (* 3. This call yields the the lookahead symbol [it] that we can use to
        settle [dcst]. *)
  settle dcst s it csts i
  (* We are done. Because [settle] is written in destination-passing style,
     we are able to make a tail call to [settle]. [settle] will take care of
     writing the array slot [csts.(i)] and returning a terminal symbol. *)

(* -------------------------------------------------------------------------- *)

(* A public entry point. *)

let settle (dcst, s, t') : cst option =
  match DCST.transition_head s dcst with
  | None ->
      None
  | Some (_ : state) ->
      try
        (* A dummy array must be created to serve as a destination. *)
        let csts = Array.make 1 CST.dummy in
        let _ : terminal = settle dcst s t' csts 0 in
        Some csts.(0)
      with CouldNotSettle ->
        None

end

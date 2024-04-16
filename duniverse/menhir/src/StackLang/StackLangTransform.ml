(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let map, length = List.(map, length)
let take, drop = MList.(take, drop)
open Printf
open StackLang
let state = EmitStackLang.state
let print = Tag.print
module Print = StackLangPrinter.ToString

(* -------------------------------------------------------------------------- *)

(* [no_info format ... x] returns [x]. [no_info] is used in the definition of
   [info], and has the same type as [info]. *)

(* [ikfprintf] has existed since OCaml 4.01.0, but its type has been made more
   general in OCaml 4.03.0, and this extra generality is exploited here. *)

let no_info format =
  ikfprintf (fun () x -> x) () format

(* [info format ... block] constructs an information message which appears in
   the generated code if [decorate] is true. Its result is the block [block],
   possibly annotated with a comment. *)

let decorate =
  false

let info =
  if decorate then
    fun format ->
      ksprintf (fun s block -> IComment (s, block)) format
  else
    no_info

(* [kinfo] is analogous to [info], but is applied to a triple [(_, _, block)]
   instead of just a block [block]. *)

let kinfo =
  if decorate then
    fun format ->
      ksprintf (fun s (x, y, block) -> x, y, IComment (s, block)) format
  else
    no_info

(* Correct singular and plural forms for "cells". *)

let cells i =
  match i with
  | 0 ->
      "0 cell"
  | 1 ->
      "1 cell"
  | _ ->
      sprintf "%d cells" i

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Specialization with respect to the state. *)

(* The purpose of this program transformation is to specialize certain
   routines for the situation where the [state] register contains a
   statically known tag.

   This is important, in particular, because it allows Unit Production
   Elimination (UPE).

   One might wonder whether we also need to specialize certain routines for
   the situation where the top stack cell contains a statically known tag.
   This is the case of the [reduce] routines associated with unit
   productions. We are able to get away without this extra complexity by
   recognizing these routines and inlining them instead, with the same
   end result. *)

module SpecializeState (X : sig val program : program end) = struct open X

(* [can_specialize label] determines whether it is desirable to create a
   specialized version of the routine labeled [label] for the situation
   where the current state is statically known.

   This function controls the specialization machinery.

   We obey the specialization hint transmitted to us by EmitStackLang. *)

let can_specialize label =
  let tblock = lookup program label in
  match tblock.spec with
  | SpecAllowed ->
      (* If the register [state] does not appear among the registers needed by
         the block labeled [label], then specialization would be pointless, so
         we return [false]. This situation is rare; almost always, the [state]
         register is needed. *)
      Reg.Set.mem state tblock.needed
  | SpecDisallowed ->
      false

(* [can_inline_when_known_state label] determines whether it is permitted (and
   desirable) to inline the routine [label] in a situation where the current
   state is statically known.

   [can_inline_when_known_stack label] determines whether it is permitted (and
   desirable) to inline the routine [label] in a situation where the state
   contained in the top stack cell is statically known.

   This must be used with care (beware of code explosion and divergence!).

   When the current state is known, we inline [goto] routines, which we
   recognize by the inlining hint [OnlyIfKnownState]. In such a situation, a
   [goto] routine degenerates to a single [jump] instruction. We also inline
   [reduce] routines associated with epsilon productions, which we recognize
   by the inlining hint [IfCells 0]. Inlining these [reduce] routines in turn
   allow inlining the [goto] routines that follow them. This guarantees that
   a reduction of an epsilon production never requires a case analysis of
   the current state.

   When the top stack state is known, we inline the routines that carry the
   inlining hint [IfCells 1]. These are the [reduce] routines associated with
   unit productions. Inlining these routines removes the need to specialize
   them for the situation where the top stack cell contains a known tag. *)

let can_inline_when_known_state label =
  let tblock = lookup program label in
  match tblock.hint with
  | Always
  | OnlyIfKnownState
  | IfCells 0 ->
      true
  | IfCells _
  | NoHint ->
      false

let can_inline_when_known_stack label =
  let tblock = lookup program label in
  match tblock.hint with
  | Always
  | IfCells 1 ->
      true
  | IfCells _
  | OnlyIfKnownState
  | NoHint ->
      false

(* The control flow graph (under construction) of the specialized program. *)

let cfg =
  ref Label.Map.empty

(* A queue of [(label, tag)] pairs, waiting to be processed. Each such pair
   represents a request to specialize the block [label] for the state [tag]. *)

let queue : (label * tag) Queue.t =
  Queue.create()

(* A set of the pairs that have already been processed (therefore must not
   be entered again into the queue). *)

module LTSet =
  Set.Make (Order.Pair(Label)(Tag))

let inserted =
  ref LTSet.empty

(* A set of the labels that have been specialized. This set is used for
   counting purposes only. *)

let labels =
  ref Label.Set.empty

(* [enqueue label tag] inserts the pair [(label, tag)] into the queue,
   unless it has already been inserted before. *)

let enqueue label tag =
  let pair = (label, tag) in
  if not (LTSet.mem pair !inserted) then begin
    Queue.add pair queue;
    inserted := LTSet.add pair !inserted;
    labels := Label.Set.add label !labels
  end

(* [spec_label label tag] is the conventional name of the specialized
   copy of the block labeled [label] for the tag [tag]. *)

let spec_label label tag : label =
  sprintf "%s_spec_%s" (Label.export label) (print tag)
  |> Label.import

(* [specialize_block_type tag block_type] specializes the block type for the
   situation where the current state is [tag]. The result is a block type
   that is a *supertype* of the original block type.

   To do so, we take the meet of the original expected stack shape and the
   stack shape that is guaranteed by [tag]. Symmetrically, we take the join
   of the original final and of the final that is associated with [tag].

   If we did not do this, then a PUSH instruction in the block could become
   ill-typed. Indeed, the typing rule for PUSH is more stringent when the
   current state is known than when it is unknown. *)

let spec_block_type tag block_type =
  let tag_type = invariant program tag in
  match
    Invariant.meet block_type.stack tag_type.stack,
    Final.lub block_type.final tag_type.final
  with
  | Some stack, Some final ->
      { stack; final }
  | None, _
  | _, None ->
      (* The meet is undefined. This means that the specialized routine
         that we are attempting to create must be dead! Yet, this seems
         impossible, since we perform specialization only on demand and
         only in reachable code -- that is, there is a call site where
         this specialized routine is needed. *)
      assert false

(* The specialization environment carries information about the stack
   and about the [state] register. It is carried down while a block
   is being transformed. Its eventual purpose is to allow on-the-fly
   simplification of [casetag] instructions and replacement of [jump]
   instructions with jumps to specialized routines.

   Because our main intention is to enable Unit Production Elimination
   (UPE), the information that we maintain about the stack is limited
   to depth 1. All we care about is to know whether the top stack cell
   contains a known state. *)

type env = {

  (* If [stack] is [Some tag], then the top stack cell definitely
     contains the tag [tag] (in its first field). Otherwise, nothing
     is known about the stack. *)
  stack: tag option;

  (* If [state] is [Some tag], then the [state] register definitely
     contains the tag [tag]. Otherwise, nothing is known about this
     register. *)
  state: tag option;

  (* Are we inside a CASEtag? This information is used to turn off
     constant propagation inside the branches of a [goto] routine. *)
  incasetag: bool;

}

(* A debugging printer. *)

let print_tag_option otag =
  match otag with
  | None ->
      "<unknown>"
  | Some tag ->
      Tag.print tag

let print_env env =
  sprintf "stack: %s; state: %s"
    (print_tag_option env.stack)
    (print_tag_option env.state)

(* [apply env v] transforms [env.state] into a substitution, which it applies
   to the value [v]. If [env.state] is [None], this substitution has no
   effect. If [env.state] is [Some tag], this substitution replaces the
   [state] register with the value [VTag tag]. Thus, in addition to
   specialization, we perform a little bit of constant propagation. This is
   cheap and produces slightly more aesthetic code. (Indeed, once all uses of
   [state] have been replaced with the constant [tag], the instruction [DEF
   state <- tag] can disappear as well.) *)

let apply env v =
  match env.state with
  | None ->
      v
  | Some tag ->
      let bs = Bindings.assign [PReg state] [VTag tag] in
      Bindings.apply bs v

(* [materialize env block] materializes the information in [env.state] into
   either nothing or an instruction of the form [DEF state <- tag]. This is
   used immediately before a JUMP instruction, and is part of the constant
   propagation machinery described above. *)

(* This transformation is disabled inside CASEtag instructions, because its
   effect there is to replace the variable [state] with a constant, thus
   making every branch look different, whereas in reality some branches may
   be identical (and could be shared, if the OCaml type-checker allows it). *)

let materialize env block =
  match env.state, env.incasetag with
  | None, _
  | _, true ->
      block
  | Some tag, false ->
      let bs = Bindings.assign [PReg state] [VTag tag] in
      Block.def bs block

(* Specialization. *)

let rec spec_block env block =
  match block with

  | IPush (vs, cell, block) ->
      let env, vs =
        if Value.occurs state vs then begin
          (* We are pushing the [state] register onto the stack. *)
          assert (Invariant.holds_state cell);
          assert (vs <> [] && List.hd vs = VReg state);
          { env with stack = env.state },
          map (apply env) vs
        end
        else
          (* Nothing is known about the new top stack cell. *)
          { env with stack = None }, vs
      in
      IPush (vs, cell, spec_block env block)

  | IPop (ps, cell, block) ->
      let env =
        if Pattern.occurs state ps then begin
          (* We are popping the [state] register off the stack. *)
          assert (ps <> [] && List.hd ps = PReg state);
          { env with stack = None; state = env.stack }
        end
        else
          (* Nothing is known about the new top stack cell. *)
          { env with stack = None }
      in
      IPop (ps, cell, spec_block env block)

  | IPeek (ps, cell, block) ->
      assert (not (Pattern.occurs state ps));
      IPeek (ps, cell, spec_block env block)

  | IDef (bs, block) ->
      let env =
        match Bindings.apply bs (VReg state) with
        | VTag tag ->
            (* We are assigning a known value to the [state] register. *)
            { env with state = Some tag }
        | VReg r' ->
            assert (state = r');
            (* We are assigning some other register. *)
            env
        | _ ->
            assert false
      in
      IDef (bs, spec_block env block)

  | IJump label ->
      (* If the current state is known to be [tag], and if the block [label]
         can be specialized for this tag, then enqueue a specialization
         request (if necessary) and replace this instruction with a jump to
         the specialized block. *)
      if env.state <> None && can_inline_when_known_state label then
        let tag = Option.force env.state in
        info "(spec) Inlining %s (state = %s)" (Label.export label) (print tag)
        (spec_block env (lookup program label).block)

      else if env.stack <> None && can_inline_when_known_stack label then
        let tag = Option.force env.stack in
        info "(spec) Inlining %s (stack = %s)" (Label.export label) (print tag)
        (spec_block env (lookup program label).block)

      else if env.state <> None && can_specialize label then begin
        let tag = Option.force env.state in
        enqueue label tag;
        info "(spec) Specializing %s (state = %s)" (Label.export label) (print tag)
        (IJump (spec_label label tag))
      end

      else
        info "(spec) Cannot inline or specialize (%s)" (print_env env)
        (materialize env (IJump label))

  | ICaseTag (r, branches) ->
      assert (r = state);
      begin match env.state with
      | Some tag ->
          (* The current state is known to be [tag]. Eliminate this [casetag]
             construct. *)
          let block = Block.select_branch tag branches in
          info "(spec) Eliminating casetag (state = %s)" (print tag)
          (spec_block env block)
      | None ->
          (* The [casetag] construct cannot be eliminated. We can (and should)
             still transform its branches. *)
          ICaseTag (r, List.concat (List.map (spec_casetag_branch env) branches))
      end

  | IPrim _
  | ITrace _
  | IComment _
  | IDead _
  | IStop _
  | IReturn _
  | ICaseToken _
    ->
      Block.map (spec_block env) block

and spec_casetag_branch env (TagSingle tag, body) =
  (* In this branch, we learn that the current state is [tag]. *)
  let env = { env with state = Some tag; incasetag = true } in
  [ TagSingle tag, spec_block env body ]

let spec_tblock tag tblock =
  assert (Reg.Set.mem state tblock.needed);
  (* The current state is initially assumed to be [tag].
     Nothing is initially assumed about the stack. *)
  let env = { stack = None; state = Some tag; incasetag = false } in
  (* Specialize this block. *)
  let block = spec_block env tblock.block in
  (* Insert a definition of the register [state] at the beginning. This
     ensures that this block does not take [state] as a parameter. If
     our constant propagation has the desired effect, this definition
     should be dead anyway. *)
  let block = IDef (Bindings.assign [PReg state] [VTag tag], block) in
  (* Remove [state] from the set of needed registers. Better do it
     explicitly, even though we later recompute the needed registers
     by calling [NeededRegisters.update]. *)
  let needed = Reg.Set.remove state tblock.needed in
  (* Specialize the type of this block. *)
  let block_type = spec_block_type tag tblock.block_type in
  (* Done. *)
  { tblock with block; block_type; needed }
  (* The specialized routine carries the same
     inlining hint as the original routine. *)

let spec_routine (label, tag) =
  let block = spec_tblock tag (lookup program label) in
  cfg := Label.Map.add (spec_label label tag) block !cfg

let inspect_tblock tblock =
  (* Nothing is initially assumed. *)
  let env = { stack = None; state = None; incasetag = false } in
  (* Inspect and transform this block. *)
  let block = spec_block env tblock.block in
  { tblock with block }

let inspect_routine _label tblock =
  inspect_tblock tblock

let program =
  cfg := Label.Map.mapi inspect_routine program.cfg;
  Misc.qiter spec_routine queue;
  { program with cfg = !cfg }

let () =
  Error.logC 1 (fun f ->
    fprintf f "%d specialized copies of %d functions have been created.\n"
      (LTSet.cardinal !inserted)
      (Label.Set.cardinal !labels)
  )

end (* SpecializeState *)

let specialize_state program =
  let module S = SpecializeState(struct let program = program end) in
  Time.tick "StackLang: specialization with respect to the state";
  S.program

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Commuting PUSH instructions. *)

module CommutePushes (X : sig val program : program end) = struct open X

(* We maintain a set of discovered labels and a FIFO queue of labels that
   have been discovered but not yet processed. *)

let discovered : Label.Set.t ref =
  ref Label.Set.empty

let waiting : label Queue.t =
  Queue.create()

(* [discover label] marks the label [label] as discovered and inserts it into
   the waiting queue if was not already discovered. *)

let discover label =
  if not (Label.Set.mem label !discovered) then begin
    discovered := Label.Set.add label !discovered;
    Queue.add label waiting
  end

(* We move trains (sequences) of PUSH instructions forward in the code.
   A wagon (a single PUSH instruction) contains the values [vs] and the
   cell [cell] that appear in the PUSH instruction. *)

type wagon =
  values * cell

(* A train is a list of wagons. The head of the list is the instruction
   that appears at the end of the instruction sequence. *)

type train =
  wagon list

(* [pushes_vanish block] determines whether a PUSH instruction, in front
   of the block [block], can vanish. This is the case if [block] contains
   a sequence of DEF, PUSH, TRACE instructions that ends with DEAD or STOP. *)

(* This information is exploited by [materialize], which itself is used in
   [pad_branch]. *)

let rec pushes_vanish block =
  match block with
  | IDead _
  | IStop _ ->
      true
  | IComment (_, block)
  | IDef (_, block)
  | IPush (_, _, block)
  | ITrace (_, block) ->
      pushes_vanish block
  | _ ->
      false

(* [materialize pushes block] re-creates the PUSH instructions described by
   the list [pushes] in front of the block [block]. *)

let materialize (pushes : train) block =
  if pushes_vanish block then
    info "(optm) Some push instructions have vanished here"
    block
  else
    List.fold_left (fun block (vs, cell) ->
      info "(optm) Re-materializing push"
      (IPush (vs, cell, block))
    ) block pushes

(* These auxiliary functions determine whether the register [r] occurs
   in a wagon and in a train. *)

let occurs_in_wagon r wagon =
  let (vs, _cell) = wagon in
  Value.occurs r vs

let occurs_in_train r wagons =
  List.exists (occurs_in_wagon r) wagons

(* [current_state_is_known bs] determines whether, according to the
   bindings [bs], the current value of the [state] register is known. *)

let current_state_is_known bs =
  match Bindings.apply bs (VReg state) with
  | VTag _ ->
      true
  | VReg _ ->
      false
  | _ ->
      assert false

(* We are pushing forward a train of PUSH instructions followed with a DEF
   instruction, that is, [PUSH*; DEF]. The PUSH instructions are represented
   by a list [pushes], where the head of the list is the rightmost PUSH, the
   most recent PUSH. The DEF instruction carries a set of bindings [bs]. *)

(* The information that is carried down during the transformation is gathered
   in an environment. *)

type env = {

  path: Label.Set.t;
  (** The path [path] records the labels of the routines that we have decided
      to inline. It is used to detect and forbid cycling inlining. *)

  pushes: train;
  (** [pushes] is a train of PUSH instructions that is being pushed forward. *)

  bs: bindings;
  (**[bs] is a set of bindings that is also being pushed forward. *)

  fresh: unit -> int;
  (**[fresh] is a generator of fresh integers. *)

}

(* We must sometimes rename a register in order to avoid a name clash.
   We add a fresh numeric suffix (delimited with an underscore) to the
   register's original name. *)

let rename env (r : register) : register =
  Reg.import (sprintf "%s_%d" (Reg.export r) (env.fresh()))

(* [transform_assignment_to_reg env r] moves the PUSH train [env.pushes] and
   the bindings [env.bs] past an assignment of the register [r]. It returns
   a pair of an updated environment and a possibly renamed register. *)

let transform_assignment_to_reg env r =
  if occurs_in_train r env.pushes then
    (* [r] appears in the train. To avoid a name clash, we rename [r] to [r'],
       so the assignment of [r] becomes an assignment of [r'] instead, and we
       we add an assignment [DEF r <- r'] to the traveling bindings [bs], so
       that the code that follows is not affected. *)
    let r' = rename env r in
    let bs' = Bindings.(seq env.bs (assign [PReg r] [VReg r'])) in
    { env with bs = bs' }, r'
  else
    (* [r] does not appear in the train. Thus, the train can safely move past
       an assignment of [r]. We simply have to remove [r] from the domain of
       the bindings [bs], since the new assignment hides any previous binding. *)
    let bs = Bindings.remove env.bs (Reg.Set.singleton r) in
    let env = { env with bs } in
    env, r

let transform_assignment_to_pat env p =
  match p with
  | PReg r ->
      let env, r = transform_assignment_to_reg env r in
      env, PReg r
  | PWildcard ->
      (* No renaming is necessary. *)
      env, p

let transform_assignment_to_tokpat env tokpat =
  match tokpat with
  | TokSingle (tok, r) ->
      let env, r = transform_assignment_to_reg env r in
      env, TokSingle (tok, r)
  | TokMultiple _ ->
      (* No renaming is necessary. *)
      env, tokpat

(* [do_not_transform_block] is a trivial function that satisfies the same
   specification as [transform_block] (see below). It consumes zero PUSH
   instructions, achieves zero happiness, and leaves [DEF bs; block]
   unchanged. *)

let do_not_transform_block env block : int * int * block =
  Block.successors discover block;
  0, 0, IDef (env.bs, block)

(* [transform_block env block] attempts to move the sequence [PUSH pushes;
   DEF bs] into the block [block]. There is no obligation to use all of the
   PUSH instructions in the list [pushes]: instead, [transform_block]
   returns an integer [k] that indicates how many PUSH instructions were
   used. It also returns an integer happiness level and (of course) a
   transformed block [block'].

   If [pushes'] is defined as [drop k pushes], then [PUSH pushes'; block']
   must be semantically equivalent to [PUSH pushes; DEF bs; block].

   It is possible to achieve nonzero happiness even without absorbing any
   PUSHES: thus, [k = 0 && happiness > 0] is possible. On the other hand, we
   ensure that [k > 0 && happiness = 0] is impossible; that is, we absorb
   one or more PUSH instructions only if this helps make us happy. Thus,
   [k > 0] implies [happiness > 0], and [happiness = 0] implies [k = 0]. *)

let rec transform_block env block : int * int * block =
  match block with

  | IPush (_vs, _cell, block) when pushes_vanish block ->
      (* As a special case, if the block that follows is so trivial that PUSH
         instructions in front of it vanish, then we are done. We drop the
         PUSH instructions (and transform the block, which should result in no
         transformation). We include this special rule because STOP reports
         that it does not want to absorb any PUSHes (see below). Thus, this
         rule remains necessary in order to transform PUSH; STOP into STOP. *)
      kinfo "(optm) Some push instructions have vanished here"
      (do_not_transform_block env block)

  | IPush (vs, cell, block) ->
      (* [DEF bs; PUSH vs] is equivalent to [PUSH vs'; DEF bs], where [vs'] is
         the result of applying the bindings [bs] to the values [vs]. *)
      let vs = map (Bindings.apply env.bs) vs in
      (* Add this wagon to the train and continue. *)
      let wagon = (vs, cell) in
      let pushes = wagon :: env.pushes in
      let env = { env with pushes } in
      let k, happiness, block = transform_block env block in
      (* [k] PUSH instructions have been absorbed into [block]. If [k] is zero,
         then this PUSH instruction should remain here; otherwise, it has been
          absorbed. Either way, the happiness level is transmitted. *)
      if k = 0 then
        0,
        happiness,
        info "(optm) Not moving this push instruction"
        (IPush (vs, cell, block))
      else
        (* This is where we may report zero absorbed PUSHes but nonzero
           happiness. *)
        k - 1,
        happiness,
        info "(optm) A push instruction was here"
        block

  | IPop (ps, cell, block) ->
      transform_pop env ps cell block

  | IPeek _ ->
      (* We expect PEEK to be rarely used, and we expect it to be used in a
         specific context (namely, a combined [action/goto] routine) which a
         PUSH train cannot enter. So, there is no need to be smart here. The
         train, if there is one, stops here. *)
      do_not_transform_block env block

  | IDef (bs', block) ->
      (* The train [PUSH*; DEF bs], which we are pushing forward, reaches an
         instruction of the form [DEF bs']. By constructing the sequential
         composition of [bs] and [bs'], we absorb this DEF instruction and
         obtain a new train of the form [PUSH*; DEF]. *)
      let bs = Bindings.seq env.bs bs' in
      let env = { env with bs } in
      transform_block env block

  | IPrim (p, prim, block) as original_block ->
      let original_env = env in
      (* This instruction assigns the pattern [p]. *)
      let env, p = transform_assignment_to_pat env p in
      (* The bindings [bs] are applied to the primitive operation. *)
      let prim = Primitive.apply env.bs prim in
      (* There remains to transform the remainder of the block. *)
      let k, happiness, block = transform_block env block in
      if happiness > 0 then
        (* We could always return this... *)
        k, happiness, IPrim (p, prim, block)
      else begin
        assert (k = 0);
        (* ... but, as a special case, if [happiness] is zero, which means
           that the remainder of the block was not transformed in a useful
           way, then we prefer to use [do_not_transform_block], so as to
           avoid the use of [Primitive.apply], which creates local bindings
           in front of a semantic action. This is purely aesthetic. This
           special case is the common case: we do not usually expect a PUSH
           instruction to be able to travel past a semantic action. *)
        do_not_transform_block original_env original_block
      end

  | ITrace (t, block) ->
      let k, happiness, block = transform_block env block in
      k, happiness, ITrace (t, block)

  | IComment (comment, block) ->
      let k, happiness, block = transform_block env block in
      k, happiness, IComment (comment, block)

  | IDead phase ->
      0, 0, IDead phase

  | IStop s ->
      (* [PUSH*; DEF; STOP] is equivalent to [STOP]. Thus, STOP is able to
         absorb an arbitrary number of PUSH instructions. However, we do not
         wish to return the integer [length pushes], because that would mean
         that we *want* to absorb all of them, and when STOP appears in a
         branch of a CASE construct, that would imply that all available PUSH
         instructions *must* enter the CASE construct. In reality, STOP does
         not care how many PUSHes enter the CASE construct; it will absorb all
         of them, anyway. So, we artificially return 0, so as to not influence
         the [max] computation that takes place at CASE constructs. *)
      0, 0, IStop s

  | IReturn (nt, v) ->
      (* We certainly have no PUSH instructions, because the parser accepts
         only when the stack is empty. We materialize just [DEF bs] and
         merge it into the RETURN instruction. *)
      assert (env.pushes = []);
      0, 0, IReturn (nt, Bindings.apply env.bs v)

  | IJump label ->

      (* [transform_block] is never applied to a branch in a [casetag]
         construct, so we need not worry about preserving the property that
         every branch of a [casetag] construct must be a jump. *)

      (* In general, we want the [PUSH*; DEF] train to stop here. We do not
         attempt to move it past the JUMP instruction and into the target
         block; that would require agreement between the predecessors of the
         label [label]. *)

      (* In some situations, however, we want to inline the target block. This
         is the case, for instance, when the target block is a [goto] routine
         and the current state is statically known; or when the target block
         is a [reduce] routine and our PUSH train is long enough to cancel all
         of the POP instructions inside it. (The details may vary.) *)

      (* Something one might wish to do is to first speculatively inline, then
         inspect the integer result [k], which tells how many PUSH instructions
         can be absorbed by the inlined routine. If [k] is positive, then
         inlining may be worthwhile. A quick experiment suggests that this is
         too aggressive: on a small grammar, the increase in code size is x4; on
         larger grammars, Menhir does not terminate in a reasonable time. *)

      (* If the target label already appears in the path that we have
         followed, then we cannot inline; we would fall into a cycle. If we
         can and do inline, then we must add this label to the path that we
         have followed. *)

      let do_not_inline format =
        ksprintf (fun s ->
          kinfo "%s" s
          (do_not_transform_block env block)
        ) format
      in

      if Label.Set.mem label env.path then
        do_not_inline "(optm) Cannot inline (cyclic path)"
      else

        let target = lookup program label in

        let do_inline format =
          ksprintf (fun s ->
            let path = Label.Set.add label env.path in
            let env = { env with path } in
            kinfo "%s" s
            (transform_block env target.block)
          ) format
        in

        begin match target.hint with
          | Always ->
              do_inline "(optm) Inlining %s (always inlined)" (Label.export label)
          | OnlyIfKnownState ->
              if current_state_is_known env.bs then
                do_inline "(optm) Inlining %s (known state)" (Label.export label)
              else
                do_not_inline "(optm) Cannot inline (unknown state)"
          | IfCells k ->
              let h = length env.pushes in
              (* If we have [k] PUSHes or more, then inline this routine. This
                 guarantees, in particular, that [reduce] routines for epsilon
                 productions are always inlined. Inlining when [k] cells are at
                 hand is good, because in this case, the [goto] routine that
                 follows can be inlined as well, and the CASEtag instruction can
                 be simplified. *)
              if k <= h then
                do_inline "(optm) Inlining %s (%s)" (Label.export label) (cells k)
                (* Even if we do not have [k] PUSHes, as soon as we have at
                   least one PUSH, we inline anyway; this allows at least one
                   PUSH/POP elimination. *)
              else if 0 < h then
                do_inline "(optm) Inlining %s (%s at hand, %s ideally needed)"
                  (Label.export label) (cells h) (cells k)
              else
                do_not_inline "(optm) Cannot inline (%s needed, %s at hand)"
                  (cells k) (cells h)
          | NoHint ->
              do_not_inline "(optm) Cannot inline (no hint)"
        end

  | ICaseToken (r, branches, odefault) ->
      transform_casetok env r branches odefault

  | ICaseTag (r, branches) ->
      assert (r = state);
      transform_casetag env branches

and transform_pop env ps cell block : int * int * block =

  (* We begin by restricting [bs] so that no names are bound both in [bs] and
     in [ps]. Clearly, this is correct: if a name is bound in [bs] and in [ps],
     then the binding in [bs] is useless and can be removed. *)

  let bs = Bindings.remove env.bs (Pattern.registers ps) in
  let env = { env with bs } in

  (* Determine whether the PUSH train is empty or nonempty. *)
  match env.pushes with

  | [] ->
      (* Our train of PUSH instructions is unfortunately empty, so this POP
         instruction cannot be cancelled. The remainder of the block is
         transformed. *)
      let k, happiness, block = transform_block env block in
      assert (k = 0);
      k, happiness, IPop (ps, cell, block)

  | (vs, _cell) :: pushes ->
      assert (Invariant.similar cell _cell);
      (* Our train of PUSH instructions is nonempty; the rightmost instruction
         in it is [PUSH vs]. Thus, we are looking at [PUSH vs; DEF bs; POP ps].

         Because no name is bound both in [bs] and in [ps], one might think
         that [DEF bs] and [POP ps] commute. This is not true: some registers
         in the codomain of [bs] could be defined by [ps]. Instead, we remark
         that [PUSH vs; DEF bs; POP ps] is equivalent to [DEF (bs || ps := vs)].
         Indeed, it is easy to check that these sequences have the same effect
         on the registers in [bs] and on the registers in [ps]. *)

      let bs = Bindings.(par bs (assign ps vs)) in
      let env = { env with pushes; bs } in

      (* We consume one wagon from the train, and propose the remainder of
         the train for use in the transformation of the remainder of the
         block. If this transformation consumes [k] wagons, then [k+1]
         wagons have been consumed in total. Because we absorb at least
         one PUSH instruction, we may report nonzero happiness. *)
      let k, happiness, block = transform_block env block in

      k + 1,
      happiness + 1,
      info "(optm) Cancelled push %s with pop %s"
        (Print.values vs)
        (Print.patterns ps)
      block

and transform_casetag env branches : int * int * block =
  match Bindings.apply env.bs (VReg state) with
  | VTag tag ->
      (* The bindings [bs] assign a known value to the register [state].
         This allows us to statically reduce this [casetag] construct. This
         is good! We bump our happiness level to indicate that we are happy.
         This probably makes no difference (indeed, if we reach this point,
         then we have probably successfully eliminated PUSH/POP pairs
         already, so our happiness level is already nonzero), but let's do
         it, just in case. *)
      let block = Block.select_branch tag branches in
      kinfo "(optm) Eliminating casetag"
      (happily (transform_block env block))
  | VReg r' ->
      assert (r' = state);
      (* The bindings [bs] do not allow reducing this [casetag] construct,
         which must therefore be preserved. We cannot allow the PUSH train to
         enter it, because the branches of a [casetag] construct must remain
         small; ideally, they should be jumps. So, the train stops here. *)
      do_not_transform_block env (ICaseTag (state, branches))
  | _ ->
      assert false

and happily (k, happiness, block) =
  k, happiness + 1, block

and transform_casetok env r branches odefault : int * int * block =
  (* As a simplifying assumption, we assume that the bindings in [bs] do not
     apply to the register [r]. This is true, at the moment, because [r] must
     be the [token] register and this register is never written by a DEF
     instruction. *)
  assert (not (Bindings.mem r env.bs));
  (* Propose to send the PUSH train down into every branch. For every
     branch, this yields a triple of an integer number of absorbed PUSHes, a
     happiness level, and a transformed branch. *)
  let branches = map (transform_casetok_branch env r) branches
  and odefault = Option.map (transform_block env) odefault in
  (* Compute the total happiness. *)
  let add2 (_, happiness, _) accu = happiness + accu in
  let happiness = List.fold_right add2 branches 0 in
  let happiness = Option.fold add2 odefault happiness in
  (* Compute the maximum number of PUSHes that are absorbed by a branch.
     This is the number of PUSHes that we must move into the [casetok]
     construct. It is therefore the number of PUSHes that we absorb. *)
  let max1 (j, _, _) accu = max j accu in
  let k = List.fold_right max1 branches 0 in
  let k = Option.fold max1 odefault k in
  assert (k <= List.length env.pushes);
  (* Keep only the PUSHes that we wish to absorb. *)
  let pushes = take k env.pushes in
  (* Now, pad each branch. Each branch has already absorbed [j] PUSHes,
     where [j <= k] holds. We must therefore move the remaining [k-j]
     PUSHes into this branch. *)
  let branches = map (pad_branch pushes) branches
  and odefault = Option.map (pad_default pushes) odefault in
  k, happiness, ICaseToken (r, branches, odefault)

and transform_casetok_branch env _r branch : int * int * tokbranch =
  let tokpat, block = branch in
  (* Rename the pattern if necessary. *)
  let env, tokpat = transform_assignment_to_tokpat env tokpat in
  (* Transform the body of this branch. *)
  let j, happiness, block = transform_block env block in
  j, happiness, (tokpat, block)

and pad_branch pushes (j, _happiness, (tokpat, block)) : tokbranch =
  (* This branch has absorbed [j] PUSHes out of the list [pushes], so
     the remainder of the list must be re-materialized at the beginning
     of the branch. *)
  tokpat,
  materialize (drop j pushes) block

and pad_default pushes (j, _happiness, block) : block =
  (* Same as above, with an implicit wildcard pattern. *)
  materialize (drop j pushes) block

let transform_tblock label tblock =
  let path = Label.Set.singleton label in
  let pushes = []
  and bs = Bindings.empty
  and fresh = Misc.mkgensym() in
  let env = { path; pushes; bs; fresh } in
  let k, _happiness, block = transform_block env tblock.block in
  assert (k = 0);
  { tblock with block }

(* Initialization and main loop. *)

let program =
  (* Create an empty new control flow graph. *)
  let cfg = ref Label.Map.empty in
  (* Insert the entry labels into the queue. *)
  program.entry |> StringMap.iter begin fun _ label ->
    discover label
  end;
  (* Process the waiting labels. *)
  waiting |> Misc.qiter begin fun label ->
    assert (Label.Set.mem label !discovered);
    assert (not (Label.Map.mem label !cfg));
    let tblock = lookup program label in
    let tblock = transform_tblock label tblock in
    cfg := Label.Map.add label tblock !cfg;
  end;
  (* Done. *)
  { program with cfg = !cfg }

end (* CommutePushes *)

let commute_pushes program =
  let module CP = CommutePushes(struct let program = program end) in
  Time.tick "StackLang: moving PUSHes";
  CP.program

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* [in_degree program] computes the in-degree of every label in the program
   [program]. The result is a table that maps every reachable label to its
   in-degree. Unreachable labels do not appear in the table. *)

(* Every entry label artificially receives an in-degree of at least 2. Every
   edge that originates in a [casetag] construct also confers in-degree 2,
   so as to forbid inlining this edge. *)

let in_degree program =

  (* Initialize a queue and a map of labels to degrees. *)
  let queue  : label Queue.t = Queue.create()
  and degree : int Label.Map.t ref = ref Label.Map.empty in

  (* [tick incasetag label] increments the degree associated with [label]. If
     its previous degree was zero, then [label] is enqueued for exploration. *)
  let tick incasetag label =
    let d =
      try
        Label.Map.find label !degree
      with Not_found ->
        Queue.add label queue;
        0
    in
    let delta = if incasetag then 2 else 1 in
    degree := Label.Map.add label (d + delta) !degree
  in

  (* [visit () label] examines the block at address [label]. *)
  let visit () label =
    (lookup program label).block
    |> Block.jumps tick
  in

  (* Initialize the queue with the entry labels. Every entry label is
     inserted into the queue with an in-degree of 2, so it cannot be
     inlined or considered unreachable. *)
  program.entry |> StringMap.iter (fun _name label ->
    Queue.add label queue;
    degree := Label.Map.add label 2 !degree
  );

  (* Process the queue until it  becomes empty. Return the final table. *)
  Misc.qfold visit () queue;
  !degree

(* -------------------------------------------------------------------------- *)

(* Removing unreachable blocks. *)

(* Removing unreachable blocks before attempting to type-check the StackLang
   program can be important. The static analysis of the stack shape and of the
   final type in an unreachable block should in principle yield bottom, but we
   do not have a way of expressing this, so we run a risk of producing
   something else (e.g. an empty stack shape, or an unknown final type). This
   can cause a jump from an unreachable block to a reachable block to be
   considered ill-typed. *)

let remove_unreachable_blocks program =
  let degree = in_degree program in
  let cfg = Label.Map.fold (fun label block accu ->
    if Label.Map.mem label degree then
      Label.Map.add label block accu
    else
      accu
  ) program.cfg Label.Map.empty in
  Time.tick "StackLang: removing unreachable blocks";
  { program with cfg }

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Inlining routines whose in-degree is 1. *)

module Inline (X : sig
  val cautious: bool
  val program : program
end) = struct open X

(* Compute every label's in-degree. *)

let degree =
  in_degree program

(* We maintain a set of discovered labels and a FIFO queue of labels that
   have been discovered but not yet processed. *)

let discovered : Label.Set.t ref =
  ref Label.Set.empty

let waiting : label Queue.t =
  Queue.create()

(* [discover label] marks [label] as discovered and inserts it into
   the waiting queue if was not already discovered. *)

let discover label =
  if not (Label.Set.mem label !discovered) then begin
    discovered := Label.Set.add label !discovered;
    Queue.add label waiting
  end

(* This simple-minded inliner performs conservative inlining only. More
   intelligent and more aggressive forms of inlining are performed by
   [specialize] and [commute_pushes] above. The following function defines
   our inlining policy. [label] and [target] are the labels of the source
   and destination blocks. *)

let can_inline label target =
  match (lookup program target).hint with
  | OnlyIfKnownState ->
      (* This hint disallows inlining. *)
      false
  | Always
  | IfCells _ ->
      (* These hints disallow inlining when [cautious] is true. *)
      not cautious
  | NoHint ->
      (* In cautious mode, inlining is allowed only if both the source
         block and the destination mode carry [NoHint]. *)
      not cautious ||
      (lookup program label).hint = NoHint

(* -------------------------------------------------------------------------- *)

(* The following functions assume that the in-degree of every reachable label
   has been computed (as shown above) and is stored in the table [degree]. *)

(* Transforming a block labeled [label]. We carry [label] around because it
   influences our inlining decisions; see [can_inline label target]. *)

let rec inline_block label block =
  match block with
  | IJump target ->
      (* If [target]'s in-degree is 1 and inlining is permitted, follow the
         indirection; otherwise, keep the [jump] instruction. Note that the
         [target]'s in-degree cannot be 0, since we have reached an edge
         that leads to [target]. *)
      if Label.Map.find target degree = 1
      && can_inline label target then
        (* The type information associated with the target block is lost:
           we do not have a construct for inserting a type annotation in
           the middle of a block. *)
        info "(inlg) Inlining %s (in-degree 1, cautious = %b)"
          (Label.export target) cautious
        (inline_block label (lookup program target).block)
      else begin
        discover target;
        IJump target
      end
  | ICaseTag _ ->
      (* Do not inline anything into a [casetag] construct. *)
      Block.successors discover block;
      block
  | _ ->
      Block.map (inline_block label) block

let inline_tblock label tblock =
  { tblock with block = inline_block label tblock.block }

(* -------------------------------------------------------------------------- *)

(* Transforming a control flow graph. *)

let program =
  (* Create an empty new control flow graph. *)
  let cfg = ref Label.Map.empty in
  (* Insert the entry labels into the queue. *)
  program.entry |> StringMap.iter begin fun _ label ->
    discover label
  end;
  (* Process the waiting labels. *)
  waiting |> Misc.qiter begin fun label ->
    assert (Label.Set.mem label !discovered);
    assert (not (Label.Map.mem label !cfg));
    let tblock = lookup program label in
    cfg := Label.Map.add label (inline_tblock label tblock) !cfg
  end;
  (* Done. *)
  { program with cfg = !cfg }

let () =
  Time.tick "StackLang: inlining"

end

let inline cautious program =
  let module I = Inline(struct
    let cautious = cautious
    let program = program
  end) in
  I.program

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Specialization with respect to the current token. *)

(* The purpose of this program transformation is to ensure that each token
   is examined at most once. That is, we wish to ensure that no path in the
   code goes through two [CASEtok] instructions on the same token.

   To achieve this, immediately after a new token is read from the lexer, we
   insert a [CASEtok] instruction with suitable branches. Then, within each
   branch, we specialize the code under the knowledge of the current token,
   so that all [CASEtok] instructions vanish.

   Of course, in the [CASEtok] instructions that we insert, determining
   which distinctions are needed requires an analysis. Creating one branch
   per terminal symbol would be too naive: many branches would be identical
   and the code size would explode. We must distinguish two terminal symbols
   only if they are treated differently at some point in the program. The
   required analysis can be formulated as a partition refinement problem. *)

module SpecializeToken (X : sig val program : program end) = struct open X

(* This flag controls internal debugging output. *)

let debug =
  false

(* We use the partition refinement algorithm offered by Fix. *)

open Fix (* Numbering, NUMBERING, Indexing, Minimize *)
type 'a enum = 'a Enum.enum
let (enum, foreach, singleton, map) = Enum.(enum, foreach, singleton, map)

(* Let us write [T] as a short-hand for [Grammar.Terminal]
   and [TSet] as a short-hand for [Grammar.TerminalSet]. *)

module T    = Grammar.Terminal
module TSet = Grammar.TerminalSet

(* -------------------------------------------------------------------------- *)

(* We assume that the [token] register is used by the StackLang program
   to hold the lookahead token. Thus, it appears in [LexerCall] and
   [CASEtok] instructions. This assumption is not essential, but
   simplifies things a little. *)

let token =
  EmitStackLang.token

(* We use the register [tokv] to temporarily hold the semantic value
   carried by a token when we analyze this token. A fresh name is
   used here. We should also be able to re-use the register [semv]
   but that would require a little more thought. *)

let tokv : register =
  Reg.import "_tokv"

(* This auxiliary function expresses an assertion. *)

let assert_not_sensitive regs =
  assert (not (Reg.Set.mem token regs));
  assert (not (Reg.Set.mem tokv  regs))

(* The pattern [tokpat toks] matches the set of tokens [toks]. If this set
   is a singleton set, then this pattern also causes the token's semantic
   value to be written to the register [tokv]. *)

let tokpat toks =
  if TSet.cardinal toks = 1 then
    TokSingle (TSet.choose toks, tokv)
  else
    TokMultiple toks

(* -------------------------------------------------------------------------- *)

(* A block can be (and is) specialized if and only if the [token] register
   appears among its needed registers. *)

let can_specialize_tblock tblock =
  Reg.Set.mem token tblock.needed

let can_specialize label =
  let tblock = lookup program label in
  can_specialize_tblock tblock

(* -------------------------------------------------------------------------- *)

(* The partition refinement problem that we must solve is equivalent to
   minimizing a deterministic finite-state automaton (DFA), which can be
   described as follows.

   A state of the automaton is either
   - a pair [(label, None)],
     where [can_specialize label] is false; or
   - a pair [(label, Some t)],
     where [can_specialize label] is true
     and [t] is a terminal symbol.

   We make the simplifying assumption that *in the transformed code* there is
   at most one [CASE] instruction per block, which can be either a [CASEtag]
   instruction or a [CASEtok] instruction. This allows us to give a simple
   definition of the type [condition], involving zero or one jumps, never more
   than one. Because a lexer call in the source code gives rise to a [CASEtok]
   instruction in the transformed code, and because a [CASEtok] instruction in
   the source code disappears, this amounts to assuming that *in the original
   code* a block never contains both a lexer call and a [CASEtag] instruction.
   (I think.) The code produced by EmitStackLang respect this restriction.
   Inlining could violate it.

   Thanks to this restriction, a jump from one block to another block
   is either unconditional (i.e., not inside a [CASE] instruction) or
   conditional (i.e., inside a [CASEtag] or [CASEtok] instruction).
   The type [condition], defined below, encodes this information.

   Furthermore, we assume that *in the transformed code* [DEAD], [STOP], and
   [RET] instructions cannot be used inside a [CASEtok] instruction. This
   amounts to assuming that *in the original code* [DEAD], [STOP] and [RET]
   instructions cannot appear in the same block as a lexer call. (I think.)
   Thanks to this restriction, we do not need to impose constraints on
   equivalence classes due to the presence of [DEAD], [STOP] or [RET]
   instructions. The information contained in block labels and in jumps from
   block to block is sufficient to prevent confusing a situation that leads
   to [STOP] and a situation that leads to [RET].

   A transition of the automaton has
   - a source state,
   - a target state,
   - and is labeled with a condition,
     which indicates under what circumstances this transition is taken.

   The problem is then to decide which states in this automaton can be
   considered equivalent, under the following constraints:

   C1- if two states [(label1, ot1)] and [(label2, ot2)] are equivalent
       then [label1 = label2]
       and either [ot1 = ot2 = None]
               or [ot1 = Some _] and [ot2 = Some _];

   C2- if two states [(label, Some t1)] and [(label, Some t2)] are equivalent
       and if the block [label] contains a [CASEtok] instruction then this
       instruction does not distinguish the terminal symbols [t1] and [t2];

   C3- equivalence must be compatible with the transitions of the automaton;
       that is, if two states [s1] and [s2] are equivalent and if [s1] has an
       outgoing transition labeled [condition] towards [s'1] then [s2] has an
       outgoing transition labeled [condition] towards some state [s'2] such
       that [s'1] and [s'2] are equivalent.

   Constraint C1 implies that two specialized blocks can be considered
   equivalent (and fused) only if they are specialized copies of the same
   source block. *)

(* In the type definitions that follow, we use the names [vertex] for states
   and [edge] for transitions. The names [state] and [transition] are later
   used for integer indices that denote states and transitions. *)

type vertex =
  label * terminal option
      (* option is [None] iff [can_specialize label] is [false] *)

type condition =
  | CondNone
  | CondTag of tag
  | CondTok of terminal

type edge =
  vertex * condition * vertex

(* The types [label], [terminal], and [tag] can be hashed (they are just
   integers), so the types [vertex], [condition] and [edge] can be hashed,
   too. It is brittle but convenient to rely on this property. We exploit
   this property when we use [Numbering.ForType] below. *)

(* Debugging printers. *)

let print_condition condition =
  match condition with
  | CondNone ->
      sprintf "CondNone"
  | CondTag tag ->
      sprintf "CondTag %s" (Tag.print tag)
  | CondTok t ->
      sprintf "CondTok %s" (T.print t)

let _ =
  print_condition

(* -------------------------------------------------------------------------- *)

(* The minimization algorithm requires a total order on conditions
   and a printer for conditions (for debugging purposes only). *)

module Condition = struct
  type t = condition
  let compare = Generic.compare (* again brittle but convenient *)
  let print condition =
    match condition with
    | CondNone ->
        "CondNone"
    | CondTag tag ->
        sprintf "CondTag %s" (Tag.print tag)
    | CondTok t ->
        sprintf "CondTok %s" (T.print t)
end

(* -------------------------------------------------------------------------- *)

(* An enumeration of all vertices. *)

let vertex : vertex enum =
  enum @@ fun yield ->
    program.cfg |> Label.Map.iter @@ fun label _tblock ->
      if can_specialize label then
        T.iter_real @@ fun t -> yield (label, Some t)
      else
        yield (label, None)

(* An enumeration of all initial vertices, that is, of all vertices that
   represent entry points. The minimization algorithm throws away all
   unreachable vertices, so it is important to provide this information. *)

let initial_vertex : vertex enum =
  enum @@ fun yield ->
    program.entry |> StringMap.iter @@ fun _ label ->
      assert (not (can_specialize label));
      yield (label, None)

(* [multi label toks] is an enumeration of all vertices [(label, Some t)]
   where [t] ranges over the set [toks]. *)

let multi label toks : vertex enum =
  enum @@ fun yield ->
    toks |> TSet.iter @@ fun t ->
      yield (label, Some t)

(* An enumeration of the equivalence classes that exist before the partition
   refinement algorithm runs. The distinctions that we impose at this stage
   encode constraints C1- and C2- in our specification of what vertices may
   be considered equivalent. *)

let groups : vertex enum enum =
  enum @@ fun yield ->
    program.cfg |> Label.Map.iter @@ fun label tblock ->
      if not (can_specialize label) then
        (* This block cannot be specialized. *)
        (* This equivalence class is a singleton. *)
        yield (singleton (label, None))
      else
        (* This block can be specialized. *)
        match has_case_token tblock.block with
        | None ->
            (* This block does not contain a [CASEtok] instruction. *)
            (* Thus, it imposes no distinction among the terminal symbols. *)
            yield (multi label TSet.universe)
        | Some (branches, odefault) ->
            (* This block contains a [CASEtok] instruction. *)
            (* Thus, it distinguishes certain classes of terminal symbols:
               one class per branch, including the default branch, if there
               is one. *)
            List.iter (fun (tokpat, _) ->
              yield (multi label (tokens tokpat))
            ) branches;
            if odefault <> None then
              yield (multi label (default_tokens branches))

let () =
  if debug then
    eprintf "DEBUG: %d equivalence classes before partition refinement\n%!"
      (Enum.length groups)

(* -------------------------------------------------------------------------- *)

(* Number the vertices. *)

module Vertex : NUMBERING with type t = vertex = struct
  module N = Numbering.ForType(struct type t = vertex end)
  let () = foreach vertex (fun v -> ignore (N.encode v))
  include N.Done()
    (* This defines [n], [encode], [decode]. *)
end

let () =
  if debug then
    eprintf "DEBUG: %d vertices\n%!" Vertex.n

(* -------------------------------------------------------------------------- *)

(* [walk jump env condition block] walks the block [block], anticipating on
   the manner in which this block will be transformed, and enumerates the
   outgoing edges of the transformed block.

   The function [jump] is used to signal the presence of each outgoing edge.
   The environment [env] indicates whether (in the transformed block) the
   current token is currently known. The parameter [condition] keeps track
   of the branches that have been taken since the transformed block was
   entered. When an edge is found, this condition serves as the label of
   this edge. *)

let rec walk
  (jump : condition -> vertex -> unit)
  (env : terminal option)
  (condition : condition)
  (block : block)
=
  match block with

  | IJump label ->
      (* If we have knowledge of the current token, and if the target block
         can be specialized with respect to the current token, then we wish
         to preserve our knowledge and jump to a specialized version of the
         target block. *)
      (* In fact, if the target block can be specialized then it must be the
         case that we already have knowledge of the current token. Indeed,
         as soon we get a new token from the lexer, we inspect it; there is
         never a jump after a lexer call and before the token is inspected. *)
      let target : vertex =
        if can_specialize label then begin
          assert (env <> None);
          (label, env)
        end
        else
          (label, None)
      in
      jump condition target

  | IPrim (p, PrimLexerCall _, block) ->
      (* A lexer call overwrites the [token] register. *)
      assert (p = PReg token);
      (* It must be the case that the current token is not known and that we
         have not already taken a branch. *)
      assert (env = None);
      assert (condition = CondNone);
      (* The lexer call will be preserved by the program transformation.
         Immediately after this call, a [CASEtok] instruction will be
         inserted so as to analyze the new token, and within each branch, a
         copy of the remainder of the current block will be placed.
         Anticipating on this transformation, we analyze the remainder of
         the block once for each terminal symbol [t]. *)
      T.iter_real @@ fun t ->
        let env = Some t in
        let condition = CondTok t in
        walk jump env condition block

  | ICaseToken (r, branches, odefault) ->
      (* [r] must be the [token] register. *)
      assert (r = token);
      (* The current token must be known, because the transformation inserts
         a [CASEtok] instruction immediately after every lexer call. *)
      let t = Option.force env in
      (* Thus, this [CASEtok] instruction can be simplified. Only one branch
         can be taken, and we can statically tell which branch that is. *)
      begin match find_tokbranch branches (TSet.singleton t), odefault with
      | Some (TokSingle _, block), _
      | Some (TokMultiple _, block), _
      | None, Some block ->
          walk jump env condition block
      | None, None ->
          (* No branch is taken, and there is no default branch.
             This cannot happen. *)
          assert false
      end

  | ICaseTag (_, branches) ->
      (* In each branch, we must update [condition]. *)
      assert (condition = CondNone);
      List.iter (fun (TagSingle t, block) ->
        let condition = CondTag t in
        walk jump env condition block
      ) branches

  (* The instructions that remain do not write the registers of interest.
     They are not transformed. *)
  | IPush _
  | IPop _
  | IPeek _
  | IDef _
  | IPrim _
  | ITrace _
  | IComment _
    ->
      assert_not_sensitive (written block);
      Block.iter (walk jump env condition) block

  (* Check that terminator instructions are never used inside [CASEtok].
     Without this property, we might inadvertently attempt to fuse a branch
     that contains a [STOP] instruction and a branch that contains a [RET]
     instruction, or two branches that contain [STOP] instructions with
     different state numbers. Technically, I think that this would cause
     [find_tokbranch] to fail because it cannot decide which branch is
     definitely taken. *)
  | IDead _
  | IStop _
  | IReturn _
    ->
      assert (match condition with CondTok _ -> false | _ -> true)

(* -------------------------------------------------------------------------- *)

(* An enumeration of all edges. *)

let edge : edge enum =
  enum @@ fun yield ->
    (* Iterate over all blocks. *)
    program.cfg |> Label.Map.iter @@ fun label tblock ->
      (* If this block can be specialized, then analyze it once for every
         terminal symbol [t]. Otherwise, analyze it just once. *)
      let go env =
        let source = (label, env) in
        let jump condition target = yield (source, condition, target) in
        let condition = CondNone in
        let block = tblock.block in
        walk jump env condition block
      in
      if can_specialize_tblock tblock then
        T.iter_real @@ fun t -> go (Some t)
      else
        go None

(* -------------------------------------------------------------------------- *)

(* Number the edges. *)

module Edge : NUMBERING with type t = edge = struct
  module N = Numbering.ForType(struct type t = edge end)
  let () = foreach edge (fun e -> ignore (N.encode e))
  include N.Done()
    (* This defines [n], [encode], [decode]. *)
end

let () =
  if debug then
    eprintf "DEBUG: %d edges\n%!" Edge.n

(* -------------------------------------------------------------------------- *)

(* Invoke the partition refinement algorithm. *)

(* The use of strongly-typed integer indices in the algorithm's API
   makes this invocation somewhat verbose. *)

module DFA = struct
  open Indexing
  (* States. *)
  module States = Const(struct let cardinal = Vertex.n end)
  type states = States.n
  let states = States.n
  type state = states index
  let encode_vertex (s : vertex) : state =
    Index.of_int States.n (Vertex.encode s)
  let decode_vertex (i : state) : vertex =
    Vertex.decode (i :> int)
  (* Transitions. *)
  module Transitions = Const(struct let cardinal = Edge.n end)
  type transitions = Transitions.n
  let transitions = Transitions.n
  type transition = transitions index
  let decode_transition (i : transition) : edge =
    Edge.decode (i :> int)
  (* Descriptions of the transitions. *)
  let label (i : transition) : condition =
    let _source, condition, _target = decode_transition i in
    condition
  let source (i : transition) : state =
    let source, _condition, _target = decode_transition i in
    encode_vertex source
  let target (i : transition) : state =
    let _source, _condition, target = decode_transition i in
    encode_vertex target
  (* The initial states. *)
  let initials : state enum =
    map encode_vertex initial_vertex
  (* The final states. (Every state is final.) *)
  let finals : state enum =
    map encode_vertex vertex
  (* The initial partition. *)
  let groups : state enum enum =
    map (map encode_vertex) groups
  (* Transmit the [debug] flag. *)
  let debug = debug
end

let () =
  if debug then
    eprintf "DEBUG: now performing partition refinement...\n%!"

module M =
  Minimize.Minimize(Condition)(DFA)

let () =
  if debug then begin
    eprintf "DEBUG: after partition refinement, %d vertices\n%!"
      (Indexing.cardinal M.states);
    eprintf "DEBUG: after partition refinement, %d edges\n%!"
      (Indexing.cardinal M.transitions)
  end

(* -------------------------------------------------------------------------- *)

(* A new vertex (that is, a vertex in the minimized automaton) is either
   - a pair [(label, None)],
     where [can_specialize label] is false; or
   - a pair [(label, Some toks)],
     where [can_specialize label] is true
     and [toks] is a set of terminal symbols. *)

type env =
  TSet.t option

type vertex' =
  label * env

(* Debug printers. *)

let print_env env =
  match env with
  | None ->
      "!"
  | Some toks ->
      sprintf "[%s]" (TSet.print toks)

let print_vertex' (label, env) =
  sprintf "%s%s" (Label.export label) (print_env env)

(* -------------------------------------------------------------------------- *)

(* A state number in the minimized automaton can be mapped to a new vertex. *)

let decode_vertex' (s : M.state) : vertex' =
  (* First, map [s] back to a (nonempty) list of source vertices,
     all of which must have the same label. *)
  let ss : vertex list =
    s
    |> M.backport_state_all
    |> Enum.map DFA.decode_vertex
    |> Enum.enum_to_list
  in
  assert (ss <> []);
  (* Find out about this label. *)
  let label, _ = List.hd ss in
  if can_specialize label then
    (* Find all vertices in the list, each of which must be of the form
       [(label, Some t)] for some terminal symbol [t], and construct a
       single vertex' [(label, Some toks)] where the set [toks ]gathers
       all of these terminal symbols. *)
    let extract (label', t) = assert (label = label'); Option.force t in
    let toks = List.map extract ss in
    let toks = TSet.of_list toks in
    (label, Some toks)
  else begin
    (* The list must be a singleton list. *)
    assert (ss = [(label, None)]);
    (label, None)
  end

(* An enumeration of all new vertices. *)

let vertex' : vertex' enum =
  map decode_vertex' (enum (Indexing.Index.iter M.states))

(* Debugging output. *)

let () =
  if debug then
    foreach vertex' @@ fun (label, env) ->
      env |> Option.iter @@ fun toks ->
        eprintf "DEBUG: equivalence class (size %d): %s (%s)\n"
          (TSet.cardinal toks) (Label.export label) (TSet.print toks)

(* -------------------------------------------------------------------------- *)

(* [transport] maps a (reachable) vertex to the corresponding new vertex. *)

let transport (v : vertex) : vertex' =
  v
  |> DFA.encode_vertex
  |> M.transport_state |> Option.force
  |> decode_vertex'

(* [enlarge] maps a candidate new vertex [v'] to a valid new vertex.

   If [v'] is of the form [(label, None)] then the result is [v'].

   If [v'] is of the form [(label, Some toks)]
   where [toks] is a (possibly strict) subset of an equivalence class
   [toks'] of tokens that are treated in the same manner at [label]
   then the result is [(label, Some toks')]. *)

let enlarge (v' : vertex') : vertex' =
  if debug then eprintf "DEBUG: enlarging %s\n%!" (print_vertex' v');
  match v' with
  | (label, None) ->
      assert (not (can_specialize label));
      v'
  | (label, Some toks) ->
      assert (can_specialize label);
      (* Choose an arbitrary terminal symbol [t] in [toks]. The choice
         of [t] does not matter; all choices lead to the same result. *)
      let t = try TSet.choose toks with Not_found -> assert false in
      (* Transporting the vertex [(label, Some t)] yields the desired
         new vertex. *)
      let v' = transport (label, Some t) in
      assert (
        let label', otoks' = v' in
        let toks' = Option.force otoks' in
        Label.equal label label' &&
        TSet.subset toks toks'
      );
      v'

(* -------------------------------------------------------------------------- *)

(* Determining which groups of tokens must be distinguished at a lexer call.  *)

(* [groups block] expects a block [block] that contains a lexer call and
   returns a partition of the set [universe], that is, a list of nonempty,
   pairwise disjoint sets of tokens whose union is the universe.  *)

(* By definition of [walk], because this block contains a lexer call, it
   must contain a jump labeled [CondTok t] for every terminal symbol [t].
   So, the union of the sets that we obtain must be the universe. *)

(* This code works as follows. First, using [walk], we enumerate the outgoing
   edges of the block [block]. We construct their target vertex in the original
   DFA, then transport it to a vertex in the minimized DFA. We sort these edges
   by target state, collecting the edge labels (which are terminal symbols)
   into sets. This tells us what sets of tokens must be distinguished. *)

let groups block : TSet.t list =
  (* Allocate a vector, indexed by the states of the minimized automaton,
     containing sets of terminal symbols. *)
  let edges = Indexing.Vector.make M.states TSet.empty in
  (* Populate this vector by examining the jumps out of [block]. *)
  (* For each jump toward a target vertex [vertex], *)
  let jump condition (target : vertex) =
    (* Extract a terminal symbol [t] out of the edge label, *)
    let t = match condition with CondTok t -> t | _ -> assert false in
    (* Map this vertex to a state of the minimized automaton. *)
    let target' : M.state =
      target
      |> DFA.encode_vertex
      |> M.transport_state |> Option.force
    in
    (* Add [t] to the set stored at index [target'] in the vector. *)
    let toks = Indexing.Vector.get edges target' in
    let toks = TSet.add t toks in
    Indexing.Vector.set edges target' toks
  in
  walk jump None CondNone block;
  (* Gather the nonempty sets from the vector. *)
  let groups =
    Indexing.Vector.fold_left (fun accu toks ->
      if TSet.is_empty toks then accu else toks :: accu
    ) [] edges
  in
  (* A sanity check. *)
  assert TSet.(is_universe (big_union groups));
  groups

(* -------------------------------------------------------------------------- *)

(* [name_vertex' v'] returns a conventional name for the transformed block that
   corresponds to the new vertex [v']. Naturally, two distinct new vertices
   must be mapped to two distinct names. *)

let name_vertex' (v' : vertex') : label =
  let (label, env) = v' in
  match env with
  | None ->
      label
  | Some toks ->
      if TSet.cardinal toks > TSet.cardinal_universe / 2 then
        (* If [toks] involves more than half of all tokens then we do not
           need to include a list of all of its members in the name. *)
        sprintf "%s_majority" (Label.export label)
        |> Label.import
      else
        (* The identifier returned by [TSet.identify] begins with 'c' so
           cannot be confused with a "majority" label. *)
        sprintf "%s_%s" (Label.export label) (TSet.identify toks)
        |> Label.import

(* -------------------------------------------------------------------------- *)

(* [spec_block env block] transforms the block [block]. *)

(* The environment [env] keeps track of whether (in the transformed block)
   the current token is currently known or unknown. It should be the case
   that [env] is [Some _] if and only the current block can be specialized,
   that is, if and only if its label satisfies [can_specialize label]. *)

let rec spec_block (env : env) block =
  match block with

  | IJump target ->
      (* Compute which new vertex is the target of the jump. *)
      let target : vertex' =
        if can_specialize target then begin
          assert (env <> None);
          enlarge (target, env)
        end
        else
          (* It may be the case here that we are forgetting information
             by moving from [env = Some _] to [env = None]. (I think.) *)
          (target, None)
      in
      IJump (name_vertex' target)

  | IPrim (p, PrimLexerCall vs, block') ->
      (* A lexer call overwrites the [token] register. *)
      assert (p = PReg token);
      (* It must be the case that the current token is not known. *)
      assert (env = None);
      (* The lexer call is preserved by the program transformation.
         Immediately after this call, we choose to eagerly analyze
         the new token using a [CASEtok] instruction and to specialize
         the code that follows. *)
      (* The branches are computed as follows. Because [groups block] is a
         partition of the set [universe], no default branch is needed. *)
      let branches : tokbranch list =
        groups block |> List.map @@ fun toks ->
          (* Changing [env] from [None] to [Some _] causes the code in
             each branch to be specialized. *)
          let env = Some toks in
          tokpat toks, spec_block env block'
      in
      IPrim (p, PrimLexerCall vs,
      ICaseToken (token, branches, None))

  | ICaseToken (r, branches, odefault) ->
      (* [r] must be the [token] register. *)
      assert (r = token);
      (* because we insert a [CASEtok] instruction immediately after every
         lexer call, the current token must be known. *)
      let toks = Option.force env in
      assert (not (TSet.is_empty toks));
      (* Thus, this CASEtok instruction can be simplified. *)
      begin match find_tokbranch branches toks, odefault with
      | Some (TokSingle (_, r), block), _ ->
          (* Emit a [DEF] instruction to copy the semantic value of the token
             from the register [tokv] to the register [r], where this branch
             expects to find it. *)
          let bs = Bindings.assign [PReg r] [VReg tokv] in
          IDef (bs, spec_block env block)
      | Some (TokMultiple _, block), _
      | None, Some block ->
          spec_block env block
      | None, None ->
          assert false (* no branch taken; no default branch present *)
      end

  (* The remaining instructions do not write the registers of interest,
     so the environment need not be updated. These instructions are not
     transformed. *)
  | IPush _
  | IPop _
  | IPeek _
  | IDef _
  | IPrim _
  | ITrace _
  | IComment _
  | IDead _
  | IStop _
  | IReturn _
  | ICaseTag _
    ->
      assert_not_sensitive (written block);
      Block.map (spec_block env) block

(* -------------------------------------------------------------------------- *)

(* Construct the complete transformed program. *)

let program =
  let cfg = ref Label.Map.empty in
  foreach vertex' begin fun v' ->
    let (label, env) = v' in
    let tblock = lookup program label in
    let block = spec_block env tblock.block in
    let tblock = { tblock with block } in
    cfg := Label.Map.add (name_vertex' v') tblock !cfg
  end;
  let cfg = !cfg in
  { program with cfg }

end (* SpecializeToken *)

(* Make this program transformation accessible to the outside as a function.  *)

let specialize_token program =
  let module S = SpecializeToken(struct let program = program end) in
  Time.tick "StackLang: specialization with respect to the token";
  S.program

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* This transformation allows a [STOP] instruction to swallow some of the
   instructions that precede it. *)

(* A stop block is a sequence of straight-line code, including [PUSH], [POP],
   [PEEK], [DEF], [TRACE], and [JUMP] instructions, including comments, and
   ending with a [STOP] instruction. *)

(* Inside a stop block, we remove all pure instructions, namely [PUSH], [POP],
   [PEEK], [DEF]. We also remove [JUMP] instructions; that is, we effectively
   inline stop blocks at their starting point. We keep [TRACE] instructions
   (which do not depend on the stack or on the registers) and comments. *)

module StopEarlier (X : sig val program : program end) = struct open X

(* [is_stop_block] recognizes a stop block. It may be applied to a block
   before or after this block has been transformed. *)

let rec is_stop_block block =
  match block with
  | IStop _ ->
      true
  | IPush (_, _, block)
  | IPop (_, _, block)
  | IPeek (_, _, block)
  | IDef (_, block)
  | IComment (_, block)
  | ITrace (_, block) ->
      is_stop_block block
  | IJump target ->
      let target_block = (lookup program target).block in
      is_stop_block target_block
  | _ ->
      false

(* The transformation recognizes stop blocks and inlines them at their
   starting point. It is implemented as a bottom-up rewriting pass. *)

let rec transform_block block =
  (* First, transform every child. *)
  let block = Block.map transform_block block in
  (* Then, look at the current instruction. *)
  match block with
  | IPush (_, _, block)
  | IPop (_, _, block)
  | IPeek (_, _, block)
  | IDef (_, block)
    when is_stop_block block ->
      (* A pure instruction in front of a stop block is dropped. *)
      block
  | IJump target ->
      (* A [JUMP] instruction in front of a stop block is dropped.
         The block is inlined. *)
      (* [is_stop_block] is applied to an untransformed block. This
         is a consequence of the fact that we do not control in what
         order blocks are processed. *)
      let target_block = (lookup program target).block in
      if is_stop_block target_block then
        transform_block target_block
      else
        block
  | _ ->
      block

let transform_tblock tblock =
  { tblock with block = transform_block tblock.block }

let program =
  { program with cfg = Label.Map.map transform_tblock program.cfg }

end (* StopEarlier *)

(* Make this program transformation accessible to the outside as a function.  *)

let stop_earlier program =
  let module S = StopEarlier(struct let program = program end) in
  Time.tick "StackLang: propagating STOP instructions";
  S.program

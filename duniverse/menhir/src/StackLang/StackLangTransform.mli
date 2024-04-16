(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open StackLang

(**[specialize_state] specializes certain routines for the situation where
   the [state] register contains a statically known tag. It also performs a
   certain amount of inlining, insofar as it has a beneficial effect on
   specialization itself. [specialize] should be applied to a program where
   every block is reachable. *)
val specialize_state : program -> program

(**[specialize_token] specializes the code with respect to the current
   token. In the transformed program, every invocation of the lexer is
   immediately followed with a case analysis that examines the new token,
   and this token is never examined again. (In other words, every [CASEtok]
   instruction immediately follows a call to the lexer.) This transformation
   can blow up the size of the code, so it should be used with caution.

   The sets of needed registers must be accurate when [specialize_token] is
   invoked. They are not updated, so [NeededRegisters.update] must be
   invoked after this transformation has been performed.

   [specialize_token] assumes that no block contains both a lexer call and a
   [CASEtag] instruction. It also assumes that [DEAD], [STOP] and [RET]
   instructions cannot appear in the same block as a lexer call or inside a
   [CASEtag] instruction. These restrictions are satisfied by EmitStackLang
   but are not necessarily obeyed by other transformations (such as inlining),
   so [specialize_token] should be applied first. *)
val specialize_token : program -> program

(**[commute_pushes] moves PUSH instructions forward in the code, in the
   hope that they meet POP instructions and cancel out. It also performs
   a certain amount of inlining, insofar as it has a beneficial effect
   on the movement of PUSH instructions. *)
val commute_pushes : program -> program

(**[remove_unreachable_blocks program] transforms the program [program] by
   removing every unreachable block. *)
val remove_unreachable_blocks : program -> program

(**[inline cautious program] transforms the program [program] by removing
   every unreachable block and by inlining away every label whose in-degree
   is 1, provided inlining is permitted. The rules that restrict inlining
   are roughly as follows:

   - Inlining a routine inside a [casetag] instruction is not permitted.

   - In normal mode (that is, when [cautious] is false), a routine that
     carries the hint [OnlyIfKnownState] cannot be inlined.

   - In cautious mode, a routine can be inlined only if both the source
     and destination blocks carry the hint [NoHint]. In short, this allows
     inlining a [run] routine into a [run] routine, and nothing else. *)
val inline : (* cautious: *) bool -> program -> program

(**[stop_earlier program] transforms the program [program] by letting a [STOP]
   instructions swallow its predecessors, as long as they are pure
   instructions. For instance, the sequence [PUSH; STOP] is replaced with just
   [STOP].

   This transformation can create opportunities for inlining. Conversely,
   inlining can create new opportunities for [STOP] instructions to swallow
   their predecessors.

   The sets of needed registers are not updated, so [NeededRegisters.update]
   must be invoked after this transformation has been performed. *)
val stop_earlier: program -> program

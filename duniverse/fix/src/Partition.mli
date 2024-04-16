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

(**This module offers a {b partition refinement} data structure.

   This data structure is useful in partition refinement algorithms, such as
   automata minimization algorithms. It is taken from Antti Valmari's 2012
   paper, "Fast brief practical DFA minimization". *)

open Indexing

(**An element is represented as an integer index in the range [\[0, n)].
   See {!Indexing} for more information about type-level indices. *)
type 'n elt =
  'n index

type 'n t
(**A data structure of type ['n t] represents a partition of a set of [n] {i
   elements}. The cardinal of this set is represented at the type level by
   ['n]. The elements are grouped into nonempty disjoint {i blocks}. The
   data structure allows blocks to be split, so, over time, blocks become
   smaller: hence the name {i partition refinement}.

   The data structure allows elements to be temporarily {i marked}; marks
   are used to determine how blocks must be split.

   Furthermore, the data structure allows some elements to be permanently
   {i discarded}. The discarded elements are set aside in a special block
   and no longer participate in the partition refinement process. *)

(**{1 Creation} *)

val create :
  ?partition:('n elt -> 'n elt -> int) ->
  'n cardinal -> 'n t
(**[create ?partition n] creates a fresh partition data structure for a set
   of cardinal [n].

   If the optional argument [partition] is absent, then the partition
   initially has a single block, which contains all elements. In this
   case, the time complexity of this operation is {i O(n)}.

   If the optional argument [partition] is present, then [partition] must
   be a function that implements a total order. Two elements [x] and [y]
   are placed in distinct blocks if and only if they are distinguished by
   this order, that is, if and only if [partition x y] is nonzero. In this
   case, the time complexity of this operation is {i O(n.log n)}. *)

(**{1 Marking Elements and Splitting Blocks} *)

val mark : 'n t -> 'n elt -> unit
(**[mark p elt] marks the element [elt]. If this element was marked
   already, then this operation has no effect. If this element was
   discarded earlier, then this operation has no effect.

   The time complexity of this operation is {i O(1)}. *)

val clear_marks : 'n t -> unit
(**[clear_marks p] clears all marks, so, upon return, every element is
   unmarked.

   The amortized time complexity of this operation is {i O(1)}. *)

val split : 'n t -> unit
(**[split p] splits every block that has both marked and unmarked elements
   into two blocks: a block of the marked elements and a block of the
   unmarked elements. Then, all marks are cleared.

   Whenever a block is split into two subblocks, the existing block index
   is re-used for the larger half, while a new block number is allocated
   and given to the smaller half.

   The amortized time complexity of this operation is {i O(1)}. *)

(**{1 Discarding Elements} *)

val discard_unmarked : 'n t -> unit
(**[discard_unmarked p] discards every unmarked element. Then, all marks
   are cleared, so, upon return, every element is unmarked.

   If every element of a block is discarded, then this block disappears;
   so, this operation can cause the number of blocks to decrease.

   The time complexity of this operation is {i O(n)}. *)

val discard : 'n t -> ('n elt -> bool) -> unit
(**[discard p f] discards every element [x] such that [f x] is true.
   [discard] requires that no element is marked, and preserves this
   property.

   If every element of a block is discarded, then this block disappears;
   so, this operation can cause the number of blocks to decrease.

   The time complexity of this operation is {i O(n)}. *)

(**{1 Querying the Data Structure} *)

type block = int
(**Blocks are identified by integer indices, comprised between [0] inclusive
   and [block_count p] exclusive. Of course, the number of blocks, the
   mapping of elements to blocks, and the choice of a distinguished element
   within each block can change when the data structure is modified. *)

val block_count : 'n t -> int
(**[block_count p] returns the current number of blocks. The special block
   that contains the discarded elements is not counted.

   The time complexity of this operation is {i O(1)}. *)

val find : 'n t -> 'n elt -> block
(**[find p elt] returns the index of the block that currently contains the
   element [elt]. If this element has been discarded, the special index [-1]
   is returned. Otherwise, a block index in the range [\[0, block_count p)]
   is returned.

   The time complexity of this operation is {i O(1)}. *)

val iter_block_elements : 'n t -> block -> ('n elt -> unit) -> unit
(**[iter_block_elements p blk yield] applies [yield] to each element that
   currently belongs to the block [blk].

   The time complexity of this operation is linear in the size of the
   block [blk]. *)

val iter_all_elements : 'n t -> ('n elt -> unit) -> unit
(**[iter_all_elements p yield] applies [yield] to every element of every
   block. The discarded elements are ignored.

   The time complexity of this operation is {i O(n)}. *)

val choose : 'n t -> block -> 'n elt
(**[choose p blk] returns an arbitrary element of the block [blk].

   The time complexity of this operation is {i O(1)}. *)

val is_chosen : 'n t -> 'n elt -> bool
(**[is_chosen p elt] returns [true] if the element [elt] has not been
   discarded and is the distinguished element of its block, that is, if
   [choose p (find p elt)] is [elt].

   The time complexity of this operation is {i O(1)}. *)

val exhaust_marked_elements : 'n t -> block -> ('n elt -> unit) -> unit
(**[exhaust_marked_elements p blk yield] applies [yield] to each marked
   element of the block [blk]. The function [yield] is allowed to mark new
   elements of this block or of other blocks. The iteration continues
   until every marked element of the block [blk] has been yielded once.

   The time complexity of this operation is linear in the number of
   yielded elements. *)

val get_marked_blocks : 'n t -> block array
(**[get_marked_blocks p] returns a (fresh) array of all blocks that have
   at least one marked element.

   The time complexity of this operation is linear in the number of such
   blocks. *)

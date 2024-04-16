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

(**An element is represented as an integer index in the range [\[0, n)]. *)
type 'n elt =
  'n index

(**A block index. Blocks are normally numbered from [0] to [z-1], where
   [z] is the current number of blocks. The block index [-1] plays a
   special role: it is used to group elements that have been discarded.

   A normal block is never empty. The function [discard_unmarked], which
   discards elements, can temporarily cause a normal block to become
   empty; this is immediately fixed by invoking [compress].

   The total number of normal blocks cannot exceed [n]. *)
type block =
  int

(**An array indexed by blocks. There are at most ever [n] blocks, so the
   length of such an array is [n]. Only valid block indices must be
   accessed. *)
type 'a block_array =
  'a array

(**A location is an index into the [element] array. The locations stored
   in the arrays [location] and [first] inhabit the range [\[0, n)]. The
   locations stored in the array [past] inhabit the range [(0, n\]]. *)
type loc =
  int

(**An array indexed by locations. *)
type 'a loc_array =
  'a array

type 'n t = {

  mutable z : block;
  (**The current number of blocks. This number cannot exceed [n].
     This number increases in [split] and can decrease in [compress]. *)

  element   : 'n elt loc_array; (* E *)
  (**An array of all elements. This array is mutable: the location of an
     element in this array can change over time. The function [mark] moves
     elements around. The elements of a block [b] are stored in this array
     from index [first.(b)] inclusive to index [past.(b)] exclusive. *)

  location  : ('n, loc) vector; (* L *)
  (**An array that maps an element to its location.
     So [element.(location.(e))] is [e]
     and [location.(element.(i))] is [i].
     Like the array E, this array is mutable. *)

  block     : ('n, block) vector; (* S *)
  (**An array that maps an element to its block. An element that has been
     discarded is mapped to the special block [-1]. *)

  first     : loc block_array; (* F *)
  (**An array that maps a block to the start location of this block. *)

  past      : loc block_array; (* P *)
  (**An array that maps a block to the end location of this block. *)

  marked    : int block_array; (* M *)
  (**An array that maps a block to the number of marked elements that it
     currently contains. At indices in the range [\[p.z, n)], this array
     is filled with zeroes. *)

  workset   : block array; (* W *)
  (**A bag of the blocks that currently have at least one marked element.
     No block appears twice in this bag, so it is actually a set. It is
     implemented as a stack of capacity [n]. *)

  mutable w : int;
  (**The current number of elements in the workset. In other words, this
     is the index of the top of the stack. The stack grows towards the
     right. *)

}

let create_empty () : Empty.n t =
  {
    z        = 0;
    element  = [||];
    location = Vector.empty;
    block    = Vector.empty;
    first    = [||];
    past     = [||];
    marked   = [||];
    workset  = [||];
    w        = 0
  }

let create_nonempty (type n) ?partition (n : n cardinal) =
  assert (cardinal n > 0);
  let p = {
    z        = 1;
    element  = Vector.as_array (Vector.init n (fun e -> e));
               (* same as [Array.init n (fun e -> e)], with a richer type *)
    location = Vector.init n (fun e -> (e :> loc));
    block    = Vector.make n 0;
    first    = Array.make (cardinal n) 0; (* uninitialized *)
    past     = Array.make (cardinal n) 0; (* uninitialized *)
    marked   = Array.make (cardinal n) 0;
    workset  = Array.make (cardinal n) 0; (* irrelevant value *)
    w        = 0
  } in
  (* Every element is now in block 0. The arrays [first] and [past] are
     uninitialized. *)
  match partition with
  | None ->
      (* We want just one block. *)
      p.first.(0) <- 0;
      p.past.(0) <- cardinal n;
      p
  | Some cmp ->
      (* Sort the array of elements. The array [location] must now be
         considered uninitialized. *)
      Array.sort cmp p.element;
      (* Iterate over the sorted array so as to identify the runs of
         adjacent elements that are equal according to [cmp]. *)
      (* During this loop, [current] is an element of the current block;
         [z] is the index of the current block. *)
      let current = ref p.element.(0) in
      p.first.(0) <- 0;
      let z = ref 0 in
      (* This loop writes [block], [location], [first], [past]. *)
      for loc = 0 to cardinal n - 1 do
        let elt = p.element.(loc) in
        if cmp !current elt <> 0 then begin
          (* Close the current block. *)
          p.past.(!z) <- loc;
          (* Allocate a new block index. *)
          incr z;
          (* Open a new block. *)
          p.first.(!z) <- loc;
          current := elt
        end;
        Vector.set p.block elt (!z);
        Vector.set p.location elt loc
      done;
      (* Close the current block, which is the last block. *)
      p.past.(!z) <- cardinal n;
      (* The number of blocks is the index of the last block, plus one. *)
      p.z <- !z + 1;
      p

let create (type n) ?partition (n : n cardinal) : n t =
  match equal n (Vector.length Vector.empty) with
  | Some Refl ->
      create_empty()
  | None ->
      create_nonempty ?partition n

let mark (p : 'a t) elt =
  let blk = Vector.get p.block elt in
  (* Marking a discarded element has no effect. *)
  if blk > -1 then begin
    (* Find the position of the leftmost unmarked element in this block,
       if there is one. *)
    let unmarked_loc = p.first.(blk) + p.marked.(blk) in
    let loc = Vector.get p.location elt in
    if loc >= unmarked_loc then begin
      (* The test [loc >= unmarked_loc] confirms that [elt] is unmarked and
         that [unmarked_loc] is indeed the position of the leftmost unmarked
         element of the block. *)
      if loc > unmarked_loc then begin
        (* The test [loc > unmarked_loc] confirms that the two elements that
           we have been discussing are distinct. Swap them. *)
        let unmarked_elt = p.element.(unmarked_loc) in
        assert (p.element.(loc) = elt);
        p.element.(loc) <- unmarked_elt;
        p.element.(unmarked_loc) <- elt;
        Vector.set p.location unmarked_elt loc;
        Vector.set p.location elt unmarked_loc
      end;
      (* If this block becomes marked, insert it into the workset. *)
      if p.marked.(blk) = 0 then begin
        p.workset.(p.w) <- blk;
        p.w <- p.w + 1
      end;
      (* Update the count of marked elements in this block. *)
      p.marked.(blk) <- p.marked.(blk) + 1
    end
  end

(* [clear_iter_work p yield] applies [yield] to every block in the workset
   and empties the workset. The function [yield] is not allowed to mark new
   elements. *)
let clear_iter_workset p yield =
  for i = 0 to p.w - 1 do
    let blk = p.workset.(i) in
    yield blk
  done;
  p.w <- 0

let clear_marks p =
  clear_iter_workset p begin fun blk ->
    p.marked.(blk) <- 0
  end

(* The time complexity of [split] is O(number of marked blocks) + O(sum of the
   sizes of the smaller subblocks). The first summand can be charged to [mark]
   and therefore disappears. The second summand remains, and, by virtue of
   Hopcroft's argument, the total cost of a series of calls to [split] is
   O(n.log n). *)

let split p =
  (* Iterate over every block [blk] that has at least one marked element. *)
  clear_iter_workset p begin fun blk ->
    assert (p.marked.(blk) > 0);
    (* Let [frontier] be the location of the leftmost unmarked element
       in this block. *)
    let frontier = p.first.(blk) + p.marked.(blk) in
    if frontier = p.past.(blk) then
      (* Every element in this block is marked. Unmark every element and
         do not split this block. *)
      p.marked.(blk) <- 0
    else begin
      (* This block has both marked and unmarked elements, so it must be
         split. The index of the new block is [p.z]. *)
      let blk' = p.z in
      assert (let n = Array.length p.element in blk' < n);
      if p.marked.(blk) <= p.past.(blk) - frontier then begin
        (* There are fewer marked elements than unmarked elements. *)
        (* The new block contains the marked elements. *)
        p.first.(blk') <- p.first.(blk);
        p.past.(blk') <- frontier;
        (* The existing block retains the unmarked elements. *)
        p.first.(blk) <- frontier
      end
      else begin
        (* There are fewer unmarked elements than marked elements. *)
        (* The new block contains the unmarked elements. *)
        p.first.(blk') <- frontier;
        p.past.(blk') <- p.past.(blk);
        (* The existing block retains the marked elements. *)
        p.past.(blk) <- frontier
      end;
      (* Assign the elements of the new block to the new block. *)
      for loc = p.first.(blk') to p.past.(blk') - 1 do
        Vector.set p.block p.element.(loc) blk'
      done;
      (* Unmark every element in the two new blocks. *)
      p.marked.(blk) <- 0;
      assert (p.marked.(blk') = 0);
      p.z <- blk' + 1
    end
  end

let iter_block_elements p blk yield =
  assert (0 <= blk && blk < p.z);
  for loc = p.first.(blk) to p.past.(blk) - 1 do
    let elt = p.element.(loc) in
    yield elt
  done

let iter_all_elements p yield =
  for blk = 0 to p.z - 1 do
    iter_block_elements p blk yield
  done

let[@inline] postincrement r =
  let x = !r in
  r := x + 1;
  x

(* [compress p] destroys all empty blocks. That is, it reindexes the blocks,
   so that empty blocks do not receive an index in the new scheme. *)

(* The arrays [element], [location] are not affected because they do not
   depend on block indices. The arrays [marked] and [workset] are not
   affected because no block is currently marked. The arrays [block],
   [first], and [past] must be adjusted. *)

let compress p =
  assert (p.w = 0);
  let next = ref 0 in
  for blk = 0 to p.z - 1 do
    (* If [p.first.(blk)] is equal to [p.past.(blk)], then this block
       is empty. Then, skip it; do not allocate a new block index. *)
    if p.first.(blk) < p.past.(blk) then begin
      (* This block is nonempty. Allocate a new block index. *)
      let blk' = postincrement next in
      if blk' < blk then begin
        (* Adjust [block]. *)
        iter_block_elements p blk begin fun elt ->
          Vector.set p.block elt blk'
        end;
        (* Adjust [first] and [past]. *)
        p.first.(blk') <- p.first.(blk);
        p.past.(blk') <- p.past.(blk)
      end
    end
  done;
  (* The new number of blocks is the final value of [next]. *)
  p.z <- !next

let discard_unmarked p =
  p.w <- 0;
  (* For every block, *)
  for blk = 0 to p.z - 1 do
    let frontier = p.first.(blk) + p.marked.(blk) in
    (* Assign every unmarked element of this block to the special block [-1]. *)
    for loc = frontier to p.past.(blk) - 1 do
      Vector.set p.block p.element.(loc) (-1)
    done;
    (* Shrink this block to just its marked elements. The block possibly
       becomes empty at this point. *)
    p.past.(blk) <- frontier;
    (* Unmark every element of this block. *)
    p.marked.(blk) <- 0
  done;
  (* Empty blocks may have been created: invoke [compress] to destroy them. *)
  compress p

let discard p f =
  (* Initially, we expect that no element is marked. *)
  assert (p.w = 0);
  iter_all_elements p (fun elt ->
    if not (f elt) then
      mark p elt
  );
  discard_unmarked p

let block_count p =
  p.z

let find p elt =
  Vector.get p.block elt

let choose p blk =
  assert (0 <= blk && blk < p.z);
  assert (p.first.(blk) < p.past.(blk));
  p.element.(p.first.(blk))

let is_chosen p elt =
  let blk = Vector.get p.block elt in
  blk > -1 &&
  let loc = Vector.get p.location elt in
  (* The block [blk] cannot be empty. *)
  assert (loc < p.past.(blk));
  loc = p.first.(blk)

let exhaust_marked_elements p blk yield =
  assert (0 <= blk && blk < p.z);
  (* [next] is the location of the next marked element that we plan
     to process. *)
  let next = ref p.first.(blk) in
  (* As long as there remain unprocessed marked elements, *)
  while !next < p.first.(blk) + p.marked.(blk) do
    (* Process the marked elements in the interval [\[next, goal)]. *)
    let goal = p.first.(blk) + p.marked.(blk) in
    for loc = !next to goal - 1 do
      yield p.element.(loc)
    done;
    (* Adjust [next]. Because the function [yield] may mark more
       elements, we might not be done; hence the [while] loop. *)
    (* The reason why this works is a bit subtle: we exploit the fact
       that when [mark] marks a new element, the newly marked element
       is placed to the right of the elements that are already marked,
       without disturbing them. *)
    next := goal
  done

let get_marked_blocks p =
  Array.sub p.workset 0 p.w

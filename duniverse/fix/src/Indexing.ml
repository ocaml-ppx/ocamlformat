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

(* A suspension is used to represent a cardinal-that-may-still-be-unknown. *)

(* Earlier versions used the definition [type 'n cardinal = int Lazy.t]. We
   now use the stronger definition that follows. This new definition forces
   the equality ['n = unit]. This equality remains invisible outside of this
   module. Inside this module, it lets the type-checker regard all phantom
   indices as equal. This allows [equal] and [assert_equal] to be well-typed;
   with the weaker definition, they would be ill-typed. *)

type 'n cardinal =
  Cardinal : int Lazy.t -> unit cardinal [@@ocaml.unboxed]

(* The function [cardinal] forces the cardinal to become fixed. Up to the
   type annotations, it is equivalent to [Lazy.force]. *)

let cardinal (type n) (Cardinal (lazy n) : n cardinal) =
  n

let is_fixed (type n) (Cardinal c : n cardinal) =
  Lazy.is_val c

type (_, _) eq = Refl : ('a, 'a) eq

let equal (type n m) (n : n cardinal) (m : m cardinal) : (n, m) eq option =
  let Cardinal (lazy n) = n in
  let Cardinal (lazy m) = m in
  if n = m then
    Some Refl
  else
    None

let assert_equal (type n m) (n : n cardinal) (m : m cardinal) : (n, m) eq =
  let Cardinal (lazy n) = n in
  let Cardinal (lazy m) = m in
  if n = m then
    Refl
  else
    sprintf
      "Fix.Indexing.assert_equal: cardinals are not equal (%d <> %d).\n%s\n"
      n m __LOC__
    |> invalid_arg

type 'n index =
  int

module type CARDINAL = sig type n val n : n cardinal end

(* [Empty] and [Const] produce sets whose cardinal is known. *)

module Empty = struct
  type n = unit
  let n = Cardinal (lazy 0)
end

module Const (X : sig val cardinal : int end) : CARDINAL = struct
  type n = unit
  let () = assert (X.cardinal >= 0)
  let n = Cardinal (lazy X.cardinal)
end

let const c : (module CARDINAL) =
  assert (c >= 0);
  (module struct type n = unit let n = Cardinal (lazy c) end)

(* [Gensym] produces a set whose cardinal is a priori unknown. A new reference
   stores the current cardinal, which grows when [fresh()] is invoked. [fresh]
   fails if the suspension [n] has been forced. *)

module Gensym () = struct

  type n = unit
  let counter = ref 0
  let n = Cardinal (lazy !counter)

  let fresh () =
    assert (not (is_fixed n));
    let result = !counter in
    incr counter;
    result

end

type ('l, 'r) either =
  | L of 'l
  | R of 'r

module type SUM = sig
  type l and r
  include CARDINAL
  val inj_l : l index -> n index
  val inj_r : r index -> n index
  val prj : n index -> (l index, r index) either
end

module Sum (L : CARDINAL)(R : CARDINAL) = struct

  type n = unit

  type l = L.n
  type r = R.n

  (* The cardinal [l] of the left-hand set becomes fixed now (if it
     wasn't already). We need it to be fixed for our injections and
     projections to make sense. *)
  let l : int = cardinal L.n
  (* The right-hand set can remain open-ended. *)
  let r : r cardinal = R.n

  let n : n cardinal =
    (* We optimize the case where [r] is fixed already, but the code
       in the [else] branch would work always. *)
    if is_fixed r then
      let n = l + cardinal r in
      Cardinal (lazy n)
    else
      Cardinal (lazy (l + cardinal r))

  (* Injections. The two sets are numbered side by side. *)
  let inj_l x = x
  let inj_r y = l + y

  (* Projection. *)
  let prj x = if x < l then L x else R (x - l)

end

let sum (type l r) (l : l cardinal) (r : r cardinal) =
  let module L = struct type n = l let n = l end in
  let module R = struct type n = r let n = r end in
  (module Sum(L)(R) : SUM with type l = l and type r = r)

module Index = struct

  type 'n t = 'n index

  let of_int (n : 'n cardinal) i : 'n index =
    let n = cardinal n in
    if 0 <= i && i < n then
      i
    else
      sprintf
        "Fix.Indexing.Index.of_int: \
         the index %d is not in the range [0, %d).\n%s\n"
        i n __LOC__
      |> invalid_arg

  let[@inline] to_int i = i

  let[@inline] iter (n : 'n cardinal) (yield : 'n index -> unit) =
    let n = cardinal n in
    for i = 0 to n - 1 do
      yield i
    done

  let[@inline] rev_iter (n : 'n cardinal) (yield : 'n index -> unit) =
    let n = cardinal n in
    for i = n - 1 downto 0 do
      yield i
    done

  exception End_of_set

  let enumerate (n : 'n cardinal) : unit -> 'n index =
    let n = cardinal n in
    let next = ref 0 in
    fun () ->
      let i = !next in
      if n <= i then raise End_of_set;
      incr next;
      i

end

type ('n, 'a) vector =
  Vector : 'a array -> (unit, 'a) vector [@@ocaml.unboxed]

module Vector = struct

  type ('n, 'a) t = ('n, 'a) vector

  let[@inline] as_array (type n) (Vector a : (n, _) t) = a

  let[@inline] get (type n) (Vector a : (n, _) t) i =
    Array.unsafe_get a i

  let[@inline] set (type n) (Vector a : (n, _) t) i x =
    Array.unsafe_set a i x

  let[@inline] set_cons t i x =
    set t i (x :: get t i)

  let length (type n) (Vector a : (n, _) t) : n cardinal =
    let n = Array.length a in
    Cardinal (lazy n)

  let empty : (Empty.n, _) t = Vector [||]

  let make (type n) (Cardinal n : n cardinal) x : (n, _) t =
    Vector (Array.make (Lazy.force n) x)

  let make' (type n) (Cardinal n : n cardinal) f : (n, _) t=
    match Lazy.force n with
    | 0 -> empty
    | n -> Vector (Array.make n (f()))

  let init (type n) (Cardinal n : n cardinal) f : (n, _) t=
    Vector (Array.init (Lazy.force n) f)

  let map (type n) f (Vector a : (n, _) t) : (n, _) t =
    Vector (Array.map f a)

  let mapi (type n) f (Vector a : (n, _) t) : (n, _) t =
    Vector (Array.mapi f a)

  let copy (type n) (Vector a : (n, _) t) : (n, _) t =
    Vector (Array.copy a)

  let iter (type n) f (Vector a : (n, _) t) =
    Array.iter f a

  let iteri (type n) f (Vector a : (n, _) t) =
    Array.iteri f a

  let iter2 (type n) f (Vector a : (n, _) t) (Vector b : (n, _) t) =
    Array.iter2 f a b

  let fold_left (type n) f acc (Vector a : (n, _) t) =
    Array.fold_left f acc a

  let fold_right (type n) f (Vector a : (n, _) t) acc =
    Array.fold_right f a acc

  let fold_left2 (type n) f acc (Vector a : (n, _) t) (Vector b : (n, _) t) =
    let acc = ref acc in
    for i = 0 to Array.length a - 1 do
      acc := f !acc (Array.unsafe_get a i) (Array.unsafe_get b i)
    done;
    !acc

  let fold_right2 (type n) f (Vector a : (n, _) t) (Vector b : (n, _) t) acc =
    let acc = ref acc in
    for i = Array.length a - 1 downto 0 do
      acc := f (Array.unsafe_get a i) (Array.unsafe_get b i) !acc
    done;
    !acc

  let to_list (type n) (Vector a : (n, _) t) =
    Array.to_list a

  let sort (type n) cmp (Vector a : (n, _) t) =
    Array.sort cmp a

  let of_array (type n) (Cardinal n : n cardinal) a : (n, _) t =
    let n = Lazy.force n
    and m = Array.length a in
    if n = m then
      Vector a
    else
      sprintf
        "Fix.Indexing.Vector.of_array: \
         cardinal and array length do not match (%d <> %d).\n%s\n"
        n m __LOC__
      |> invalid_arg

  let invert (type m) (type n) (n : n cardinal) (v : (m, n index) t) : (n, m index option) t =
    (* We assume that [v] represents an injection of [m] into [n].
       We want to tabulate the inverse function [w], a partial function. *)
    let w = make n None in
    v |> iteri @@ begin fun i j ->
      assert (get w j = None);
      set w j (Some i);
    end;
    w

  module type V = sig
    type n
    type a
    val vector : (n, a) t
  end

  module Of_array (A : sig type a val array : a array end) = struct
    type n = unit
    type a = A.a
    let vector = Vector A.array
  end

end

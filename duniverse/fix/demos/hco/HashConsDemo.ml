open Fix

(* We first define a type of skeletons, then close the recursive knot
   to obtain a type of (hash-consed) trees. *)

module MySkeleton = struct

  (* As an example, we use a skeleton that describes binary trees with
     an integer payload in every node. This is an arbitrary example. *)

  type 'a t =
    | Leaf
    | Node of int * 'a * 'a

  (* Equality of skeletons. *)

  let equal equal sk1 sk2 =
    match sk1, sk2 with
    | Leaf, Leaf ->
        true
    | Node (x1, l1, r1), Node (x2, l2, r2) ->
        x1 = x2 && equal l1 l2 && equal r1 r2
    | Node _, Leaf
    | Leaf, Node _ ->
        false

  (* Hashing of skeletons. *)

  let hash hash sk =
    match sk with
    | Leaf ->
        0
    | Node (x, l, r) ->
        x + hash l + hash r

  (* The size of a skeleton. *)

  let size size sk =
    match sk with
    | Leaf ->
        1
    | Node (_, l, r) ->
        1 + size l + size r

end

type tree =
  skeleton HashCons.cell

and skeleton =
  S of tree MySkeleton.t [@@unboxed]

(* We have a choice between [HashCons.ForHashedType] and
   [HashCons.ForHashedTypeWeak]. *)

module M =
  HashCons.ForHashedTypeWeak(struct
    type t = skeleton
    let equal (S sk1) (S sk2) =
      MySkeleton.equal HashCons.equal sk1 sk2
    let hash (S sk) =
      MySkeleton.hash HashCons.hash sk
  end)

let leaf () : tree =
  M.make (S MySkeleton.Leaf)

let node x l r : tree =
  M.make (S (MySkeleton.Node (x, l, r)))

(* This [size] function computes the size of the underlying tree;
   a DAG node can be visited several times! *)

let rec size (t : tree) : int =
  let S sk = HashCons.data t in
  MySkeleton.size size sk

let example() =
  node 0
    (leaf())
    (leaf())

let () =
  assert (example() == example());
  Printf.printf "Size of example tree is %d.\n" (size (example()));
  print_endline "Success."

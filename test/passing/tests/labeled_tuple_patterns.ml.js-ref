(* This file is a copy of a labeled tuples test from the compiler.  Not everything here
   typechecks, but everything should parse. *)

(* Test match statements with exception patterns *)

exception Odd

let x_must_be_even (~x, y) = if x mod 2 = 1 then raise Odd else ~x, y

let foo xy k_good k_bad =
  match x_must_be_even xy with
  | ~x, y -> k_good ()
  | exception Odd -> k_bad ()
;;

(* Test correctness *)
let _ = foo (~x:2, 5) (fun () -> true) (fun () -> false)
let _ = foo (~x:3, 5) (fun () -> false) (fun () -> true)

(* Test that the actions occur outside of the exception handler *)
let _ =
  try foo (~x:2, 5) (fun () -> raise Odd) (fun () -> false) with
  | Odd -> true
;;

let _ =
  try foo (~x:3, 5) (fun () -> false) (fun () -> raise Odd) with
  | Odd -> true
;;

(* Labeled tuple pattern *)
let ~x:x0, ~y:y0, _ = ~x:1, ~y:2, "ignore me"

(* Pattern with punning and type annotation *)
let ~(x : int), ~y, _ = ~x:1, ~y:2, "ignore me"

(* Patterns in functions *)
let f (~foo, ~bar) = (foo * 10) + bar
let bar = 5
let _ = f (~foo:1, ~bar)

(* Correct annotation *)
let f : (foo:int * bar:int) -> int = fun (~foo, ~bar) -> (foo * 10) + bar
let f (~foo, ~bar) : foo:int * bar:int = (foo * 10) + bar

(* Missing label *)
let f : int * bar:int -> int = fun (~foo, ~bar) -> (foo * 10) + bar
let f (~foo, ~bar) : foo:int * int = (foo * 10) + bar

(* Wrong label *)
let f : (foo:int * foo:int) -> int = fun (~foo, ~bar) -> (foo * 10) + bar

(* Wrong type *)
let f : (foo:float * foo:int) -> int = fun (~foo, ~bar) -> (foo * 10) + bar

(* Annotated pattern *)
let f ((~x, y) : x:int * int) : int = x + y

(* Misannotated pattern *)
let f ((~x, y) : int * int) : int = x + y
let f ((~x, y) : int * x:int) : int = x + y

(* Annotation within pattern *)
let f ((~(x : int), y) : x:int * int) : int = x + y
let f (~(x : int), y) = x + y
let f (~x:(x0 : int), y) = x0 + y

(* Misannotation within pattern *)
let f (~(x : float), y) = x + y

(* Reordering in functions *)
type xy = x:int * y:int
type yx = y:int * x:int

let xy_id (pt : xy) = pt
let yx_id (pt : yx) = pt
let xy_id (~y, ~x) : xy = ~x, ~y
let swap (~x, ~y) = ~y, ~x
let swap ((~y, ~x) : xy) = ~y, ~x
let swap (~x, ~y) : yx = ~x, ~y
let swap (pt : xy) : yx = pt
let swap : xy -> yx = Fun.id
let swap : xy -> yx = xy_id
let swap : xy -> yx = yx_id

(* Reordering and partial matches *)
let lt = ~x:1, ~y:2, ~x:3, 4

(* Full match, in order *)
let matches =
  let ~x, ~y, ~x:x2, z = lt in
  x, y, x2, z
;;

(* Full match, over-bound *)
let matches =
  let ~x, ~y, ~x, z = lt in
  x, y, z
;;

(* Full match, missing label *)
let matches =
  let ~x, ~y, z = lt in
  x, y, z
;;

(* Full match, wrong label *)
let matches =
  let ~x, ~y, ~w, z = lt in
  x, y, z
;;

(* Full match, extra label *)
let matches =
  let ~x, ~y, ~x, ~y, z = lt in
  x, y, z
;;

(* Full match, extra unlabeled label *)
let matches =
  let ~x, ~y, ~x, z, w = lt in
  x, y, z
;;

(* Partial match *)
let matches =
  let ~x, ~y, .. = lt in
  x, y
;;

(* Partial match, reordered *)
let matches =
  let ~y, ~x, .. = lt in
  x, y
;;

(* Partial match, reordered, over-bound *)
let matches =
  let ~y:x, ~x, .. = lt in
  x
;;

(* Partial match one *)
let matches =
  let ~x, .. = lt in
  x
;;

(* Partial match all *)
let matches =
  let ~x, ~y, ~x:x2, z, .. = lt in
  x, y, x2, z
;;

(* Partial match too many of a name *)
let matches =
  let ~y, ~y:y2, ~x, .. = lt in
  x, y
;;

(* Partial match bad name *)
let matches =
  let ~w, ~y, ~x, .. = lt in
  x, y, x2, z
;;

(* Nested pattern *)
let f (z, (~y, ~x)) = x, y, z

(* Non-principally known patterns *)

let f (z, (~y, ~x, ..)) = x, y, z
let f (~x, ~y, ..) = x, y

(* Labeled tuples nested in records *)

let x = ref (~x:1, ~y:2, ~x:3, 4)

(* Good match *)
let _1234 =
  match x with
  | { contents = ~x:x0, ~y, ~x, z } -> x0, y, x, z
;;

(* Good partial match *)
let _1 =
  match x with
  | { contents = ~x, .. } -> x
;;

(* Wrong label *)
let () =
  match x with
  | { contents = ~w, .. } -> w
;;

(* Missing unordered label *)
let () =
  match x with
  | { contents = ~x:x0, ~y, ~x } -> y
;;

(* Extra unordered label *)
let () =
  match x with
  | { contents = ~x:x0, ~y, ~x, w1, w2 } -> y
;;

(* Extra unordered label, open *)
let () =
  match x with
  | { contents = ~x:x0, ~y, ~x, w1, w2, .. } -> y
;;

(* Missing label *)
let () =
  match x with
  | { contents = ~x:x0, ~y, x } -> y
;;

(* Extra label *)
let () =
  match x with
  | { contents = ~y:y0, ~y, ~x } -> y
;;

(* Behavior w.r.t whether types are principally known *)

let f (z : x:_ * y:_) =
  match z with
  | ~y, ~x -> x + y
;;

let f = function
  | ~x, ~y -> x + y
;;

let g z =
  ( f z
  , match z with
    | ~y, ~x -> x + y )
;;

let f = function
  | ~x, ~y -> x + y
;;

let g z =
  match z with
  | ~y, ~x -> x + y, f z
;;

(* More re-ordering stress tests *)
type t = x:int * y:int * int * x:int * x:int * y:int * y:int * int * int * y:int * x:int

let t : t = ~x:1, ~y:2, 3, ~x:4, ~x:5, ~y:6, ~y:7, 8, 9, ~y:10, ~x:11

let _ =
  let ~y, ~y:y2, ~y:y3, .. = t in
  y, y2, y3
;;

let _ =
  let a, b, c, .. = t in
  a, b, c
;;

let _ =
  let n3, ~y:n2, ~y, ~x:n1, .. = t in
  n1, n2, n3, y
;;

let _ =
  let ~x:x1, ~x:x2, ~x:x3, ~x, .. = t in
  x1, x2, x3, x
;;

let _ =
  let ~y:n2, ~y:n6, n3, ~x:n1, ~y:n7, n8, ~y:n10, ~x:n4, ~x:n5, ~x:n11, n9 = t in
  n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11
;;

let _ =
  let n3, n8, n9, ~y:n2, ~y:n6, ~y:n7, ~y:n10, ~x:n1, ~x:n4, ~x:n5, ~x:n11 = t in
  n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11
;;

let _ =
  let ~x:n1, ~y:n2, n3, ~x:n4, ~x:n5, ~y:n6, ~y:n7, n8, n9, ~y:n10, ~x:n11 = t in
  n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11
;;

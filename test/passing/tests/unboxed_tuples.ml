(* This test file is just a copy of labeled_tuples.ml and
   labeled_tuples_regressions.ml, with less labels, and some additional
   regression tests at the bottom. *)

(* Basic expressions *)
let x = #(1, 2)

let z = 5

let punned = 2

let _ = #(~x:5, 2, ~z, ~(punned : int))

(* Basic annotations *)
let (x : #(int * int)) = #(1, 2)

let (x : #(x:int * int)) = #(~x:1, 2)

(* Incorrect annotations *)
let (x : #(int * int)) = #(~x:1, 2)

let (x : #(x:string * int)) = #(~x:1, 2)

let (x : #(int * y:int)) = #(~x:1, 2)

(* Happy case *)
let foo b = if b then #(~a:"s", 10, ~c:"hi") else #(~a:"5", 10, ~c:"hi")

(* Missing label (the type vars in the error aren't ideal, but the same thing
   happens when unifying normal tuples of different lengths) *)
let foo b = if b then #(~a:"s", 10, "hi") else #(~a:"5", 10, ~c:"hi")

(* Missing labeled component *)
let foo b = if b then #(~a:"s", 10) else #(~a:"5", 10, ~c:"hi")

(* Wrong label *)
let foo b = if b then #(~a:"s", 10, ~a:"hi") else #(~a:"5", 10, ~c:"hi")

(* Types in function argument/return *)
let default = #(~x:1, ~y:2)

let choose_pt replace_with_default pt =
  if replace_with_default then default else pt

(* Application happy case *)
let a = choose_pt true #(~x:5, ~y:6)

(* Wrong order *)
let a = choose_pt true #(~y:6, ~x:5)

(* Mutually-recursive definitions *)
let rec a = #(1, ~lbl:b)

and b = #(2, ~lbl:a)

let rec l = #(~lbl:5, ~lbl2:10) :: l

(* Tuple containing labeled tuples *)
let tup = #(#(1, 2), #(~b:3, ~a:4), 5)

(* Polymorphic variant containing labeled tuple *)
let a = `Some #(~a:1, ~b:2, 3)

(* List of labeled tuples *)
let lst = #(~a:1, ~b:2) :: []

(* Ref of labeled tuple *)
let x = ref #(~x:"hello", 5)

(* Polymorphic record containing a labeled tuple *)
type 'a box = {thing: 'a}

let boxed = {thing= #("hello", ~x:5)}

(* Punned tuple components with type annotations. *)
let x = 42

let y = "hi"

let z = #(~x, ~(y : string))

let z = #(~(x : int), ~y:"baz")

let z = #((x : string), ~y:"baz")

(* Take a [a:'a * b:'a] and an int, and returns a [swapped:[a:'a * b:'a] *
   same:bool]. The swapped component is the input with the [a] and [b]
   components swapped as many times as the input int. The second component is
   whether the first equals the input. *)
let rec swap #(~a, ~b) = function
  | 0 -> #(~swapped:#(~a, ~b), ~same:true)
  | n -> swap' #(~a:b, ~b:a) (n - 1)

and swap' #(~a, ~b) = function
  | 0 -> #(~swapped:#(~a, ~b), ~same:false)
  | n -> swap #(~a:b, ~b:a) (n - 1)

let foobar = swap #(~a:"foo", ~b:"bar") 86

let barfoo = swap #(~a:"foo", ~b:"bar") 87

(* Labeled tuple type annotations *)
(* Bad type *)
let x : #(string * a:int * int) = #(~lbl:5, "hi")

(* Well-typed *)
let x : #(string * a:int * int) = #("hi", ~a:1, 2)

(* Function type *)
let mk_x : #(foo:unit * bar:unit) -> #(string * a:int * int) = fun _ -> x

let x = mk_x #(~foo:(), ~bar:())

(* Labeled tuples in records *)

type bad_t = {x: #(lbl:bad_type * int)}

type tx = {x: #(foo:int * bar:int)}

type tx_unlabeled = {x: #(int * int)}

let _ = {x= #(~foo:1, ~bar:2)}

let _ : tx = {x= #(~foo:1, ~bar:2)}

let _ : tx = {x= #(1, ~bar:2)}

let _ : tx = {x= #(~foo:1, 2)}

let _ : tx = {x= #(1, 2)}

let _ = {x= #(1, 2)}

(* Module inclusion *)

module IntString : sig
  type t

  val mk : #(x:int * string) -> t

  val unwrap : t -> #(x:int * string)
end = struct
  type t = #(string * x:int)

  let mk #(~x, s) = #(s, ~x)

  let unwrap #(s, ~x) = #(~x, s)
end

module Stringable = struct
  module type Has_unwrap = sig
    type t

    val unwrap : t -> #(x:int * string)
  end

  module type Has_to_string = sig
    include Has_unwrap

    val to_string : t -> string
  end

  module Make (M : Has_unwrap) : Has_to_string with type t := M.t = struct
    include M

    let to_string int_string =
      let #(~x, s) = unwrap int_string in
      Int.to_string x ^ " " ^ s
  end
end

module StringableIntString = struct
  include IntString
  include functor Stringable.Make
end

let _ = StringableIntString.to_string (StringableIntString.mk #(~x:1, "hi"))

module M : sig
  val f : #(x:int * string) -> #(x:int * string)

  val mk : unit -> #(x:bool * y:string)
end = struct
  let f x = x

  let mk () = #(~x:false, ~y:"hi")
end

(* Module inclusion failure *)
module X_int_int = struct
  type t = #(x:int * int)
end

module Y_int_int : sig
  type t = #(y:int * int)
end = struct
  include X_int_int
end

module Int_int : sig
  type t = #(int * int)
end =
  X_int_int

(* Recursive modules *)
module rec Tree : sig
  type t = Leaf of string | Branch of string * TwoTrees.t

  val in_order : t -> string list
end = struct
  type t = Leaf of string | Branch of string * TwoTrees.t

  let rec in_order = function
    | Leaf s -> [s]
    | Branch #(s, #(~left, ~right)) -> in_order left @ [s] @ in_order right
end

and TwoTrees : sig
  type t = #(left:Tree.t * right:Tree.t)
end = struct
  type t = #(left:Tree.t * right:Tree.t)
end

let leaf s = Tree.Leaf s

let tree_abc = Tree.Branch ("b", #(~left:(leaf "a"), ~right:(leaf "c")))

let tree_abcde = Tree.Branch ("d", #(~left:tree_abc, ~right:(leaf "e")))

let _ = Tree.in_order tree_abcde

(* Motivating example *)
let two_kinds_of_sums ints =
  let init = #(~normal_sum:0, ~absolute_value_sum:0) in
  List.fold_left
    (fun #(~normal_sum, ~absolute_value_sum) elem ->
      let normal_sum = elem + normal_sum in
      let absolute_value_sum = abs elem + absolute_value_sum in
      #(~normal_sum, ~absolute_value_sum) )
    init ints

let _ = two_kinds_of_sums [1; 2; 3; 4]

let _ = two_kinds_of_sums [1; 2; -3; 42; -17]

let x = #(~x:1, ~y:2)

(* Attribute should prevent punning *)
let z = 5

let y = #(~z, ~z, ~z:(z [@attr]))

let #(~x:x0, ~s, ~(y : int), ..) : #(x:int * s:string * y:int * string) =
  #(~x:1, ~s:"a", ~y:2, "ignore me")

(* Constructor with labeled arguments (disallowed) *)

type ('a, 'b) pair = Pair of 'a * 'b

(* Labeled tuple pattern in constructor pattern, with the same arity as the
   constructor. This is intentionally disallowed. *)
let f = function Pair #(~x:5, 2) -> true | _ -> false

(* Labeled tuple patterns in constructor patterns with that can union with
   the constructor pattern type. *)
let f = function Some #(~x:5, 2) -> true | _ -> false

type t = Foo of #(x:int * int)

let f = function Foo #(~x:5, 2) -> true | _ -> false

let f = function #((A | B), C) -> ()

let x = #((fun () -> ()), fun () -> ())

let x = #((if y then 1 else 2), if y then 1 else 2)

let x = #((match y with () -> ()), match y with () -> ())

let _ =
  #( abcdefghijklmnopqrstuvwxyz
   , abcdefghijklmnopqrstuvwxyz
   , abcdefghijklmnopqrstuvwxyz
   , abcdefghijklmnopqrstuvwxyz
   , abcdefghijklmnopqrstuvwxyz )

let _ =
  match () with
  | #( abcdefghijklmnopqrstuvwxyz
     , abcdefghijklmnopqrstuvwxyz
     , abcdefghijklmnopqrstuvwxyz
     , abcdefghijklmnopqrstuvwxyz
     , abcdefghijklmnopqrstuvwxyz ) ->
      ()

type t =
  t
  * ( abcdefghijklmnopqrstuvwxyz
    * abcdefghijklmnopqrstuvwxyz
    * abcdefghijklmnopqrstuvwxyz
    * abcdefghijklmnopqrstuvwxyz
    * abcdefghijklmnopqrstuvwxyz )

type t =
  t
  * #( abcdefghijklmnopqrstuvwxyz
     * abcdefghijklmnopqrstuvwxyz
     * abcdefghijklmnopqrstuvwxyz
     * abcdefghijklmnopqrstuvwxyz
     * abcdefghijklmnopqrstuvwxyz )

let x = match foo with #(Some x, ~l:(Some y)) -> ()

let foo a =
  match a with
  | #( None
     , Some _
     , [1; 2]
     , 3 :: []
     , {x: _; y: _}
     , 42
     , _
     , `Baz
     , `Bar _
     , (1 | 2)
     , [|1; 2|]
     , (3 : int)
     , (lazy _)
     , ( module M )
     , (exception _)
     , [%bar baz]
     , M.(A)
     , M.(A 42) ) ->
      false

let bar =
  #( foo
   , 42
   , (let x = 18 in
      x )
   , (function x -> x)
   , (fun x -> x)
   , foo 42
   , (match () with () -> ())
   , (try () with _ -> ())
   , (1, 2)
   , (~x:1, ~y:2)
   , None
   , Some 42
   , `A
   , `B 42
   , {x= 42; z= false}
   , foo.lbl
   , (foo 42).lbl
   , (foo.lbl <- 42)
   , [|1; 2|]
   , [:1; 2:]
   , [1; 2]
   , [a for a = 1 to 10]
   , (if true then true else false)
   , (() ; ())
   , while true do
       ()
     done
   , for i = 1 to 2 do
       ()
     done
   , (42 : int)
   , (42 :> int)
   , (42 : int :> bool)
   , foo#bar
   , foo #~# bar
   , new M.c
   , (x <- 2)
   , {<x = 42; y = false>}
   , (let module M = N in
     () )
   , (let exception Ex in
     () )
   , assert true )

(* Labeled tuples in function return positions: Parens are needed iff the
   first element is labeled AND the return is `local_` *)
module type S = sig
  val t1 : unit -> #(int * y:bool)

  val t2 : unit -> local_ #(int * y:bool)

  val t3 : unit -> #(x:int * y:bool)

  val t4 : unit -> local_ #(x:int * y:bool)

  val f :
       #(foo:int * very_long_type_name_so_we_get_multiple_lines)
    -> (* cmt *)
       bar:('long_type_var_1, 'long_type_var_2) Long_module_name.t
    -> additional_somewhat_long_type_name
end

let _ =
  match w with
  | A -> #([], A.(B (C (f x))), None, f x y, g y x)
  | B -> #(a, b, c, d, e, f)
  | C ->
      #( []
       , A.(B (C (this is very looooooooooooooooooooooooooooooooooooong x)))
       , None
       , f x y
       , g y x )

let _ = [%ext #(1, 2, 3)]

let _ =
  [%ext
    #( loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong
     , loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong
     , 3 )]

type t = int [@@deriving #(1, 2, 3)]

type t = int
[@@deriving
  #( sexp
   , compare
   , loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong
   )]

let _ =
  #( 1
   , 2
   , looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong
   , looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong
   )

let _ = #(1, 2, 3, short) ;;

#( 1
 , 2
 , looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong
 , looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong
 )
;;

#(1, 2, 3, short)

(* make sure to not drop parens for local open. *)
let _ = A.(#(1, 2))

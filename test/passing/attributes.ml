let _ = (function[@warning "-4"] None -> true | _ -> false) None

let f (x[@warning ""]) = ()

let v = (fun [@inline] x -> x) 1

external f : (float[@unboxed]) -> int = "blah" [@@noalloc]

val x : ?x:unit (** not dropped *) -> unit

type t =
  { a: int
  ; b: int [@default 1] [@drop_if]
  ; c: int [@default 1] [@drop_if]
        (** docstring that is long enough to break *) }

type t =
  { a: int
  ; b: someloooooooooooooooooooooooooooooong typ
        [@default looooooooooooooooooooooooooooooooooooooooong]
        [@drop_if somethingelse]
  ; b: somelong typ [@default 1]
  ; c: someloooooooooooooooooooooooooooooong typ
        [@default looooooooooooooooooooooooooooooooooooooooong]
        [@drop_if somethingelse]
        (** docstring that is long enough to break *) }

val foo : int
  [@@deprecated "it is good the salad"] [@@warning "-32"] [@@warning "-99"]

val foo : int
  [@@deprecated "it is good the salad"]
  [@@warning "-32"]
  [@@warning "-99"]
  [@@some long comment]

type t = A of int [@attr] | B of (float[@attr]) | C [@attr]

type t = [`A of int [@attr] | `B of (float[@attr]) | `C [@attr]]

let[@inline always] f x =
  let[@something] e = 1 in
  e

module type M = S [@test1]

module type M = sig
  module T (T : sig end) : (S with type t = r [@test2])

  module T (S : S [@test]) : S

  module T : (S with type t = (r[@test3]) [@test4])

  module T :
    (S
      with type t = t
       and type u := u
       and module R = R
       and module S := S
     [@test])

  module T : module type of X [@test5]

  module T : (module type of X) [@test6]

  module T : [%ext] [@test7]

  module T = T [@@test8]
end

let f = fun [@inline] [@inline never] x -> x

let g = fun [@inline] [@something_else] [@ocaml.inline] x -> x

let h x = (g [@inlined] [@ocaml.inlined never]) x

let v = (fun [@inline] [@inlined] x -> x) 1

let[@inline] i = fun [@inline] x -> x

;;
if [@test] true then () else ()

;;
if [@test] true then () else if [@test] true then () else ()

let _ = ((A [@test]), (() [@test]), ([] [@test]), [||] [@test])

type blocklist =
  { f1: int [@version 1, 1, 0]  (** short comment *)
  ; f2: (int64 * int64) list
        (** loooooooooooooooooooooooooooooong
            commmmmmmmmmmmmmmmmmmmmmmmmmmmmmmment *) }

type blocklist =
  | F1 of int [@version 1, 1, 0]  (** short comment *)
  | F2 : int -> blocklist [@version 1, 1, 0]  (** short comment *)
  | F3 of (int64 * int64) list
      (** loooooooooooooooooooooooooooooong
          commmmmmmmmmmmmmmmmmmmmmmmmmmmmmmment *)

type u =
  | C of int * int
      [@doc
        [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. "
        ; "Etiam vel mauris fermentum, condimentum quam a, porta nisi" ]]
[@@deriving something]
[@@doc ["Ut at dolor a eros venenatis maximus ut at nisi."]]

let ((A, B)[@test]) = ()

let ((lazy a)[@test]) = ()

let ((exception a)[@test]) = ()

let ((B x)[@test]) = ()

let ((`B x)[@test]) = ()

let (B[@test]) = ()

let (`B[@test]) = ()

let (B.(A)[@test]) = ()

let ('x' .. 'z'[@test]) = ()

let (#test[@test]) = ()

let ((module X)[@test]) = ()

let (a[@test]) = ()

let (_[@test]) = ()

let (""[@test]) = ()

let _ = f x ~f:(fun [@test] x -> x)

let _ = f x ~f:(function [@test] x -> x)

let _ = f x ~f:(function [@test] X -> x | X -> x)

let () = ()

and[@warning "-32"] f = ()

external x : a -> b -> (a -> b[@test]) = ""

let f = fun [@test] x y -> ()

let f y = fun [@test] y -> ()

let (f[@test]) = fun y -> fun [@test] y -> ()

module type T = sig
  class subst :
    ((ident -> ident)[@attr])
    -> (ident -> ident)
    -> object
         inherit mapper
       end[@attr]
end

let _ = fun [@inlined always] x y -> z

let () = assert ((assert false [@imp Show]) 1.0 = "1.")

let () = f (assert false)

let _ = match x with A -> [%expr match y with e -> e]

let _ =
  match x with A -> [%expr match y with e -> ( match e with x -> x )]

type t = {a: int}
[@@deriving xxxxxxxxxxxxxxxxxxxxxxxxxxx]
(* comment *)
[@@deriving xxxxxxxxxxxxxxxxxxxxxxxxxxx]

module type A = sig
  module A := A.B [@@attr]
end

module M = struct
  type t
  [@@immediate]
  (* ______________________________________ *)
  [@@deriving variants, sexp_of]
end

let _ = {<>} [@a]

let _ = f ({<>} [@a])

let _ = {<x = 1>} [@a]

let _ = f ({<x = 1>} [@a])

let _ = (x :> t) [@a]

let _ = f ((x :> t) [@a])

let _ = (module M) [@a]

let _ = f ((module M) [@a])

let _ = (module M : S) [@a]

let _ = f ((module M : S) [@a])

let _ = ([] @ []) [@a]

let _ = ("" ^ "") [@a]

let _ = (0 + 0) [@a]

let _ = (a.x <- 1) [@a]

let _ = f ((a.x <- 1) [@a])

let _ =
  object
    method g = (a <- b) [@a]

    method h = f ((a <- b) [@a])

    method i =
      (a <- b) [@a] ;
      ()
  end

let _ = a.(b) [@a]

let _ = f (a.(b) [@a])

let _ = (a.*?!@{b} <- c) [@a]

let _ = f ((a.*?!@{b} <- c) [@a])

(* Regression tests for https://github.com/ocaml-ppx/ocamlformat/issues/1256
   (dropped parentheses around tuples with attributes). *)

;;
(0, 0) [@a]

let _ = ((0, 0) [@a])

let _ = f ((0, 0) [@a])

(* Ensure that adding an attribute doesn't break left-alignment of tuple
   components *)

;;
( a________________________________________
, b________________________________________ ) [@a]

let _ =
  f
    (( a________________________________________
     , b________________________________________ ) [@a])

let _ = a [@a] ; b

let _ = f (a [@a] ; b)

let _ = a ; b [@a]

let _ = f (a ; b [@a])

let _ = (a ; b) [@a]

let _ = f ((a ; b) [@a])

let _ = a ; b [@a] ; c

let _ =
  a ;
  (b1 ; b2) [@a]

let _ =
  a ;
  (b1 ; b2) [@a] ;
  c

(* Ensure that adding an attribute doesn't break left-alignment of sequenced
   expressions *)
let _ =
  (aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa ;
   bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb) [@a]

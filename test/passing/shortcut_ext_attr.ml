(* Expressions *)
let () =
  let%foo[@foo] x = 3 and[@foo] y = 4 in
  [%foo
    (let module M = M in
    ()) [@foo]] ;
  [%foo M.(()) [@foo]] ;
  [%foo fun [@foo] x -> ()] ;
  [%foo function[@foo] x -> ()] ;
  [%foo try[@foo] () with _ -> ()] ;
  [%foo if [@foo] () then () else ()] ;
  [%foo
    ( while () do
        ()
      done ) [@foo]] ;
  [%foo
    ( for x = () to () do
        ()
      done ) [@foo]] ;
  () ;%foo
  () ;
  [%foo (assert true) [@foo]] ;
  [%foo (lazy x) [@foo]] ;
  [%foo (object end) [@foo]] ;
  [%foo (3) [@foo]] ;
  [%foo (new x) [@foo]] ;
  [%foo
    match[@foo] () with
    | [%foo ?  (* Pattern expressions *)
        (lazy x[@foo])] -> ()
    | [%foo ? (exception x[@foo])] -> ()]

(* Class expressions *)
class x =
  (fun x ->
    let[@foo] x = 33 in
    object
      inherit x [@@foo]
      val x = 333 [@@foo]
      val virtual x : t [@@foo]
      val! mutable x = 3 [@@foo]
      method x = 3 [@@foo]
      method virtual x : t [@@foo]
      method! private x = 3 [@@foo]
      initializer x [@@foo]
    end [@foo] ) [@foo]
  

(* Class type expressions *)
class type t =
  object
    inherit t [@@foo]
    val x : t [@@foo]
    val mutable x : t [@@foo]
    method x : t [@@foo]
    method private x : t [@@foo]
    constraint t = t' [@@foo]
  end[@foo] 

(* Type expressions *)
type t = [%foo : ((module M)[@foo])]

(* Module expressions *)
module M = (functor [@foo] (M : S) -> (val x) [@foo] (struct end [@foo]))

(* Module type expression *)
module type S = functor [@foo1] (M :
  S) -> functor (_ :(module type of 
  M)
[@foo2]) -> sig end [@foo3]

(* Structure items *)
let%foo x = 4[@@foo]

and y = x[@@foo]

[%%foo
type t = int [@@foo]

and t = int [@@foo]]

[%%foo
type t += T [@@foo]]

[%%foo
class x =
  x
  [@@foo]]

[%%foo
class type x =
  x [@@foo]]

[%%foo
external x : _ = ""  [@@foo]]

[%%foo
exception X [@@foo]]

[%%foo
module M = M [@@foo]]

[%%foo
module rec M : S = M [@@foo]
and M : S = M [@@foo]]

[%%foo
module type S = S [@@foo]]

[%%foo
include M [@@foo]]

[%%foo
open M [@@foo]]

(* Signature items *)
module type S = sig
  [%%foo : val x : t  [@@foo]] 

  [%%foo : external x : t = ""  [@@foo]] 

  [%%foo : type t = int [@@foo]
 and t' = int [@@foo]] 

  [%%foo : type t += T [@@foo]] 

  [%%foo : exception X [@@foo]] 

  [%%foo : module M : S [@@foo]] 

  [%%foo : module rec M : S [@@foo]
           and M : S [@@foo]] 

  [%%foo : module M = M [@@foo]] 

  [%%foo : module type S = S [@@foo]] 

  [%%foo : include M[@@foo]] 

  [%%foo : open M [@@foo]] 

  [%%foo
  : class x :
      t [@@foo]]
  

  [%%foo
  : class type x =
      x [@@foo]]
  
end

module M : sig
  (** M *)

  type 'a t

  class c :
    'a t
    -> l:'a t
    -> ?k:'a t
    -> object
         [@@@attr]
       end

  module type S = sig
    include module type of struct end
  end

  module M : functor (X : module type of N with type t = t) () ->
    S with type t = t and module N = N
end = struct
  type t =
    | A  (** A *)
    | B : int * int -> t
    | C of {a: int  (** a *); b: int  (** b *)}
    constraint
      'a =
      [> `A | b] * [< `A > `B `C] * < m: t ; .. > * (module S) * t #u as 'a

  (** f *)
  let f : 'a. [%id] t = f (fun X -> assert false)

  module M (X : S) () = struct end
end

let _ =
  (* Insert every expressions in sequence here *)
  (module M.N (X.Y) : S) ;
  let rec x = x and (lazy _) = y in
  f
    (function
      (* Insert every patterns here *)
      | (((x : t), _ | a, b | A, B x | `A, `B x | #t) as x)
       |{a= _; b= _; _}
       |[|a; b|]
       |A | B
       |(module M)
       |(module M : S)
       |(module _)
       |(exception E)
       |[%ppx]
       |M.(A | B)
       |{x= (module M : S)}
       |{x= (x' : t)}
       |{x= (P : t)}
       |{x= ((x' : t)[@attr])} ->
          . )
    (fun (type t) X ~a ~b:Y ?c ?c:(Z = W) {a; _} -> ())
    X ~a ~b:(x y) ?c ?c:(Some x)
    {a; b= (u :> t); c: t; d= (module M : S)}
    (try a.x <- b.x with Failure msg -> msg)
    [|a; b + c + d|] ;
  if x.{a, b} then y.*(0) else z.*(a; b) ;
  for i = f x to f y do
    r := x.(i)
  done ;
  (f x)#m (new M.c) {<x = (f x : t); y = (x : t); z>} ;
  let module M = (val S.(M.N X.Y).x) in
  let exception E of t in
  let open! M in
  let* x = ~-y and* z = (w [@attr]) in
  lazy (( let* ) (function X -> ( + )) (( * ) [@attr])) ;
  1 :: ~-2 ;
  [1; 2]

class virtual c x ~y ?z:(z' = 0) =
  let open M in
  object (self)
    inherit M.c x

    val mutable y = 0

    initializer y <- x

    method m a = a - y

    method n = self#m {<>}

    method virtual o : #ct -> int
  end

type t = ..

type t += X : t -> t

exception E of t

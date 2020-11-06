module M : sig
  (** M *)

  type t

  class c :
    'a t
    -> l:'a t
    -> ?k:'a t
    -> object
         [@@@attr]
       end
end = struct
  type t =
    | A  (** A *)
    | B : int * int -> t
    | C of {a: int  (** a *); b: int  (** b *)}

  (** f *)
  let f (_ : t) = ()
end

let _ =
  (* Insert every expressions in sequence here *)
  x ;
  (module M.N (X.Y) : S) ;
  let rec x = x in
  let _ = x and (lazy _) = y in
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
   |M.(A | B) ->
      . ) ;
  (fun (type t) X ~a ~b:Y ?c ?c:(Z = W) {a; _} -> ())
    X ~a ~b:(x y) ?c ?c:(Some x)
    {a; b= (u :> t)}
    (try a.x <- b.x with Failure msg -> msg)
    [|a; (b ; c)|] ;
  if x then y else z ;
  for i = f x to f y do
    do_ i
  done ;
  (f x)#m (new M.c) {<x = f x>} ;
  let module M = (val S.(M.N X.Y).x) in
  let exception E of t in
  let open! M in
  let* x = y and* z = w in
  lazy x

class virtual c x ~y ?z:(z' = 0) =
  let open M in
  object (self)
    inherit M.c x

    val mutable y = 0

    initializer y <- x

    method m a = a - y

    method n = self#m

    method virtual o : #ct -> int
  end

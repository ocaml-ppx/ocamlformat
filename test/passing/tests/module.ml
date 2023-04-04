module AAAAAAAAAAAAAAAAAAA =
  Soooooooooooooooooooooooome.Loooooooooooooooooooooooong.Mod

let _ =
  let module A = B in
  let module AAAAAAAAAAAAAAAAAAA =
    Soooooooooooooooooooooooome.Loooooooooooooooooooooooong.Mod
  in
  t

let create (type a b) t i w p =
  let module T = (val (t : (a, b) t)) in
  T.create i w p

module C = struct
  module rec A : sig
    type t

    module rec B : sig
      type t

      type z
    end

    and A : B
  end =
    A

  and B : sig end = B
end

module O : sig
  type t
end
with type t := t = struct
  let () = ()
end

module O : sig
  type t
end
with type t := t
 and type s := s = struct
  let () = ()
end

include struct
  (* a *)
end

include A (struct
  (* a *)
end)

let x : (module S) = (module struct end)

let x = (module struct end : S)

module rec A : (sig
  type t
end
with type t = int) = struct
  type t = int
end

module A (_ : S) = struct end

module A : functor (_ : S) -> S' = functor (_ : S) -> struct end

let helper ?x =
  match x with Some (module X : X_typ) -> X.f | None -> X_add_one.f

let helper ?x:((module X) = (module X_add_one : X_typ)) = X.f

module GZ : functor (X : sig end) () (Z : sig end) -> sig end =
  (val Mooooooooooooooooooo)

module GZZZZZZZZZZZZZZ : functor (X : sig end) () (Z : sig end) -> sig end =
  _

module M = struct end

module M = F ()
module M = F (* xxx *) ( (* xxx *) ) (* xxx *)

module M = F (struct end)

module M = F (G) ()
module M = F (G) ( (* xxx *) )

module M = F (G) (struct end)

module M =
  F
    (struct
      val x : t

      val y : t
    end)
    ( (* struct type z = K.y end *) )

let _ =
  let module M =
    (val (* aa *) m (* bb *) : (* cc *) M (* dd *) :> (* ee *) N (* ff *))
  in
  let module M =
    ( val m
        : M with type t = k and type p = k
        :> N with type t = t and type k = t )
  in
  let module M =
    ( val (* aa *) m (* bb *)
        : (* cc *)
        M with type t = t (* dd *)
        :> (* ee *)
        N with type t = t (* ff *) )
  in
  ()

module M =
  [%demo
  module Foo = Bar

  type t]

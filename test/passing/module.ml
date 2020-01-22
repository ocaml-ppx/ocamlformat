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

let kk = (* foo *) (module A : T)

let kk = ((* foo *) (module A : T))

let kk = ((module A : T) (* foo *))

let kk = ((* foo *) (module A : T) (* foo *))

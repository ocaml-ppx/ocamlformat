(* All relevant attributes are copied. *)
external foo = Float.ldexp

(* All relevant attributes are copied. *)
external bar : float -> int -> float = Float.ldexp

external id : 'a. 'a -> 'a = "%identity"

(* We reject impossible types which are explicitly distinguishable. *)
type u = A
module type S = sig external x : u = id end

(* We reject impossible types from the initial environment. *)
module type S = sig external x : int = id end

(* Cannot define a self-referential primitive. *)
module rec A : sig
  external p = A.p
end = struct
  external p = A.p
end

(* Cannot define mutually-recursive primitives. *)
module rec B : sig
  external p = C.p
end = struct
  external p = C.p
end

and C : sig
  external p = B.p
end = struct
  external p = B.p
end

(* Cannot refer to primitives from other mutually-recursive modules. *)
module rec D : sig
  external p = E.p
end = struct
  external p = Obj.magic
end

and E : sig
  external p = Obj.magic
end = struct
  external p = D.p
end

external identity : 'a -> 'a = Obj.magic

(* Can't use a recursive module to generalize a primitive's type. *)
module rec F : sig
  external magic : 'a -> 'b = identity
end = F

(* As above, but with concrete types. *)
module rec G : sig
  type a
  type b

  external magic : a -> b = identity
end = G

external identity : 'a -> 'a = "%identity"

(* We use regular unification to check whether the requisite type equalities are
   possible, sometimes rejecting signatures in cases where it is possible to
   define a satisfactory structure. *)
module Int_backed : sig
  type t
  external to_int : t -> int = identity
end = struct
  type t = int
  external to_int : t -> int = identity
end

let id x = x

external identity = id

class c =
  object
    val id = id

    method identity =
      let open struct
        external identity = id
      end in
      identity
  end

class c =
  object (id)
    method identity =
      let open struct
        external identity = id
      end in
      identity
  end

class s = object end

class c =
  object
    inherit s as id

    method identity =
      let open struct
        external identity = id
      end in
      identity
  end

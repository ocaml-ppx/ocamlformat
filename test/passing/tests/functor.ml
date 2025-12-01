module type S = functor () -> sig end

module type S = functor () () -> sig end

module type M = functor () -> sig end

module type M = functor (S : S) -> sig end

module type M = functor (S : S) (T : T) -> sig end

module type M = functor (S : S) (T : T) -> U

module type M = functor (S : S) () -> sig end

module type M = functor
  (SSSSS : SSSSSSSSSSSSSS)
  (TTTTT : TTTTTTTTTTTTTTTT)
  -> sig
  val t1 : a

  val t2 : b
end

module M : functor () -> sig end = functor () -> struct end

module M = (functor (S : S) -> struct end) (S)

module M = (functor (S : S) (T : T) -> struct end) (S) (T)

module M = (functor (S : S) (T : T) -> struct end : U) (S) (T)

module M = (functor (S : S) () -> struct end : U) (S) (T)

module M = (functor (S : S) (T : T) -> (struct end : U)) (S) (T)

module rec A (S : S) = S

module type S = S -> S -> S
module type S = (S -> S) -> S
module type S = (functor (M : S) -> S) -> S
module type S = functor (M : S -> S) -> S
module type S = ((M : S) -> S) -> S
module type S = (M : S -> S) -> S

module M : X -> X = Y
module M : X -> (Y : T) -> (_ : T) -> Z = M
module M : X -> (Y : T) (_ : T) -> Z = M
module M : (X : T) -> T -> (_ : T) -> Z = M
module M : (_ : X) -> X = Y

module type S = sig
  module rec A : functor (S : S) -> S
end

module M =
  (functor
    (SSSSS : sssssSSSSSSSSSSSSSS)
    (TTTTT : TTTTTTTTTTTTTTTTTTTTT)
    ->
    struct
      let x = 2

      let y = 3
    end)
    (S)
    (T)

module type Module_type_fail = sig
  include S

  module F : functor (_ : T) -> sig end

  include S
end

module type KV_MAKER = functor (G : Irmin_git.G) (C : Irmin.Contents.S) ->
  S
    with type key = string list
     and type step = string
     and type contents = C.t
     and type branch = string
     and module Git = G

module Make
    (TT : TableFormat.TABLES)
    (IT :
      InspectionTableFormat.TABLES__________________________________________)
    (ET :
      EngineTypes.TABLE
        with type terminal = int
         and type nonterminal = int
         and type semantic_value = Obj.t)
    (E : sig
      type 'a env = (ET.state, ET.semantic_value, ET.token) EngineTypes.env
    end) =
struct
  type t = t
end

module Make
    (TT : TableFormat.TABLES)
    (IT :
      InspectionTableFormat.TABLES__________________________________________) =
struct
  type t = t
end

(* Long syntax should be preserved *)
module M = functor (_ : S) -> struct end

module M (_ : S) = struct end

module M : functor (_ : S) -> S' = functor (_ : S) -> struct end

module type SETFUNCTOR = (Elt : ORDERED_TYPE) -> sig end

module WrongSet : (Elt : ORDERED_TYPE) -> SET = Set

module M : (A : S) (B : S) -> S = N
module M : (A : S) -> (B : S) -> S = N
module M : functor (A : S) -> (B : S) -> S = N
module M : functor (A : S) -> functor (B : S) -> S = N
module M : functor (A : S) (B : S) -> S = N
module M : functor (A : S) -> functor (B : S) -> S = N
module M : (A : S) -> functor (B : S) -> S = N

module M : X -> X =
functor (X : X) -> struct
  let x = X.x
end

module M : (_ : X) -> X = Y

module M = struct
  [@@@ocamlformat "break-struct=natural"]

  module M = F ((struct type t end : sig type t end))

  module M = struct type t end

  module type S = sig type t end
end

module Simple: (Parameters with type update_result := state) -> S = M
module Simple: S -> (Parameters with type update_result := state) = M

module Left_variadic:
  (Parameters with type update_result := state * left array) -> S = M
module Left_variadic:
  S -> (Parameters with type update_result := state * left array) = M
module Left_variadic:
  (A -> B) ->
  (Parameters with type update_result := state * left array) -> S = M
module Left_variadic:
  S ->
  (Parameters with type update_result := state * left array) -> S = M
module Left_variadic:
  sig type t end ->
  (Parameters with type update_result := state * left array) -> S = M

module N : S with module type T = (U -> U) = struct end

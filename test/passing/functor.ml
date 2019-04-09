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

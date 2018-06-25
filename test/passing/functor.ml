module type S = functor () -> sig end

module type S = functor () () -> sig end

module type M = functor () -> sig end

module type M = functor (S :  S) -> sig end

module type M = functor (S :  S) (T :  T) -> sig end

module type M = functor (S :  S) (T :  T) -> U

module type M = functor (S :  S) () -> sig end

module type M = functor (SSSSS :  SSSSSSSSSSSSSS)
(TTTTT :  TTTTTTTTTTTTTTTT)
-> sig end

module M : functor () -> sig end = functor () -> struct end

module M = (functor (S : S) -> struct end) (S)

module M = (functor (S : S) (T : T) -> struct end) (S) (T)

module M = (functor (S : S) (T : T) -> struct end : U) (S) (T)

module M = (functor (S : S) () -> struct end : U) (S) (T)

module M = (functor (S : S) (T : T) -> (struct end : U)) (S) (T)

module M =
  (functor
  (SSSSS : sssssSSSSSSSSSSSSSS)
  (TTTTT : TTTTTTTTTTTTTTTTTTTTT) ->
  struct
  end)
    (S)
    (T)

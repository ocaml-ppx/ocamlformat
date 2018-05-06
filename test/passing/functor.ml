module type S = functor () -> sig end

module type S = functor () -> functor () -> sig end

module M : functor () -> sig end =
functor
() ->
struct
  
end

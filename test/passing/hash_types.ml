module F (X : sig
  type t
end) =
struct
  class type ['a] c =
    object
      method m : 'a -> X.t
    end
end

class ['a] c =
  object
    constraint 'a = 'a #F(Int).c
  end

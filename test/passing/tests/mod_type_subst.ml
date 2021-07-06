(** Basic *)
module type x = sig
  type t = int
end

module type t = sig
  module type x

  module M : x
end

module type t' = t with module type x = x

module type t'' = t with module type x := x

module type t3 =
  t
    with
      module type x = sig
        type t
      end

module type t4 =
  t
    with
      module type x := sig
        type t
      end

(** nested *)

module type ENDO = sig
  module Inner : sig
    module type T

    module F (_ : T) : T
  end
end

module type ENDO_2 = ENDO with module type Inner.T = ENDO

module type ENDO_2' = ENDO with module type Inner.T := ENDO

module type S = sig
  module M : sig
    module type T
  end

  module N : M.T
end

module type R = S with module type M.T := sig end

(** Adding equalities *)

module type base = sig
  type t = X of int | Y of float
end

module type u = sig
  module type t = sig
    type t = X of int | Y of float
  end

  module M : t
end

module type s = u with module type t := base

module type base = sig
  type t = X of int | Y of float
end

module type u = sig
  type x

  type y

  module type t = sig
    type t = X of x | Y of y
  end

  module M : t
end

module type r =
  u with type x = int and type y = float and module type t = base

module type r =
  u with type x = int and type y = float and module type t := base

(** First class module types require an identity *)

module type fst = sig
  module type t

  val x : (module t)
end

module type ext

module type fst_ext = fst with module type t = ext

module type fst_ext = fst with module type t := ext

module type fst_erased = fst with module type t := sig end

module type fst_ok = fst with module type t = sig end

module type S = sig
  module M : sig
    module type T
  end

  val x : (module M.T)
end

module type R = S with module type M.T := sig end

module type S = sig
  module M : sig
    module type T

    val x : (module T)
  end
end

module type R = S with module type M.T := sig end

(** local module type substitutions *)

module type s = sig
  module type u := sig
    type a

    type b

    type c
  end

  module type r = sig
    type r

    include u
  end

  module type s = sig
    include u

    type a = A
  end
end

module type s = sig
  module type u := sig
    type a

    type b

    type c
  end

  module type wrong = sig
    type a

    include u
  end
end

module type fst = sig
  module type t := sig end

  val x : (module t)
end

module type hidden = sig
  module type t := sig
    type u
  end

  include t

  val x : (module t)

  val x : int
end

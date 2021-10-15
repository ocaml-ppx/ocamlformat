exception EvalError of Error.t [@@deriving sexp]

exception Duplicate_found of (unit -> Base.Sexp.t) * string

exception Duplicate_found of ((unit -> Base.Sexp.t) -> string)

type t = Duplicate_found of (unit -> Base.Sexp.t) * string

type t = Duplicate_found : (unit -> Base.Sexp.t) * string -> t

type t = Duplicate_found : ((unit -> Base.Sexp.t) -> string) -> t

module type S = sig
  exception EvalError of Error.t [@@deriving sexp]

  exception Duplicate_found of (unit -> Base.Sexp.t) * string

  exception Duplicate_found of ((unit -> Base.Sexp.t) -> string)

  type t = Duplicate_found of (unit -> Base.Sexp.t) * string

  type t = Duplicate_found : (unit -> Base.Sexp.t) * string -> t

  type t = Duplicate_found : ((unit -> Base.Sexp.t) -> string) -> t
end

exception Recursion_error of (Lv6Id.long as 'id) * (string list as 'stack)

exception
  Internal_error of
    [ `Doc_comment of
      [ `Moved of Location.t * Location.t * string
      | `Unstable of Location.t * string ] ]

exception E : _

exception E : t

exception E : [%ext t]

exception E : (t as 'a)

exception E : (t * t)

exception E : (t -> t)

exception E : (module M)

exception E : [`X | `Y]

exception E : 'x

exception E : < x ; y ; .. >

exception E : #c

exception E : t #c

exception E : (t -> t) #c

exception E : a b #c

exception E : (a * b) #c

exception E : (a, b) #c

exception E : (t -> t) #c

exception E : (t as 'a) #c

exception E of _

exception E of t

exception E of [%ext t]

exception E of (t as 'a)

exception E of (t * t)

exception E of (t -> t)

exception E of (module M)

exception E of [`X | `Y]

exception E of 'x

exception E of < x ; y ; .. >

exception E of #c

exception E of t #c

exception E of (t -> t) #c

exception E of a b #c

exception E of (a * b) #c

exception E of (a, b) #c

exception E of (t -> t) #c

exception E of (t as 'a) #c

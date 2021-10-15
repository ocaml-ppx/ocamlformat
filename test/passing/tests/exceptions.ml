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

let _ =
  let exception Duplicate_found of (unit -> Base.Sexp.t) * string in
  let exception Duplicate_found of ((unit -> Base.Sexp.t) -> string) in
  ()

exception Recursion_error of (Lv6Id.long as 'id) * (string list as 'stack)

exception
  Internal_error of
    [ `Doc_comment of
      [ `Moved of Location.t * Location.t * string
      | `Unstable of Location.t * string ] ]

exception E: _

exception E: t

exception E: [%ext t]

exception E: (t as 'a)

exception E: (t * t)

exception E: (t -> t)

exception E: (module M)

exception E: [`X | `Y]

exception E: 'x

exception E: < x ; y ; .. >

exception E: #c

exception E: t #c

exception E: (t -> t) #c

exception E: a b #c

exception E: (a * b) #c

exception E: (a, b) #c

exception E: (t -> t) #c

exception E: (t as 'a) #c

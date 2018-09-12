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

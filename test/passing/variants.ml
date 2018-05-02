type t = A of (int * int) * int

type t = A of int * int

type t = A of (int * int)

let _ = match x with Some (Some None) -> t

type t = ..

type t = private ..

type t = u = private ..

type t += A

type t += B = A

type t =
  < hello: string  (** some doc *)
  ; world: int
  ; more: int * float
  ; make: int
  ; it: string
  ; long: float [@default 42.] >
[@@deriving make]

type 'a u = <hello: string  (** more doc *) ; world: int ; .. > as 'a

type 'a v = < .. > as 'a

type 'a w = (< .. > as 'a) -> 'a

type z = < > t

let x : unit -> <bouh: string ; .. > = fun () -> assert false

let lookup_obj : < .. > -> (< .. > as 'a) list -> 'a = fun _ -> assert false

let _ = [%ext : <a ; b> ]

let _ = (x [@att : <a ; b> ])

type t = [`A of <a ; b> ]

type t = {a: < >; b: int}

type t = {b: int; a: < > }

type 'a u = [< `A | `B of < > > `B] as 'a

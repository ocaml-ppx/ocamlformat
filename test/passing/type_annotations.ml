let f = match None with (_: int option) -> true

let f (x: int) : int = e

let f (x as y: int) : int = e

let f ((x: int) as y) : int = e

let f ((x: int): int) = e

let _ = match x with exception (e: exn) -> true | _ -> false

let x = (0 : int :> int)

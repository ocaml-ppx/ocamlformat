let _ = match x with Atom x -> x | List [ Atom x; Atom y ] -> x ^ y

let _ =
  match x with Atom x -> x | List (Atom x :: Atom y :: rest) -> x ^ y

let _ = match x with (x :: y) :: z -> true

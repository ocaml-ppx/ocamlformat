let _ = match x with Atom x -> x | List [Atom x; Atom y] -> x ^ y

let _ =
  match x with Atom x -> x | List (Atom x :: Atom y :: rest) -> x ^ y

let _ = match x with (x :: y) :: z -> true

let x = function
  | [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit"
    ; [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit"
      ; (* ", sed do eiusmod tempor incididunt ut labore et dolore"; *)
        "sed do eiusmod tempor incididunt ut labore et dolore"
        (* " magna aliqua. Ut enim ad minim veniam, quis nostrud "; *)
        (* "exercitation ullamco laboris nisi ut aliquip ex ea commodo " *)
       ]
    ; (* ", sed do eiusmod tempor incididunt ut labore et dolore"; *)
      "sed do eiusmod tempor incididunt ut labore et dolore"
      (* " magna aliqua. Ut enim ad minim veniam, quis nostrud "; *)
      (* "exercitation ullamco laboris nisi ut aliquip ex ea commodo " *)
     ] ->
      ()

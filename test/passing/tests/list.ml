let f x = match x with P ({xxxxxx} :: {yyyyyyyy} :: zzzzzzz) -> true

let f x =
  match x with
  | P
      ({xxxxxxxxxxxxxxxxxxxxxx}
      :: {yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy}
         :: zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz ) ->
      true

let f x = match x with P [{xxxxxx}; {yyyyyyyy}] -> true

let x = (x :: y) :: z

let x = match x with (x :: y) :: z -> ()

let _ = [a; b; c]

let _ = match x with Atom x -> x | List [Atom x; Atom y] -> x ^ y

let _ = match x with Atom x -> x | List (Atom x :: Atom y :: rest) -> x ^ y

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
      (* "exercitation ullamco laboris nisi ut aliquip ex ea commodo " *) ]
    ->
      ()

[@@@ocamlformat "space-around-lists=true"]

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
  | [ [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit" ]
    ; (* ", sed do eiusmod tempor incididunt ut labore et dolore"; *)
      "sed do eiusmod tempor incididunt ut labore et dolore"
      (* " magna aliqua. Ut enim ad minim veniam, quis nostrud "; *)
      (* "exercitation ullamco laboris nisi ut aliquip ex ea commodo " *)
    ] ->
      ()

let _ = f (* A *) ~x:(a :: b) (* B *) ~y

let _ = f (* A *) ~x:((* B *) a :: b (* C *)) (* D *) ~y

let _ = f ~x:((* A *) a (* B *) :: (* C *) b (* D *) :: (* E *) c (* F *)) ~y

let _ = f ((* A *) x (* B *) :: (* C *) y (* D *) :: (* E *) z (* F *))

let _ = abc :: (* def :: *) ghi :: jkl

let _ = abc :: def (* :: ghi *) :: jkl

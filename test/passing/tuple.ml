let _ =
  match w with
  | A -> ([], A.(B (C (f x))), None, f x y, g y x)
  | B -> (a, b, c, d, e, f)
  | C ->
      ( []
      , A.(B (C (this is very looooooooooooooooooooooooooooooooooooong x)))
      , None
      , f x y
      , g y x )

let _ = [%ext 1, 2, 3]

let _ =
  [%ext
    loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong
    , 2
    , 3]

type t = int [@@deriving 1, 2, 3]

type t = int
[@@deriving
  sexp
  , compare
  , loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong]

let _ =
  ( 1
  , 2
  , 3
  , looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong
  )

let _ = (1, 2, 3, short)

;; 1
   , 2
   , 3
   , looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong

;; 1, 2, 3, short

let (a, b): int * int =
  let (a, b) : int * int = (1, 2) in
  (a, b)

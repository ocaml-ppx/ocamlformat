let s =
  List.fold x ~f:(fun y -> function
    | Aconstructor avalue -> afunction avalue
    | Bconstructor bvalue -> bfunction bvalue )

let f _ = (function x -> x + 1)
let f _ = function x -> x + 1
let f _ = fun _ -> (function x -> x + 1)
let f _ = fun _ -> function x -> x + 1
let f _ = (fun _ -> (function x -> x + 1))
let f _ = (fun _ -> function x -> x + 1)
let f _ = (fun _ -> fun x -> x + 1)
let f _ = fun _ -> fun x -> x + 1
let f _ = (fun _ -> (fun x -> x + 1))
let f _ = fun _ -> (fun x -> x + 1)

let _ = let f _ = (function x -> x + 1) in ()
let _ = let f _ = function x -> x + 1 in ()
let _ = let f _ = fun _ -> (function x -> x + 1) in ()
let _ = let f _ = fun _ -> function x -> x + 1 in ()
let _ = let f _ = (fun _ -> (function x -> x + 1)) in ()
let _ = let f _ = (fun _ -> function x -> x + 1) in ()
let _ = let f _ = (fun _ -> fun x -> x + 1) in ()
let _ = let f _ = fun _ -> fun x -> x + 1 in ()
let _ = let f _ = (fun _ -> (fun x -> x + 1)) in ()
let _ = let f _ = fun _ -> (fun x -> x + 1) in ()

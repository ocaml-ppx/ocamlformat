let (lazy a) = lazy 1

let (lazy (a, b)) = lazy (1, 2)

let () =
  let (lazy a) = lazy 1 in
  let (lazy (a, b)) = lazy (1, 2) in
  ()

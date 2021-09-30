let ( let* ) x f = f x

let ( and* ) a b = (a, b)

let x = 1

and y = 2

and z = 3

let p =
  let* x and* y and* z in
  (x, y, z)

let q =
  let%foo x = x and y = y and z = z in
  (x, y, z)

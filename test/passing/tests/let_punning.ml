let ( let* ) x f = f x

let ( and* ) a b = (a, b)

let x = 1

and y = 2

and z = 3

let p =
  let* x and* y and* z in
  (x, y, z)

let q =
  let%foo x and y and z in
  (x, y, z)

let p =
  let* x = x and* y = y and* z = z in
  (x, y, z)

let q =
  let%foo x = x and y = y and z = z in
  (x, y, z)

let p =
  let* x = x and* y = z in
  (x, y)

let q =
  let%foo x = x and y = z in
  (x, y)

let p =
  let* x = y and* z = z in
  (x, z)

let q =
  let%foo x = y and z = z in
  (x, z)

(* Line breaks *)

let mixed =
  let%foo a = a
  and b = b
  and c = c
  and d = e + f
  in 
  ()

let long =
  let%foo a = a
  and b = b
  and c = c
  and d = d
  and e = e
  and f = f
  and g = g
  and h = h
  and i = i
  and j = j
  and k = k
  and l = l
  in
  ()

let looong =
  let%foo a = a
  and b = b
  and c = c
  and d = d
  and e = e
  and f = f
  and g = g
  and h = h
  and i = i
  and j = j
  and k = k
  and l = l
  and m = m
  and n = n
  and o = o
  and p = p
  and q = q
  and r = r
  and s = s
  and t = t
  and u = u
  and v = v
  in
  ()

(* Comments *)

let r =
  let* (* 11111 *) w = w in
  let* x (* 22222 *) = x in
  let* y = (* 33333 *) y in
  let* z = z (* 44444 *) in
  let*
      (* 55555 *)
      (* 66666 *) x
      (* 77777 *)
      (* 88888 *) =
    (* 99999 *)
    x
    (* 00000 *)
  in
  let* (* 1111111111 *) x = x
  and* (* 2222222222 *) y = y
  and* z (* 3333333333 *) = z
  and* w = (* 4444444444 *) w
  and* q = q (* 5555555555 *) in
  (* 6666666666 *)
  x + y + z + w + q

let r =
  let%foo (* 11111 *) w = w in
  let%bar x (* 22222 *) = x in
  let%baz y = (* 33333 *) y in
  let%foo z = z (* 44444 *) in
  let%bar
      (* 55555 *)
      (* 66666 *) x
      (* 77777 *)
      (* 88888 *) =
    (* 99999 *)
    x
    (* 00000 *)
  in
  let%baz (* 1111111111 *) x = x
  and (* 2222222222 *) y = y
  and z (* 3333333333 *) = z
  and w = (* 4444444444 *) w
  and q = q (* 5555555555 *) in
  (* 6666666666 *)
  x + y + z + w + q

(* Attribute on let binding *)

let w_attr x y =
  let%foo[@bar] x = x
  and[@baz] y = y in
  x + y

(* Non-standard syntax *)

let s = [%bar let x = x and y = y in ()]

(* Is it a pun??? *)

let t =
  let x = x in
  ()

let u =
  let%foo x y = x in
  ()

let v =
  let%foo x[@bar] = x and y = y[@baz] in
  ()

let w =
  let%foo local_ x = x and local_ y = y in
  ()

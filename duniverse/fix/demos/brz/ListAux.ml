let rec equal (=) xs ys =
  match xs, ys with
  | [], [] ->
      true
  | x :: xs, y :: ys ->
      x = y && equal (=) xs ys
  | _, _ ->
      false

let rec uniq1 cmp x ys =
  match ys with
  | [] ->
      []
  | y :: ys ->
      if cmp x y = 0 then
        uniq1 compare x ys
      else
        y :: uniq1 cmp y ys

let uniq cmp xs =
  match xs with
  | [] ->
      []
  | x :: xs ->
      x :: uniq1 cmp x xs

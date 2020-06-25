module L = Stdlib.List

let partition_map l ~f =
  let fst, snd =
    L.fold_left
      (fun (fst, snd) x ->
        match f x with
        | `Fst x' -> (x' :: fst, snd)
        | `Snd x' -> (fst, x' :: snd) )
      ([], []) l
  in
  (L.rev fst, L.rev snd)

include Base.List

let partition_map l ~f =
  let fst, snd =
    fold_left l
      ~f:(fun (fst, snd) x ->
        match f x with
        | Either.Left x' -> (x' :: fst, snd)
        | Either.Right x' -> (fst, x' :: snd))
      ~init:([], [])
  in
  (rev fst, rev snd)

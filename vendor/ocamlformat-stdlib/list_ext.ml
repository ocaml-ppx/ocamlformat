include Base.List

let partition_map l ~f =
  let fst, snd =
    fold_left l
      ~f:(fun (fst, snd) x ->
        match f x with
        | Base.Either.First x' -> (x' :: fst, snd)
        | Base.Either.Second x' -> (fst, x' :: snd))
      ~init:([], [])
  in
  (rev fst, rev snd)

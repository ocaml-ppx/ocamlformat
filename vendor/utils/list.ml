open Base

let partition_map l ~f =
  let fst, snd =
    List.fold_left l
      ~f:(fun (fst, snd) x ->
        match f x with
        | `Fst x' -> (x' :: fst, snd)
        | `Snd x' -> (fst, x' :: snd) )
      ~init:([], [])
  in
  (List.rev fst, List.rev snd)

let sum_of_squares num =
  num + 1 
  |> List.range 0 
  |> List.map ~f:square
  |> List.fold_left ~init:0 ~f:( + )

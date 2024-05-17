let s =
  List.fold x ~f:(fun y -> function
    | Aconstructor avalue -> afunction avalue
    | Bconstructor bvalue -> bfunction bvalue )

let f x =
  [%Trace.call fun {pf} -> pf "%i" x]
  ;
  print_int x ;
  x
  |>
  [%Trace.retn fun {pf} -> pf "%i"]

let f x =
  [%Trace.call fun {pf} : t -> pf "%i" x]
  ;
  print_int x ;
  x
  |>
  [%Trace.retn fun {pf} : t -> pf "%i"]

(* parens have the effect of erasing optional arguments *)
let _ = (f x y) z

let _ = List.map ~f:(( + ) (M.f x))

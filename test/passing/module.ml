module AAAAAAAAAAAAAAAAAAA =
  Soooooooooooooooooooooooome.Loooooooooooooooooooooooong.Mod

let _ =
  let module A = B in
  let module AAAAAAAAAAAAAAAAAAA =
    Soooooooooooooooooooooooome.Loooooooooooooooooooooooong.Mod
  in
  t

let create (type a b) t i w p =
  let module T = (val (t : (a, b) t)) in
  T.create i w p

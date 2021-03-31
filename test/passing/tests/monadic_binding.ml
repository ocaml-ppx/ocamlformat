let ( let* ) t f = fooooooo

let ( and* ) t1 t2 = foooooo

let map f t =
  let* a = t in
  pure (f a)

let ( and+ ) t1 t2 = ( and* ) t1 t2

let ( and+ ) t1 t2 = ( and* ) t1 t2 x

let ( and+ ) t1 t2 =
  ( and* ) t1 t2 x foooooooooooooooooo foooooooooooooooooooo
    foooooooooooooooooo foooooooooooooooooo

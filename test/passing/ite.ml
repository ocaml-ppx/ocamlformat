let _ = if b then e else ( e1 ; e2 )

let _ =
  if b then e
  else (
    something loooooooooooooooooooooooooooooooong enough to_trigger a break ;
    this is more )

let _ =
  if b then (
    something loooooooooooooooooooooooooooooooong enough to_trigger a break ;
    this is more )
  else if b1 then (
    something loooooooooooooooooooooooooooooooong enough to_trigger a break ;
    this is more )
  else e

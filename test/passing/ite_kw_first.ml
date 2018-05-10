let _ = if b then e else ( e1 ; e2 )

let _ =
  if b
  then e
  else (
    something loooooooooooooooooooooooooooooooong enough to_trigger a break ;
    this is more )

let _ =
  if b
  then (
    something loooooooooooooooooooooooooooooooong enough to_trigger a break ;
    this is more )
  else if b1
  then (
    something loooooooooooooooooooooooooooooooong enough to_trigger a break ;
    this is more )
  else e

;; f
     ( if loooooooooooooooooooooooooooooooooooooooooooooooooooooooooong
     then ()
     else () )

;; f
     ( if loooooooooooooooooooooooooooooooooooooooooooooooooooooooooonger
     then ()
     else () )

;; f
     ( if even
            loooooooooooooooooooooooooooooooooooooooooooooooooooooooooonger
     then ()
     else () )

;; f
     ( if and_ even
            loooooooooooooooooooooooooooooooooooooooooooooooooooooooooonger
     then ()
     else () )

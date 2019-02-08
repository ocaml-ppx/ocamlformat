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

;;
f
  ( if loooooooooooooooooooooooooooooooooooooooooooooooooooooooooong then ()
  else () )

;;
f
  ( if loooooooooooooooooooooooooooooooooooooooooooooooooooooooooonger then
    ()
  else () )

;;
f
  ( if even loooooooooooooooooooooooooooooooooooooooooooooooooooooooooonger
  then ()
  else () )

;;
f
  ( if
    and_ even
      loooooooooooooooooooooooooooooooooooooooooooooooooooooooooonger
  then ()
  else () )

let () = if [@test] true then () else if [@other] true then ()

[@@@ocamlformat "if-then-else=sparse"]

let foo =
  if cond1 then
    arm1
  else if cond2 then
    arm2
  else
    arm3

let foo =
  if cond1 then (
    arm1 ;
    foooooooooooooo ;
    fooooooooooooooooooo fooooooooooooooo foooooooooooo ;
    List.foo ~fooooooo:foooooooooooooooo ~foo:(fun fooooooooo ->
        fooooooooooooo ) )
  else if cond2 then (
    arm2 ;
    foooooooooooooo ;
    fooooooooooooooooooo fooooooooooooooo foooooooooooo ;
    List.foo ~fooooooo:foooooooooooooooo ~foo:(fun fooooooooo ->
        fooooooooooooo ) )
  else (
    arm3 ;
    foooooooooooooo ;
    fooooooooooooooooooo fooooooooooooooo foooooooooooo ;
    List.foo ~fooooooo:foooooooooooooooo ~foo:(fun fooooooooo ->
        fooooooooooooo ) )

let _ =
  if condition then
    let a = 1 in
    let b = 2 in
    a + b
  else if other_condition then
    12
  else
    0

let foo =
  if is_sugared_list e2 then
    Some (Semi, Non)
  else
    Some
      ( ColonColon
      , 
        if exp == e2 then
          Right
        else
          Left )

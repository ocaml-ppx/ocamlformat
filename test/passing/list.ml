let f x = match x with P ({ xxxxxx } :: { yyyyyyyy } :: zzzzzzz) -> true

let f x =
  match x
  with
  | P
      ({ xxxxxxxxxxxxxxxxxxxxxx }
      :: { yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy }
         :: zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz)
  -> true

let f x = match x with P [ { xxxxxx }; { yyyyyyyy } ] -> true

let x = (x :: y) :: z

let x = match x with (x :: y) :: z -> ()

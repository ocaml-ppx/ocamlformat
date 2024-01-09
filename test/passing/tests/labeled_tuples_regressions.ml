let foo a =
  match a with
  | ( ~a:None
    , ~b:(Some _)
    , ~c:[ 1; 2 ]
    , ~d:(3 :: [])
    , ~e:{ x : _; y : _ }
    , ~f:42
    , ~g:_
    , ~h:`Baz
    , ~i:(`Bar _)
    , ~j:(1 | 2)
    , ~k:[| 1; 2 |]
    , ~l:(3 : int)
    , ~m:(lazy _)
    , ~n:(module M)
    , ~o:(exception _)
    , ~p:[%bar baz]
    , ~q:M.(A)
    , ~r:M.(A 42) ) -> false
;;

let _ = {!e with a; b= c}

let _ = {!(f e) with a; b= c}

let _ =
  { !looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
    with a; b= c }

let _ =
  { !looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
    with
    aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  ; b= c }

let _ = {(a : t) with a; b; c}

let _ = {(f a) with a; b; c}

let _ = {(a ; a) with a; b; c}

let _ = {(if x then e else e) with e1; e2}

let _ = {(match x with x -> e) with e1; e2}

let _ = {(x : x) with e1; e2}

let _ = {(x :> x) with e1; e2}

let _ = {(x#x) with e1; e2}

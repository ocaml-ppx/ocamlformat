let f (module X) = X.x

let f = function `A {x : int = _} -> ()

let f (`A | `B) = ()

let f (module X) = X.x

let f = function `A { x= (_ : int) } -> ()

let f (`A | `B) = ()

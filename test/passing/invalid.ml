let f = function "as" .. 3 | 3. .. 'q' | 3 .. -3. | -3. .. 3 -> ()

let f = function (lazy (exception A)) -> () | exception (lazy A) -> ()

let f = (A) a b

let f = (A x) a b

let f = (`A) a b

let f = (`A x) a b

let f = ((::)) a b c

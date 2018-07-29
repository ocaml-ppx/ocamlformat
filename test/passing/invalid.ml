let f = function "as" .. 3 | 3. .. 'q' | 3 .. -3. | -3. .. 3 -> ()

let f = function (lazy (exception A)) -> () | exception (lazy A) -> ()

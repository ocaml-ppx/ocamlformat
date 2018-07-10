let _ = f (*f*)a(*a*) ~b(*comment*) ~c:(*comment*)c' ?d ?e ()

let _ =
  let _ =
    f
      (*comment*)(let open M in
                 let x = x in
                 e)
  in
  ()

let _ = ((*comment*)a(*comment*), b)

let foo = function Blah ( (* old *)x, y) -> ()

let foo = function Blah ( x(* old *), y) -> ()

let foo = function Blah, (* old *)(x, y) -> ()

let foo = function Blah (x, y)(* old *) -> ()

let foo = function Blah, (x, y(* old *)) -> ()

let foo = function Blah, (x, (* old *)y) -> ()

let foo = function ((x, y)(* old *), z) -> ()

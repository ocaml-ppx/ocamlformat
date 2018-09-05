;;
[%sexp { x : int; y : string }]

let _ = fun { x : int; y : string } -> ()

let _ = { A.b= A.b }

let { A.b } = x

let _ =
  object
    method x = {<x = A.x>}
  end

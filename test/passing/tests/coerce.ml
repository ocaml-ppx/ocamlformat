let _ =
  let a :> x = v in
  let a : x :> y = v in
  let a = (v :> x) in
  let a = (v : x :> y) in
  let a : x :> y = (v : x :> y) in
  ()

let a :> x = v

let a : x :> y = v

let a = (v :> x)

let a = (v : x :> y)

let a : x :> y = (v : x :> y)

class c =
  let a :> x = v in
  let a : x :> y = v in
  let a = (v :> x) in
  let a = (v : x :> y) in
  let a : x :> y = (v : x :> y) in
  object end

let f (type a) :> a M.u = function z -> z

let f x (type a) :> a M.u = function z -> z

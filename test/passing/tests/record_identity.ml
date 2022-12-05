let a = { x = x }
let b = { x : a = x }
let c = { x : a :> b = x }
let d = { x = x[@foo] }
let e = { x = y }

let { x = x } = f

let deep = { x = x ; y = { x = x }}

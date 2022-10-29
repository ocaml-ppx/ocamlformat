let _ = List.filter (( != ) e) l

let _ = List.map (( != ) x) l

let _ = x != y

let _ = - !e

let _ = - !e.f

let z = (( ! ) ~x:4) 1 2 ~c:3

let z = (( ! ) ~x:4 y z) 1 2 ~c:3

let z = (( ! ) ~x:4 [@attr]) 1 2 ~c:3

let z = (( ! ) [@attr]) 1 2 ~c:3

let z = (( ! ) [@attr])

let i x = (!r [@attr]) x

let _ = ( * ) [@attr]

let _ = f (( * ) [@attr]) ;;

( * ) [@attr]

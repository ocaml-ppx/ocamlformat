let op = if b then ( * ) else ( + ) in
()
;;

assert ( * ) ;;

( * ) [@a] ;;

assert (( * ) [@a])

module Array = struct
  let ( .!() ) = Array.unsafe_get

  let ( .!()<- ) = Array.unsafe_set
end

let ( .!() ), ( .!()<- ) = Array.((( .!() ) [@attr]), ( .!()<- ))

let _ = ( let++ ) [@attr] ;;

( let++ ) [@attr]

let ( let++ ), (( and++ ) [@attr]) = X.((( let++ ) [@attr]), ( and++ ))

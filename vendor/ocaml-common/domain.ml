(* Unsafe replacement for some [Domain] functions needed to implement
   [Format_]. Remove this module if OCamlformat ever uses parallelism. *)

module DLS = struct
  type 'a key = 'a ref
  let new_key f = ref (f ())
  let get k = !k
  let set k v = k := v
end

let at_exit = at_exit
let before_first_spawn f = f ()

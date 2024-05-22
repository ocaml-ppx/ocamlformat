let _ =
 fun (module Store : Irmin.Generic_key.S with type repo = repo)
     (module Store : Irmin.Generic_key.S with type repo = repo)
 -> body

let _ =
  f
    (fun (module Store : Irmin.Generic_key.S with type repo = repo)
         (module Store : Irmin.Generic_key.S with type repo = repo)
    -> body )

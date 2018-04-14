let () =
  [%ext expr] ;
  ()


let _ = (match%ext x with () -> ()) [@attr y]

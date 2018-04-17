let () =
  [%ext expr] ;
  ()

let _ = (match%ext x with () -> ()) [@attr y]

val f : compare:[%compare : 'a] -> sexp_of:[%sexp_of : 'a] -> t

let () = [%ext expr] ; ()

let _ = (match%ext x with () -> ()) [@attr y]

let _ =
  match%ext x with () ->
    let y = [%test let x = y] in
    let%test x = d in
    d

val f : compare:[%compare : 'a] -> sexp_of:[%sexp_of : 'a] -> t

let invariant t =
  Invariant.invariant [%here] t [%sexp_of : t] (fun () ->
      assert (check_t_invariant t) )

;; [%e
     ? ( xxxxxxxxx
       , xxxxxxxxxxxxx
       , xxxxxxxxxxxxxxxx
       , xxxxxxxxxxxxxx
       , xxxxxxxxxxx
       , xxxxxxxxxxxxxxxxxxxx )]

;; [%e
     ? ( xxxxxxxxx
       , xxxxxxxxxxxxx
       , xxxxxxxxxxxxxxxx
       , xxxxxxxxxxxxxx
       , xxxxxxxxxxx
       , xxxxxxxxxxxxxxxxxxxx ) when a < b]

[%%ext
;; 11111111111111111111

;; 22222222222222222222

;; 33333333333333333333]

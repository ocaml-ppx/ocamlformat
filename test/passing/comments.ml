let _ = f (*f*)a(*a*) ~b(*comment*) ~c:(*comment*)c' ?d ?e ()

let _ =
  let _ =
    f
      (*comment*)(let open M in
                 let x = x in
                 e)
  in
  ()

let _ = ((*comment*)a(*comment*), b)

let foo = function Blah ( (* old *)x, y) -> ()

let foo = function Blah ( x(* old *), y) -> ()

let foo = function Blah, (* old *)(x, y) -> ()

let foo = function Blah (x, y)(* old *) -> ()

let foo = function Blah, (x, y(* old *)) -> ()

let foo = function Blah, (x, (* old *)y) -> ()

let foo = function ((x, y)(* old *), z) -> ()

let _ = if ((* a0 *) b (* c0 *))
        then ((* d0 *) e (* f0 *))
        else ((* g0 *) h (* i0 *))

let _ = if (* a1 *) b (* c1 *)
        then (* d1 *) e (* f1 *)
        else (* g1 *) h (* i1 *)

let _ = if (* a2 *) B (* c2 *)
        then (* d2 *) E (* f2 *)
        else (* g2 *) H (* i2 *)

let _ = if ((* a3 *) B (* c3 *))
        then ((* d3 *) E (* f3 *))
        else ((* g3 *) H (* i3 *))


;; match x with
   | true -> "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
   | false -> "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
   (* this comment should not change the formatting of the following case *)
   | false -> "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

;; try f x
   with
   (* this comment is intended to refer to the entire case below *)
   | Caml.Not_found
   -> ()

;; match x
   with
   (* this comment is intended to refer to the entire case below *)
   | false
   -> ()

;; match x with
   | Aaaaaaaaaaaaaaaaaaaa
   (* this comment is intended to refer to the case below *)
    |Bbbbbbbbbbbbbbbbbbbb ->
       ()

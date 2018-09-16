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

let _ =
  (* this comment is intended to refer to the entire match below *)
  match x with
  | "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" -> ()
  | "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" -> ()



module type M = sig

  val f
    (* A list of [name], [count] pairs. *)
    :  (string * int) list
    -> int

end

let _ = f ~f:(fun a b -> c) (* comment *) ~f:(fun a b -> c)
let _ = f (fun x -> g h) (* comment *) ~f:(fun a b -> c)
let _ = f (g h) (* comment *) ~f:(fun a b -> c)



let _ = f ((0 + 0) (* test *) + (1 * 1) (* test *))

let _ = f ((1 * 1) (* test *) + (0 + 0) (* test *))

let _ = match e with
  | 3 (* test *) -> e
  | 3 (* test *) :: tail -> e

module rec A = struct end
(*test*)
and B = struct end

module type T = sig
  module rec A : sig end
  (*test*)
  and B : sig end
end

let f = (* comment *) function x -> x

let foo x = (* comment *) (y : z)


let _ = (*a*)s(*b*).(*c*)((*d*)i(*e*))
let _ = (*a*)s(*b*).(*c*)((*d*)i(*e*))(*f*)<-(*g*)x 

let _ = (*a*)s(*b*).(*c*)[(*d*)i(*e*)]
let _ = (*a*)s(*b*).(*c*)[(*d*)i(*e*)](*f*)<-(*g*)x 

let _ = (*a*)s(*b*).(*c*){(*d*)i(*e*)}
let _ = (*a*)s(*b*).(*c*){(*d*)i(*e*)}(*f*)<-(*g*)x 

let _ = (*a*)s(*b*).%{(*c*)i(*d*)}
let _ = (*a*)s(*b*).%{(*c*)i(*d*)}(*e*)<-(*f*)x 


type t = {
a : int [@default a]
(* comment *)
;  b : flag }

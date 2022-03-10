(*
*)

(**)

(* *)

(*$*)
(*$ *)
(*$  *)

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

let _ = if a then b :: c (* d *) else e

let (b :: c (* d *)) = x

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

let () =
  (* *)
  
  (* *)
  ()

(* break when unicode sequence length measured in bytes but ¬ in code points *)

type t =
  | Aaaaaaaaaa (* Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. *)
  | Bbbbbbbbbb (* foo *)
  | Bbbbbbbbbb (* foo *)

let () =
  xxxxxxxxxx
  || (* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx *)
     xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

let () =
  xxxxxxxxxx
  land (* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx *)
    xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

let rec fooooooooooo = function
  | ((*XX*) (x :: t) (*YY*)) -> k
  | (* AA*) ((*BB*) [(* CC *)x(* DD *) ; (* EE *)y(* FF *)] (* GG *)) (* HH *) -> k
  | (* AA*) ((*BB*) ((* CC *)x(* DD *) :: (* EE *)t(* FF *)) (* GG *)) (* HH *) -> k
  | (* AA*) ((*BB*) (((* CC *)x(* DD *)) (* XX *) :: (* YY *) ((* EE *)t(* FF *))) (* GG *)) (* HH *) -> k
  | (* AA *) ( (* BB *) ( (* CC *) module (* DD *) F (* EE *) : (* FF *) M (* GG *) ) (* HH *) :: (* II *) t (* JJ *) ) (* KK *) -> foo

let%map (* __________________________________________________________________________________________ *) _ = ()

type t = < (* a *)
  a : int[@atr]; (* b *)
  b : int; (* c *)
>

type t = <
  a : int; (* a *)
  (* b *) .. (* c *)
>

type t = < (* a *) .. (* b *) >

class type i = object (* test *)
inherit oo end

class i = object (* test *)
inherit oo end

let _ =
  try_with (fun () -> (* comment before *)
    match get () with
    | None -> do_something ()
    | Some _ -> () (* do nothing *))
;;

let _ =
  try_with (fun () -> (* comment before *)
    a;
    b (* after b *))
;;

let _ =
  match x with
  | Some y ->
    (match y with
     | None -> ()
     | Some z -> incr z (* double some *))
  | None -> ()
;;

type prefix = {
  sib_extend : int; (* extended sib index bit *)
  (** add more as needed *)
}

type t =
  | A (* A *)
  (* | B *)
  | C

type t =
  (* | B *)
  | A (* A *)
  | C

type t =
  | A (* A *) (* | B *)
  | C

type foo = Alpha | Beta [@@ocaml.warning "-37" (* Explanation of warning *)]

type foo =
  | Alpha______________________________
  | Beta_______________________________ 
  [@@ocaml.warning "-37" (* Explanation of warning *)]

let y = f (* a *) (* b *) x

module A (* A *) () (* B *) = (* C *) B

let kk = (* foo *) (module A : T)

let kk = ((* foo *) (module A : T))

let kk = ((module A : T) (* foo *))

let kk = ((* foo *) (module A : T) (* foo *))

let _ = assert (foo (bar + baz <= quux))
(* this comment should stay attached to the preceding item *)

let _ = foo

let a = [b;
(*  *)
(* c *)
]

let _ =
  1
  + (* foooooooooooooooooooooooo fooooooooooooooo fooooooooooooooooo *)
  fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
  - (* fooooooooooooo foooooooooooooooooooooo foooooooooooooooooooo *)
  foooooooooooooo foooooooooooooo foooooooooooooooooo fooooooooo
  % (* foooooooooooooooo foooooooooooo foooooooooooooooooo *)
  fooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
  / (* foooooooooooooooooooooooo fooooooooooooooo fooooooooooooooooo *)
  barrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
  * (* convert from foos to bars blah blah blah blah blah blah blah blah *)
  foooooooooooooooooooooooo foooooooooooooooo fooooooooooooooo
  $ (* convert from foos to bars blah blah blah blah blah blah blah blah *)
  foooooooooooooooooooooooo foooooooooooooooo fooooooooooooooo
  & (* convert from foos to bars blah blah blah blah blah blah blah blah *)
  foooooooooooooooooooooooo foooooooooooooooo fooooooooooooooo
  = (* convert from foos to bars blah blah blah blah blah blah blah blah *)
  foooooooooooooooooooooooo foooooooooooooooo fooooooooooooooo
  > (* convert from foos to bars blah blah blah blah blah blah blah blah *)
  foooooooooooooooooooooooo foooooooooooooooo fooooooooooooooo
  < (* convert from foos to bars blah blah blah blah blah blah blah blah *)
  foooooooooooooooooooooooo foooooooooooooooo fooooooooooooooo
  @ (* convert from foos to bars blah blah blah blah blah blah blah blah *)
  foooooooooooooooooooooooo foooooooooooooooo fooooooooooooooo
  ^ (* convert from foos to bars blah blah blah blah blah blah blah blah *)
  foooooooooooooooooooooooo foooooooooooooooo fooooooooooooooo
  || (* convert from foos to bars blah blah blah blah blah blah blah blah *)
  foooooooooooooooooooooooo foooooooooooooooo fooooooooooooooo
  #= (* convert from foos to bars blah blah blah blah blah blah blah blah *)
  foooooooooooooooooooooooo foooooooooooooooo fooooooooooooooo

let _ =
  assert (
    (* before let module *)
    let module (* before A *) A (* after A *) = struct
      let v = true
    end in
    A.v || b)

let _ =
  assert (
    (* before let exception *)
    let exception Foo in
    raise Foo)

let _ =
  assert (
    (* before fun *)
    fun x -> x)

let _ =
  assert (
    (* before if then else *)
    if (* before x *) x (* after x *) then (* before y *) y (* after y *) else (* before z *) z (* after z *) )

let _ =
  assert (
    (* before nested assert *)
    assert x )

let _ = assert ( (* before list *) [] (* after list *) )

let _ = assert ( (* before list *) [x; y] (* after list *) )

let _ = assert ( (* before array *) [||] (* after array *) )

let _ = assert ( (* before array *) [|x; y|] (* after array *) )

let _ = assert ( (* before function *) function _ -> true )

let _ = assert ( (* before let *) let x = y in z )

let _ = assert ( (* before letop *) let>> x = y in z )

let _ = assert ( (* before module *) module X )

let _ = assert ( (* before record *) {x; y} (* after record *) )

let _ = assert ( (* before record *) {r with x; y} (* after record *) )

let _ = assert ( (* before record *) {x= x'; y= y'} (* after record *) )

let _ = assert ( (* before record *) {r with x= x'; y= y'} (* after record *) )

let _ = assert ( (* before lazy *) lazy z )

let _ = assert ( (* before for *) for x = 0 to 1 do () done (* after done *))

let _ = assert ( (* before while *) while x do () done (* after done *) )

let _ = assert ( (* before object *) object end (* after end *) )

let _ = assert ( (* before object *) object (self) end (* after end *) )

let _ = assert ( (* before object *) object method x = x end (* after end *) )

let _ = assert ( (* before object *) object (self) method x = x end (* after end *) )

let _ = assert ( (* before override *) {<>} (* after override *) )

let _ = assert ( (* before override *) {< x; y>} (* after override *) )

let _ = assert ( (* before override *) {< x= x'; y= y'>} (* after override *) )

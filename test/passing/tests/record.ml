type t = {x: int; y: int}

let _ = {x= 1; y= 2}

let _ = {!e with a; b= c}

let _ = {!(f e) with a; b= c}

let _ =
  { !looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
    with
    a
  ; b= c }

let _ =
  { !looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
    with
    aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  ; b= c }

let _ = {(a : t) with a; b; c}

let _ = {(f a) with a; b; c}

let _ = {(a ; a) with a; b; c}

let _ = {(if x then e else e) with e1; e2}

let _ = {(match x with x -> e) with e1; e2}

let _ = {(x : x) with e1; e2}

let _ = {(x :> x) with e1; e2}

let _ = {(x#x) with e1; e2}

let f ~l:{f; g} = e

let f ?l:({f; g}) = e

let _ = {a; b = ((match b with `A -> A | `B -> B | `C -> C) : c); c}

let a () = A {A.a = (a : t)}

let x =
  { aaaaaaaaaa
  (* b *); b}

let x =
  { aaaaaaaaaa
  (* b *)
  ; b}

type t = { a : (module S); b : (module S) }

let _ = { a = (module M : S); b = (module M : S) }

let to_string {x; _ (* we should print y *)} = string_of_int x

let { x = (x : t) } = x

type t =
  { xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx : YYYYYYYYYYYYYYYYYYYYY.t
    (* ____________________________________ *)
  }

let _ =
  let _ = function
    | {
      foooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo;
      foooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo;
    } ->
        ()
  in
  ()

let foo
    ({
       foooooooooooooooooooooo;
       invalidation_trace;
       access_trace;
       must_be_valid_reason;
     } [@warning "+missing-record-field-pattern"]) =
  ()

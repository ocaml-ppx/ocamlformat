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

let Mmmmmm.
      { xxxx
      ; xxxxxxxxx
      ; xxxxxxxxxxxxxxxxxx
      ; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx } =
  ()

let Mmmmmm.
      { xxxx
      ; xxxxxxxxx
      ; xxxxxxxxxxxxxxxxxx
      ; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx (* foooooooooooo *) }
    (* fooooooooo *) =
  ()

let _ = {a; b: c = (match b with `A -> A | `B -> B | `C -> C); c}

let a () = A {A.a: t}

let x = {(*test*) aaa: aa; bbb: bb}

let x = {aaa: aa (* A *); bbb: bb}

let x = {aaa: aa; (* A *) bbb: bb}

let x = {(*test*) aaa: aa = aa; bbb: bb}

let x = {aaa: aa (* A *) = aa; bbb: bb}

let x = {aaa: aa = (* A *) aa; bbb: bb}

let x = {aaa: aa; (* A *) bbb: bb}

let {(*a*) a: a} = e

let {a (*a*): a} = e

let {a: (*a*) a} = e

let {a: a (*a*)} = e

let _ =
  (* comment here *)
  { (* comment here *)
    aaaaaaaaaaaaaaaaaaaaaaaaaaaaa= aaaaaaaaaaaaaaaaaaaaaaaa
  ; bbbbbbbbbbbb: bbbbbbbbbbb = bbbbbbbbbbbbbbbbb }

let { (* comment here *)
        aaaaaaaaaaaaaaaaaaaaaaaaaaaaa= aaaaaaaaaaaaaaaaaaaaaaaa
    ; bbbbbbbbbbbb: bbbbbbbbbbb = bbbbbbbbbbbbbbbbb } =
  e

type t =
  { (* comment here *)
    aaaaaaaaaaaaaaaaaaaaaaaaaaaaa: aaaaaaaaaaaaaaaaaaaaaaaa
  ; bbbbbbbbbbbb: bbbbbbbbbbb }

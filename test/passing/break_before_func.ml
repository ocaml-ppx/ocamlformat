[@@@ocamlformat "break-before-func=false"]

;;
List.fooo (fun x ->
    xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx )

;;
List.fooo (fun x ->
    xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx )

(* comment *)

;;
(List.fooo (fun x ->
     xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
       xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx ) [@foo])

;;
(List.fooo (fun x ->
     xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
       xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx ) [@foo])

(* comment *)

(* foooooooooooo *)

;;
List.fooo (function
  | A ->
      xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
        xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx )

;;
List.fooo (function
  | A ->
      xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
        xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx )

(* comment *)

;;
(List.fooo (function
   | A ->
       xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
         xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx ) [@foo])

;;
(List.fooo (function
   | A ->
       xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
         xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx ) [@foo])

(* comment *)

;;
List.fooo (function
  | A ->
      xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
        xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx )

(* foooooooooo *)

;;
List.map (fun x ->
    xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx
  ) list

;;
List.map (* toto *)
  (fun x ->
    xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx
  ) list

;;
List.map (* toto *)
  (fun x ->
    xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx
  ) (* foooooooooo *) list

;;
List.map
  (* this is a loooooooooooooooooooooooooooooong commmmmmmmmmment
     blablablabla *)
  (fun xxxxxxxxxxxxxxxxxxxxx yyyyyyyyyyyyyyyy zzzzzzzzzzzzzzzzzz
         aaaaaaaaaaaaaaaa ->
    xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx
  ) list

;;
List.map
  (* this is a loooooooooooooooooooooooooooooong commmmmmmmmmment
     blablablabla *)
  (fun xxxxxxxxxxxxxxxxxxxxx yyyyyyyyyyyyyyyy zzzzzzzzzzzzzzzzzz
         aaaaaaaaaaaaaaaa ->
    xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx )
  (fun xxxxxxxxxxxxxxxxxxxxx yyyyyyyyyyyyyyyy zzzzzzzzzzzzzzzzzz
       aaaaaaaaaaaaaaaa ->
    xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx )
  (fun xxxxxxxxxxxxxxxxxxxxx yyyyyyyyyyyyyyyy zzzzzzzzzzzzzzzzzz
       aaaaaaaaaaaaaaaa ->
    xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx )
  list

;;
List.map
  (* this is a loooooooooooooooooooooooooooooong commmmmmmmmmment
     blablablabla *)
  (function
  | aaaaaaaaaaaaaaaa ->
      xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
        xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx )
  (fun xxxxxxxxxxxxxxxxxxxxx yyyyyyyyyyyyyyyy zzzzzzzzzzzzzzzzzz
       aaaaaaaaaaaaaaaa ->
    xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx )
  (fun xxxxxxxxxxxxxxxxxxxxx yyyyyyyyyyyyyyyy zzzzzzzzzzzzzzzzzz
       aaaaaaaaaaaaaaaa ->
    xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx )
  list

;;
List.map
  (* this is a loooooooooooooooooooooooooooooong commmmmmmmmmment
     blablablabla *)
  (fun xxxxxxxxxxxxxxxxxxxxx yyyyyyyyyyyyyyyy zzzzzzzzzzzzzzzzzz
         aaaaaaaaaaaaaaaa ->
    xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx )
  (* foooooooo foooooo *)
    (fun xxxxxxxxxxxxxxxxxxxxx yyyyyyyyyyyyyyyy zzzzzzzzzzzzzzzzzz
         aaaaaaaaaaaaaaaa ->
    xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx )
  (fun xxxxxxxxxxxxxxxxxxxxx yyyyyyyyyyyyyyyy zzzzzzzzzzzzzzzzzz
       aaaaaaaaaaaaaaaa ->
    xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx )
  list

;;
List.map
  (* this is a loooooooooooooooooooooooooooooong commmmmmmmmmment
     blablablabla *)
  (function
  | aaaaaaaaaaaaaaaa ->
      xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
        xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx )
  (* foooooooo foooooo *)
    (fun xxxxxxxxxxxxxxxxxxxxx yyyyyyyyyyyyyyyy zzzzzzzzzzzzzzzzzz
         aaaaaaaaaaaaaaaa ->
    xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx )
  (fun xxxxxxxxxxxxxxxxxxxxx yyyyyyyyyyyyyyyy zzzzzzzzzzzzzzzzzz
       aaaaaaaaaaaaaaaa ->
    xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx )
  list

;;
List.map
  (* this is a loooooooooooooooooooooooooooooong commmmmmmmmmment
     blablablabla *)
  (function
  | aaaaaaaaaaaaaaaa ->
      xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
        xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx )
  (* foooooooo foooooo *)
    (function
    | aaaaaaaaaaaaaaaa ->
        xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
          xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx)
  (fun xxxxxxxxxxxxxxxxxxxxx yyyyyyyyyyyyyyyy zzzzzzzzzzzzzzzzzz
       aaaaaaaaaaaaaaaa ->
    xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx )
  list

;;
List.map (function
  | A ->
      xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
        xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx
  ) list

;;
List.map (* toto *)
  (function
  | A ->
      xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
        xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx
  ) list

;;
List.map (* toto *)
  (function
  | Aaaaaaaaa
      ( aaaaaaaaaaaaaaa
      , aaaaaaaaaaaaaaaa
      , {aaaaaaaaaaaaa; aaaaaaaaaaaaaaa; aaaaaaaaaaaa}
      , (aaaaaaaaaa, aaaaaaaaaaaa) ) ->
      xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
        xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx
  ) list

;;
List.map
  (* this is a loooooooooooooooooooooooooooooong commmmmmmmmmment
     blablablabla *)
  (function
  | A ->
      xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxx
        xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx
  ) list

let myfunc : int -> int = fun x ->
  let y = x + 1 in
  y

let myfunc : int -> int = function
  | x ->
      let y = x + 1 in
      y

let foo =
  protect ~f:(fun () ->
      ignore (CType_decl.add_types_from_decl_to_tenv tenv dec) ;
      ignore (CType_decl.add_types_from_decl_to_tenv tenv dec)
    ) ~foo:bar

(* G |- pth : m *)
let path : Path.t -> term_judg =
  (* ------------ x: m |- x: m

     G |- A: m[Dereference] ----------------------- G |- A.x: m

     G1 |- A: m[Dereference] G2 |- B: m[Dereference]
     ------------------------ (as for term application) G1 + G2 |- A(B): m *)
  fun pth ->
  match pth with
  | Path.Pident x -> single x
  | Path.Pdot (t, _) -> path t << Dereference
  | Path.Papply (f, p) -> join [path f << Dereference; path p << Dereference]

(* G |- pth : m *)
let path : Path.t -> term_judg =
  (* ------------ x: m |- x: m

     G |- A: m[Dereference] ----------------------- G |- A.x: m

     G1 |- A: m[Dereference] G2 |- B: m[Dereference]
     ------------------------ (as for term application) G1 + G2 |- A(B): m *)
  function
  | Path.Pident x -> single x
  | Path.Pdot (t, _) -> path t << Dereference
  | Path.Papply (f, p) -> join [path f << Dereference; path p << Dereference]

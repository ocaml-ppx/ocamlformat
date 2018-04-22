(* Note that {[ let ident : typ = exp ]} is different from {[ let (ident :
   typ) = exp ]}. The difference should be maintained *)

let _: int = x1

let (x: int) = x2

let _: int = x3

let x : int = x4

let _ =
  let (x : int) = x in
  let x : int = x in
  let _ : int = x in
  let _ : int = x in
  ()

let%ext _: int = x1

let%ext (x: int) = x2

let%ext _: int = x3

let%ext x : int = x4

let%ext _ =
  let%ext (x : int) = x in
  let%ext x : int = x in
  let%ext _ : int = x in
  let%ext _ : int = x in
  ()

let f: 'a. 'a ty -> 'a = fun y -> g y

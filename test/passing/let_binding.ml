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

let [%ext let x = 3] = 2

let [%ext: [%exp let x = 3]] = 2

let f: 'a. 'a ty -> 'a = fun y -> g y

let f (A _ | B | C) = ()

let f
    ( AAAAAAAAAAAAAAAAAAAAAAAAAAAAa _ | BBBBBBBBBBBBBBBBBBBBBbb
    | CCCCCCCCCCCCCCCCCCCCCCccccc ) =
  ()

let f
    ( AAAAAAAAAAAAAAAAAAAAAAAAAAAAa
        ( EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEe | FFFFFFFFFFFFFFFFFFFFFFFFFFf
        | GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGggggggggg )
    | BBBBBBBBBBBBBBBBBBBBBbb | CCCCCCCCCCCCCCCCCCCCCCccccc ) =
  ()

let f (AAA (EEEEEEE | FFFFF | GGGGG) | BBBBBB | CCCCCCC) = ()

let f = function AAA (EEEEEEE | FFFFF | GGGGG) | BBBBBB | CCCCCCC -> ()

let f = function EEEEEEE | F | GGGGG | B | CCCCCCC -> ()

let f = function
  | EEEEEEE | FFFFFFFFFFFFFFFFFFFFFFF | GGGGG
   |BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBbb | CCCCCCC ->
      ()

let _: t -> t -> int = (compare : int list -> int list -> int)

let _ =
  let[@test] rec f = x in
  y

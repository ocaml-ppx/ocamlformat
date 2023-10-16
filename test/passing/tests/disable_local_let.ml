let f () =
  let [@ocamlformat "disable"] x = y
  in
  ()

let f () =
  let x = y [@@ocamlformat "disable"]
  in
  ()

let f () =
  let open [@ocamlformat "disable"] X
  in
  ()

let f () =
  let module [@ocamlformat "disable"] X = Y
  in
  ()

let f () =
  let exception [@ocamlformat "disable"] X
  in
  ()

class c =
  let open [@ocamlformat "disable"] X
  in
  x

class type c =
  let open [@ocamlformat "disable"] X
  in
  x

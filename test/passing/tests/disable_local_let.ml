let f () =
  let [@ocamlformat "disable"] x = y
  in
  ()

let f () =
  let x = y [@@ocamlformat "disable"]
  in
  ()

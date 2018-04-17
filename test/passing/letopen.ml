let _ = Some_module.Submodule.(a + b)

let _ =
  let open Some_module.Submodule in
  AAAAAAAAAAAAAAAAAAAAAAAAAAAA.(a + b)

let _ =
  let open Some_module.Submodule in
  let module A = MMMMMM in
  a + b + c

let _ =
  let open Some_module.Submodule in
  let exception A of int in
  a + b

let _ =
  let open Some_module.Submodule in
  [%except {| result |}]

let _ =
  let open Some_module.Submodule in
  [%except {| loooooooooooooooooooooooooong result |}]

let _ =
  let open Some_module.Submodule in
  let x = a + b in
  let y = f x in
  y

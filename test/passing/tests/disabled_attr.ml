let _ =
  let disabled = {|
  |}[@ocamlformat "disable"] in
  ()

let _ =
  let disabled = "
  "[@ocamlformat "disable"] in
  ()

let _ =
  let disabled =
    begin
      (* xxx

         xxx *)

      y
    end[@ocamlformat "disable"]
  in
  ()

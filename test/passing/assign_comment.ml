(* see https://github.com/ocaml-ppx/ocamlformat/issues/1088 *)

let r = ref 0
;;

let _ =
  r (* _________________________________________________________________ *) := 1
;;

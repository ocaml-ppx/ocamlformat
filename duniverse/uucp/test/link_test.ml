(*
  Compile with:
  ocamlfind ocamlopt -package uucp -linkpkg -o link_test.native link_test.ml
*)

let () = ignore (Uucp.Age.age (Uchar.of_int 0x1F42B))

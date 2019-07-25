(*$
  ;;
  for i = 1 to 3 do
    Printf.printf "let x%d = %d\n" i i
  done
*)
let x1 = 1

(*$*)

let x = 1

(*$
  ;;
  print_newline () ;
  List.iter
    (fun s -> Printf.printf "let ( %s ) = Pervasives.( %s )\n" s s)
    ["+"; "-"; "*"; "/"]
*)
(*$*)
let y = 2

(*$
  ;;
  #use "import.cinaps"

  ;;
  List.iter all_fields ~f:(fun (name, type_) ->
      printf "\nexternal get_%s : unit -> %s = \"get_%s\"" name type_ name)
*)
external get_name : unit -> string = "get_name"

(*$*)

let x = 1

(*$
  ;;
  let x = 1 in
  (* fooooooo *)
  let y = 2 in
  (* foooooooo *)
  z
*)
(*$*)

let foo = foo

(*$QR foo Q.small_int (fun i-> foo i (+) [1;2;3] = List.fold_left (+) i
  [1;2;3] ) *)
let foo = foo

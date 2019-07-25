(*$
  ;;
  for i = 1 to 3 do
    Printf.printf "let x%d = %d\n" i i
  done
*)
let x1 = 1

let x2 = 2

let x3 = 3

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

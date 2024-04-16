{

  open Parentheses

}

rule read accu = parse
  | '('
      { read (L :: accu) lexbuf }
  | ')'
      { read (R :: accu) lexbuf }
  | '\n'
  | eof
      { List.rev accu }
  | _ as c
      { failwith (Printf.sprintf "Invalid input character: %C." c) }

{

  let () =
    print_endline
      (if parse (Array.of_list (read [] (Lexing.from_channel stdin))) then "yes" else "no");
    flush stdout

}


let _ =
  if b then e
  else (
    something loooooooooooooooooooooooooooooooong enough to_trigger a break ;
    this is more )
[@@ocamlformat "if-then-else=keyword-first"]

let _ =
  if b then e
  else (
    something loooooooooooooooooooooooooooooooong enough to_trigger a break ;
    this is more )
[@@ocamlformat.typo "if-then-else=keyword-first"]


let _ =
  if b then e
  else (
    something loooooooooooooooooooooooooooooooong enough to_trigger a break ;
    this is more )
[@@ocamlformat 1, "if-then-else=keyword-first"]

let _ =
  if b then e
  else (
    something loooooooooooooooooooooooooooooooong enough to_trigger a break ;
    this is more )
[@@ocamlformat "if-then-else=bad"]


module M = struct
  [@@@ocamlformat "if-then-else=keyword-first"]

  let _ =
    if b then e
    else (
      something loooooooooooooooooooooooooooooooong enough to_trigger a break ;
      this is more )
           [@@ocamlformat "if-then-else=bad"]

             let _ =
    if b then e
    else (
      something loooooooooooooooooooooooooooooooong enough to_trigger a break ;
      this is more )

             let _ =
    if b then e
    else (
      something loooooooooooooooooooooooooooooooong enough to_trigger a break ;
      this is more )
           
           [@@@ocamlformat "if-then-else=compact"]

                 let _ =
    if b then e
    else (
      something loooooooooooooooooooooooooooooooong enough to_trigger a break ;
      this is more )
           
end

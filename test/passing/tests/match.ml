let _ = match a with A -> ( match b with B -> b | C -> c ) | D -> D

let _ =
  match a with
  | AAAAAAAAAA -> (
    match bbbbbbbbbbbbb with
    | BBbbbbbbbbbbbbb -> bbbbbbbbbbbb
    | CCCCCCCCCCCCCCcc -> ccccccccccccccccc )
  | DDDDDDDDDDDDDDd -> DDDDDDDDDDDDDDDDdD

let _ =
  match a with
  | AAAAAAAAAA -> (
      let x = 3 in
      match bbbbbbbbbbbbb with
      | BBbbbbbbbbbbbbb -> bbbbbbbbbbbb
      | CCCCCCCCCCCCCCcc -> ccccccccccccccccc )
  | DDDDDDDDDDDDDDd -> DDDDDDDDDDDDDDDDdD

let _ =
  match x with
  | _ -> (
    match
      something long enough to_break
        _________________________________________________________________
    with
    | AAAAAAAAAA -> (
        let x = 3 in
        match bbbbbbbbbbbbb with
        | BBbbbbbbbbbbbbb -> bbbbbbbbbbbb
        | CCCCCCCCCCCCCCcc -> ccccccccccccccccc )
    | DDDDDDDDDDDDDDd -> DDDDDDDDDDDDDDDDdD )

let x =
  let g =
    match x with
    | `A -> ( fun id -> function A -> e ; e | _ -> () )
    | `B -> ( fun id -> function A -> e ; e | _ -> () )
  in
  ()

let x =
  let g =
    match x with
    | `A -> ( fun id -> function A -> () | B -> () )
    | `B -> ( fun id -> function A -> () | _ -> () )
  in
  ()

let x =
  let g =
    match x with
    | `A -> ( function A -> () | B -> () )
    | `B -> ( function A -> () | _ -> () )
  in
  ()

let x =
  let g = match x with `A -> fun (A | B) -> () | `B -> fun (A | _) -> () in
  ()

let _ = match x with _ -> b >>= fun () -> c

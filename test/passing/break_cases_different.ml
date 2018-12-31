[@@@ocamlformat "break-cases=different"]

let f x = function
  | C | P (this, test, [is; wide; enough; _to; break], [the; line]) | A | K
    -> 1
  | D ->
      let a = "this" in
      let b = "breaks" in
      ()

let f =
  let g = function
    | H when x y <> k -> 2
    | T | P | U -> 3
  in
  fun x g t h y u ->
    match x with
    | E -> 4
    | Z | P | M -> (
      match y with
      | O -> 5
      | P when h x -> (
          function
          | A -> 6 ) )

;;
match x with
| true -> (
  match y with
  | true -> "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  | false -> "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" )
| false -> "cccccccccccccccccccccccccccccc"

;;
match x with
| "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", yyyyyyyyyy
  when fffffffffffffff bbbbbbbbbb yyyyyyyyyy -> ()
| _ -> ()

let is_sequence exp =
  match exp.pexp_desc with
  | Pexp_sequence _
  | Pexp_extension
      ( _
      , PStr [{pstr_desc= Pstr_eval ({pexp_desc= Pexp_sequence _}, []); _}]
      ) -> true
  | _ -> false

let _ =
  let f x y =
    match x with
    | None -> false
    | Some looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong
      -> (
      match y with
      | Some _ -> true
      | None -> false )
  in
  ()

let () =
  match fooooo with
  | x -> x

let () =
  match foooo with
  | x | x | x -> x
  | y | foooooooooo | fooooooooo -> y
  | foooooo when ff fff fooooooooooooooooooo ->
      foooooooooooooooooooooo foooooooooooooooooo

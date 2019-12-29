type x =
  | Fooooooooooo
  | Fooooooooooooo
  | Fooooooooooooooo
  | Foooooooooooooooooo
  | Foo                 of padding * int array
  | Foooooooo           of padding * int array
  | Fooooooooo          of padding * int array
  | Fooooooooooo        of padding * int array * int array
  (* fooooooooooooooooo *)
  | Fooooooooooo
      (* fooooooooooooooooooo *) of
      padding * int array * int array
  (* fooooooooooooooooo *)
  | Foooooooooo         of padding * int array * int array
  | Foooo               of padding * int array * int array
  | Fooooooo            of padding * int array * int array
  | Foooooo             of int array

type x =
  [ `Foooooooooooooo
  | `Foooooooooooooooooo
  | (* fooooo *)
    (* fooooo *)
    `Foooooooooooooooooo
  | `Foooooo (* fooooo *) of padding * int array
  | `Fooooooooo (* fooooooooooooooooo *) of padding * int array * int array
  | (* fooooooooooooooooo *)
    `Fooooooooo          of int array
  | `Foooooooooooo       of int array ]

(* alignment disabled due to unpredictible patterns *)
let fooooooooooo =
  match foooooooooooooooooooooooo with
  | Bfooooooooooooooooo -> foooooooooooo
  | C (a, b, c, d)      -> fooooooooooooooooooo
  | _                   -> fooooooooooooooooooo

let fooooooooooo =
  match foooooooooooooooooooooooo with
  | Bfooooooooooooooooo -> foooooooooooo
  | Xxxxxxxxxxxxxx (* not aligned because of cmt *) -> foooooooooooooooooo
  | `Foooooooo -> fooo
  (* comments before are not an issue *)
  | "foooooooooooooo" -> foo
  | 3453535353533 -> foooooooooooooooooo
  | _ -> fooooooooooooooooooo

let fooooooooooo =
  match foooooooooooooooooooooooo with
  | Bfooooooooooooooooo -> foooooooooooo
  | Xxxxxxxxxxxxxx (* foooo *) -> fooooooooooooooooooo
  | `Foooooooo -> fooo
  | "foooooooooooooo"
  (* foooooooooooooooooooooo foooooooooooooooo foooooooooooooo fooooooooo*)
    ->
      foo
  | 3453535353533 ->
      foooooooooooooooooo
      (* foooooooooooooooooooooo foooooooooooooooo foooooooooooooo fooooooooo*)
  | _ -> fooooooooooooooooooo

let _ =
  match f with
  | 'a'     -> 1
  | '\n'    -> 2
  | '\t'    -> 2
  | '\x12'  -> 2
  | pattern -> 3

type t =
  | ( :: ) of a * b
  | []     of looooooooooooooooooooooooooooooooooooooong_break

let _ =
  match (a, b) with
  | A, B   -> a
  | AA, BB -> b
  | p      -> c

let _ =
  match (a, b) with
  | A, B           -> a
  | AA, BB         -> b
  | longer_pattern -> c

let _ =
  match (a, b) with
  | (A, B), B      -> a
  | pat, BB        -> b
  | longer_pattern -> c

let _ =
  match f with
  | Foo     -> toto
  | Bar ijx -> bar ijx

let _ =
  match f with
  | `Foo     -> toto
  | `Bar ijx -> bar ijx

let _ =
  match (foooooooooooooo, foooooooooooooo) with
  | Some a, Some b                     -> a + b
  | None, _                            -> 1
  | Some _, None                       -> 2
  | Foo_types.Foo_fooo_foooo_fooo_fooo -> Foo.BarFoo.int_as_foo 5 bar
  | Foo_types.Foo_foooooo              -> Fooo_bar.int_as_bar 6 foooo

let _ =
  match (foooooooooooooo, foooooooooooooo) with
  | [x]    -> x
  | [_; x] -> x
  | _      -> 0

type x = Foooooooo of int | Fooooooooooooo of int

type x = [`Foooooooo of int | `Fooooooooooooo of int]

[@@@ocamlformat "type-decl=sparse"]

type x =
  | Foooooooo      of int
  | Fooooooooooooo of int

type x =
  [ `Foooooooo      of int
  | `Fooooooooooooo of int ]

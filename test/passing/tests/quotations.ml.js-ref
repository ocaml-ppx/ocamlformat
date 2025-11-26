#syntax quotations on

let simple_number = <[123]>

let npower x_quoted n =
  let rec loop m = if m = 0 then <[1]> else <[$x_quoted * $(loop (m - 1))]> in
  loop n
;;

let example_function n = <[fun x -> $(npower <[x]> n)]>

let longer_example m n =
  let first_quote = <[fun x -> $(npower <[x]> m)]> in
  let second_quote = <[fun y -> $(npower <[y]> n)]> in
  let combined_quote = <[fun x -> $second_quote ($first_quote x)]> in
  <[$combined_quote, $combined_quote]>
;;

let even_longer m n =
  <[ fun x y ->
     let xm = $(npower <[x]> m) in
     let yn = $(npower <[y]> n) in
     let z = xm + yn in
     $(npower <[z]> (m + n)) ]>
;;

type s = <[int]>
type t = s expr

let f (x : t) : <[$(s) * $(s)]> expr = <[$x, $x + 1]>

let double =
  <[ let x = <[42]> in
     <[123 + $x]> ]>
;;

(* Long lines and breaks *)

let _ =
  <[ let aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa = 1 in
     [ aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
     ; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
     ; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
     ] ]>
;;

let _ =
  <[ fun xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
       yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy
       zzzzzzzzzzzzzzzzzzzzzzz ->
     $( <[ xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
           + yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy ]>
      , <[zzzzzzzzzzzzzzzzzzzzzzz]> ) ]>
;;

let _ =
  let xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx =
    <[fun aaaaaaaaaaaaaaaaaaaaa -> aaaaaaaaaaaaaaaaaaaaa, aaaaaaaaaaaaaaaaaaaaa]>
  in
  <[ ( $xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
     , $xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
         (fun bbbbbbbbbbbbbbbbbbbbb -> bbbbbbbbbbbbbbbbbbbbb) ) ]>
;;

let _ =
  fun xxxxxxxxxxxxxxxxxxxx ->
  <[ fun zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz ->
     $((fun yyyyyyyyyyyyyyyyyyyy -> <[zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz + 42]>)
         xxxxxxxxxxxxxxxxxxxx) ]>
;;

(* Comments *)

(* let q = <[fun xxxxxxxxxx -> (xxxxxxxxxx, xxxxxxxxxx, xxxxxxxxxx)]> in let f a =
   <[$q $a + $q $a + $q $a]> in f <[111111111111111111]> *)

let _ =
  <[ 2222222222222222222222222222222222
     + 555555555555555555555555555555
     + 44444444444444444444
     - 3333333333333333333333333333
     (* these comments are meant to be completely ignored in the formatting and should be
        processed correctly *)
     + 987654321
     - 654987987321654 ]>
;;

let _ =
  <[ (* a *)
     1
     (* b *) + 2
     + 3
     + (* c *) 4
     + (* dd *)
       (* e *) 5
     (* f *) + 6
     (* ggg *)
     (* h *) + 7 (* ii *) ]>
;;

let _ =
  (* 1 *)
  <[ (* 2 *)
     fun (* 3 *) x (* 4 *) ->
     (* 5 *)
     $((* 6 *)
         (fun (* 7 *)
              (* 8 *) y (* 9 *) -> (* 10 *) y (* 11 *))
         (* 12 *) <[(* 13 *) x (* 14 *)]> (* 15 *))
     (* 16 *) ]>
;;

(* 17 *)

type 'a t = <[int (* 1 *) -> $('a (* 2 *) -> 'a (* 3 *) -> <[int]> (* 4 *)) (* 5 *)]>
(* 6 *)

(* Attributes *)

let _ =
  <[ (fun xxxxxxxxxxxxx -> (555555 + xxxxxxxxxxxxx) [@nontail]) 1111333333777777 [@inlined]
  ]>
;;

let _ = <[fun x -> $((fun y -> y) (<[x]> [@nontail])) [@inlined]]> [@boxed]

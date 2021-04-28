open Printf
open Parentheses

let generate n =
  let w = Array.make n lpar in
  let rec fill s k =
    if k = 0 then
      ()
    else begin
      w.(s) <- lpar;
      let j = 2 + 2 * ((Random.int k) / 2) in
      w.(s+j-1) <- rpar;
      fill (s+1) (j-2);
      fill (s+j) (k-j)
    end
  in
  fill 0 n;
  w

module P =
  CYK.Make(Parentheses)

let bench size =
  let word = generate size in
  let accepted = P.parse word in
  assert accepted;
  (* Toggle one parenthesis so as to create an invalid input. *)
  let i = Random.int size in
  word.(i) <- toggle word.(i);
  let accepted = P.parse word in
  assert (not accepted)

let bench n size =
  for _k = 0 to n do
    bench size
  done;
  printf "Tested %d valid words and %d invalid words of length %d.\n%!" n n size

let () =
  let n = 100 in
  bench n 10;
  bench n 20;
  bench n 30;
  bench n 40;
  bench n 50;
  bench n 60;
  ()

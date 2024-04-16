(* Tests. *)

open Fix

(* -------------------------------------------------------------------------- *)

(* Test that [defensive_fix] detects cycles. *)

module M =
  Memoize.ForType(Glue.INT)

(* We define a cyclic function where [f x] calls [f (x-1)] when [x] is positive
   and [f 0] calls [f n] for some fixed [n]. *)

let test n =
  let f =
    M.defensive_fix (fun f x ->
      if x = 0 then f n else f (x - 1) + 1
    )
  in
  Printf.printf "Testing n = %d... %!" n;
  let x = 2 * n in
  match f x with
  | y ->
      Printf.printf "failure! f(%d) returns %d.\n%!" x y
  | exception M.Cycle (zs, z) ->
      Printf.printf "success! A cycle is detected:\n";
      List.iter (fun z ->
        Printf.printf "%d, " z
      ) zs;
      Printf.printf "%d" z;
      Printf.printf "\n%!"

(* Test several values of [n]. *)

let () =
  test 0;
  test 5;
  test 10;
  test 100;
  ()

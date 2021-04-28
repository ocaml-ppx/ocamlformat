open Fix

(* -------------------------------------------------------------------------- *)

(* Give a direct definition of Fibonacci's function. We tabulate the function
   up to a certain bound. *)

let size =
  100000

let reference : int -> int =
  let fib = Array.make size 1 in
  for n = 2 to size - 1 do
    fib.(n) <- fib.(n-2) + fib.(n-1)
  done;
  Array.get fib

(* -------------------------------------------------------------------------- *)

(* Check that a candidate [fib] yields the same results as the reference
   implementation. *)

let test name fib =
  Printf.printf "Testing %s... %!" name;
  let duration = Time.chrono (fun () ->
    for i = 0 to size/2 do
      assert (fib i = reference i)
    done;
    for i = size - 1 downto 0 do
      assert (fib i = reference i)
    done;
    for i = 0 to size - 1 do
      assert (fib i = reference i)
    done
  ) in
  Printf.printf "Success (%02fs).\n%!" duration

(* -------------------------------------------------------------------------- *)

(* Instantiate [Memoize] for keys of type [int]
   and properties of type [int]. *)

module M =
  Memoize.ForType(Glue.INT)

(* -------------------------------------------------------------------------- *)

(* Using [Memoize], define a memoising (therefore asymptotically efficient)
   version of Fibonacci's function. *)

let fib : int -> int =
  M.fix (fun fib n ->
    if n <= 1 then 1 else fib (n-2) + fib (n-1)
  )

let () =
  test "fib based on Memoize.fix" fib

(* We can also use [defensive_fix], which should be indistinguishable from
   [fix], as there are no dependency cycles here. *)

let fib : int -> int =
  M.defensive_fix (fun fib n ->
    if n <= 1 then 1 else fib (n-2) + fib (n-1)
  )

let () =
  test "fib based on Memoize.defensive_fix" fib

(* -------------------------------------------------------------------------- *)

(* Instantiate [Fix] for keys of type [int]
   and properties of type [int option]. *)

module F =
  Fix.ForType
    (Glue.INT)
    (Prop.Option(Glue.INT))

(* -------------------------------------------------------------------------- *)

(* Using [Fix], define another efficient version of Fibonacci's function. *)

(* This is admittedly a degenerate use case for [Fix]. Here, the dependencies
   are acyclic, so it would be preferable to use a simpler and more efficient
   memoising fixed point combinator. Doing so would obviate the need for
   options. Here, options are required because [Fix] requires a bottom
   element, and [Fix] requires such an element because it needs one, in
   general, in order to deal with cyclic dependencies. *)

let fib : int -> int option =
  F.lfp (fun n fib ->
    if n <= 1 then
      Some 1
    else
      match fib (n-2), fib (n-1) with
      | Some p, Some q ->
	  Some (p + q)
      | _, _ ->
	  None
  )

let fib : int -> int =
  fun n ->
    match fib n with Some k -> k | None -> assert false

let () =
  test "fib based on Fix.lfp" fib

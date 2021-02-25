open! Import
open! Random

module State = struct
  include State

  let%test_unit ("random int above 2^30"[@tags "64-bits-only"]) =
    let state = make [| 1; 2; 3; 4; 5 |] in
    for _ = 1 to 100 do
      let bound = Int.shift_left 1 40 in
      let n = int state bound in
      if n < 0 || n >= bound
      then
        failwith (Printf.sprintf "random result %d out of bounds (0,%d)" n (bound - 1))
    done
  ;;
end

external random_seed : unit -> Caml.Obj.t = "caml_sys_random_seed"

let%test_unit _ =
  (* test that the return type of "caml_sys_random_seed" is what we expect *)
  let module Obj = Caml.Obj in
  let obj = random_seed () in
  assert (Obj.is_block obj);
  assert (Obj.tag obj = Obj.tag (Obj.repr [| 13 |]));
  for i = 0 to Obj.size obj - 1 do
    assert (Obj.is_int (Obj.field obj i))
  done
;;

module type T = sig
  type t [@@deriving compare, sexp_of]
end

(* We test that [count] trials of [generate ()] all produce values between [min, max], and
   generate at least one value between [lo, hi]. *)
let test (type t) here m count generate ~min ~max ~check_range:(lo, hi) =
  let (module T : T with type t = t) = m in
  let between t ~lower_bound ~upper_bound =
    T.compare t lower_bound >= 0 && T.compare t upper_bound <= 0
  in
  let generated =
    List.init count ~f:(fun _ -> generate ()) |> List.dedup_and_sort ~compare:T.compare
  in
  require
    here
    (List.for_all generated ~f:(fun t -> between t ~lower_bound:min ~upper_bound:max))
    ~if_false_then_print_s:
      (lazy
        [%message
          "generated values outside of bounds"
            (min : T.t)
            (max : T.t)
            (generated : T.t list)]);
  require
    here
    (List.exists generated ~f:(fun t -> between t ~lower_bound:lo ~upper_bound:hi))
    ~if_false_then_print_s:
      (lazy
        [%message
          "did not generate value inside range"
            (lo : T.t)
            (hi : T.t)
            (generated : T.t list)])
;;

let%expect_test "float" =
  test
    [%here]
    (module Float)
    1_000
    (fun () -> float 100.)
    ~min:0.
    ~max:100.
    ~check_range:(10., 20.);
  [%expect {||}]
;;

let%expect_test "float_range" =
  test
    [%here]
    (module Float)
    1_000
    (fun () -> float_range (-100.) 100.)
    ~min:(-100.)
    ~max:100.
    ~check_range:(-20., -10.);
  [%expect {||}]
;;

let%expect_test "int" =
  test [%here] (module Int) 1_000 (fun () -> int 100) ~min:0 ~max:99 ~check_range:(10, 20);
  [%expect {||}]
;;

let%expect_test "int_incl" =
  test
    [%here]
    (module Int)
    1_000
    (fun () -> int_incl (-100) 100)
    ~min:(-100)
    ~max:100
    ~check_range:(-20, -10);
  [%expect {||}];
  test
    [%here]
    (module Int)
    1_000
    (fun () -> int_incl 0 Int.max_value)
    ~min:0
    ~max:Int.max_value
    ~check_range:(0, Int.max_value / 100);
  [%expect {||}];
  test
    [%here]
    (module Int)
    1_000
    (fun () -> int_incl Int.min_value Int.max_value)
    ~min:Int.min_value
    ~max:Int.max_value
    ~check_range:(Int.min_value / 100, Int.max_value / 100);
  [%expect {||}]
;;

let%expect_test "int32" =
  test
    [%here]
    (module Int32)
    1_000
    (fun () -> int32 100l)
    ~min:0l
    ~max:99l
    ~check_range:(10l, 20l);
  [%expect {||}]
;;

let%expect_test "int32_incl" =
  test
    [%here]
    (module Int32)
    1_000
    (fun () -> int32_incl (-100l) 100l)
    ~min:(-100l)
    ~max:100l
    ~check_range:(-20l, -10l);
  [%expect {||}];
  test
    [%here]
    (module Int32)
    1_000
    (fun () -> int32_incl 0l Int32.max_value)
    ~min:0l
    ~max:Int32.max_value
    ~check_range:(0l, Int32.( / ) Int32.max_value 100l);
  [%expect {||}];
  test
    [%here]
    (module Int32)
    1_000
    (fun () -> int32_incl Int32.min_value Int32.max_value)
    ~min:Int32.min_value
    ~max:Int32.max_value
    ~check_range:(Int32.( / ) Int32.min_value 100l, Int32.( / ) Int32.max_value 100l);
  [%expect {||}]
;;

let%expect_test "int64" =
  test
    [%here]
    (module Int64)
    1_000
    (fun () -> int64 100L)
    ~min:0L
    ~max:99L
    ~check_range:(10L, 20L);
  [%expect {||}]
;;

let%expect_test "int64_incl" =
  test
    [%here]
    (module Int64)
    1_000
    (fun () -> int64_incl (-100L) 100L)
    ~min:(-100L)
    ~max:100L
    ~check_range:(-20L, -10L);
  [%expect {||}];
  test
    [%here]
    (module Int64)
    1_000
    (fun () -> int64_incl 0L Int64.max_value)
    ~min:0L
    ~max:Int64.max_value
    ~check_range:(0L, Int64.( / ) Int64.max_value 100L);
  [%expect {||}];
  test
    [%here]
    (module Int64)
    1_000
    (fun () -> int64_incl Int64.min_value Int64.max_value)
    ~min:Int64.min_value
    ~max:Int64.max_value
    ~check_range:(Int64.( / ) Int64.min_value 100L, Int64.( / ) Int64.max_value 100L);
  [%expect {||}]
;;

let%expect_test "nativeint" =
  test
    [%here]
    (module Nativeint)
    1_000
    (fun () -> nativeint 100n)
    ~min:0n
    ~max:99n
    ~check_range:(10n, 20n);
  [%expect {||}]
;;

let%expect_test "nativeint_incl" =
  test
    [%here]
    (module Nativeint)
    1_000
    (fun () -> nativeint_incl (-100n) 100n)
    ~min:(-100n)
    ~max:100n
    ~check_range:(-20n, -10n);
  [%expect {||}];
  test
    [%here]
    (module Nativeint)
    1_000
    (fun () -> nativeint_incl 0n Nativeint.max_value)
    ~min:0n
    ~max:Nativeint.max_value
    ~check_range:(0n, Nativeint.( / ) Nativeint.max_value 100n);
  [%expect {||}];
  test
    [%here]
    (module Nativeint)
    1_000
    (fun () -> nativeint_incl Nativeint.min_value Nativeint.max_value)
    ~min:Nativeint.min_value
    ~max:Nativeint.max_value
    ~check_range:
      (Nativeint.( / ) Nativeint.min_value 100n, Nativeint.( / ) Nativeint.max_value 100n);
  [%expect {||}]
;;

(* The int63 functions come from [Int63] rather than [Random], but we test them here
   along with the others anyway. *)

let%expect_test "int63" =
  let i = Int63.of_int in
  test
    [%here]
    (module Int63)
    1_000
    (fun () -> Int63.random (i 100))
    ~min:(i 0)
    ~max:(i 99)
    ~check_range:(i 10, i 20);
  [%expect {||}]
;;

let%expect_test "int63_incl" =
  let i = Int63.of_int in
  test
    [%here]
    (module Int63)
    1_000
    (fun () -> Int63.random_incl (i (-100)) (i 100))
    ~min:(i (-100))
    ~max:(i 100)
    ~check_range:(i (-20), i (-10));
  [%expect {||}];
  test
    [%here]
    (module Int63)
    1_000
    (fun () -> Int63.random_incl (i 0) Int63.max_value)
    ~min:(i 0)
    ~max:Int63.max_value
    ~check_range:(i 0, Int63.( / ) Int63.max_value (i 100));
  [%expect {||}];
  test
    [%here]
    (module Int63)
    1_000
    (fun () -> Int63.random_incl Int63.min_value Int63.max_value)
    ~min:Int63.min_value
    ~max:Int63.max_value
    ~check_range:
      (Int63.( / ) Int63.min_value (i 100), Int63.( / ) Int63.max_value (i 100));
  [%expect {||}]
;;

let%expect_test "ascii" =
  test
    [%here]
    (module Char)
    1_000
    ascii
    ~min:Char.min_value
    ~max:(Char.of_int_exn 127)
    ~check_range:('a', 'z');
  [%expect {||}]
;;

let%expect_test "char" =
  test
    [%here]
    (module Char)
    1_000
    char
    ~min:Char.min_value
    ~max:Char.max_value
    ~check_range:('\128', '\255');
  [%expect {||}]
;;

let%test_module "float upper bound is inclusive despite docs" =
  (module struct
    (* The fact that this test passes doesn't demonstrate that the bug has gone away,
       since the test was explicitly contrived to provoke the bug. *)

    let%expect_test _ =
      (* No choice but to use magic, since trying to brute-force a seed that causes this
         behavior takes too long.

         Hack random_state so that the next 60 bits produced by [bits] are all 1s. *)
      let random_state =
        let st = Array.create 0 ~len:55 in
        st.(1) <- 0b11111__11111__11111__11111__11111__00000;
        st.(2) <- 0b11111__11111__11111__11111__11111__00000;
        (Caml.Obj.magic (st, 0) : Random.State.t)
      in
      require
        [%here]
        ~cr:CR_someday
        (Float.( < ) (Random.State.float random_state 1.) 1.);
      [%expect {| |}]
    ;;

    (* This bug is more clearly illustrated by copying the implementation of
       [Random.float] from the stdlib (which is just re-exported by Base).

       Basically, when [r1 /. scale +. r2] requires more than 53 bits of precision, and
       [bits2] consists of all 1s, rounding causes [rawfloat] to return 1. *)

    let rawfloat bits1 bits2 =
      let scale = 1073741824.0
      and r1 = Caml.float bits1
      and r2 = Caml.float bits2 in
      ((r1 /. scale) +. r2) /. scale
    ;;

    let%expect_test "likelihood of failure" =
      (* test 256 states of the random number generator, highest as 60-bit numbers, out of
         which 64 would have yield a float exactly equal to 1 if [Random.State.float] was
         not recursive. *)
      let lbound = (1 lsl 30) - (1 lsl 8) in
      let ubound = (1 lsl 30) - 1 in
      let bits2 = ubound in
      let failures = ref 0 in
      for bits1 = lbound to ubound do
        let open Float.O in
        if rawfloat bits1 bits2 >= 1. then Int.incr failures
      done;
      let prob = Caml.float !failures *. 0x1p-60 in
      print_s [%message "likelihood of failure" (failures : int ref) (prob : float)];
      [%expect
        {|
        ("likelihood of failure"
          (failures 64)
          (prob     5.5511151231257827E-17)) |}]
    ;;
  end)
;;

let%expect_test _ = e

let%bench "test" = fun () -> ()

let%expect_test _ =
  assert false ;
  [%expect.unreachable]
    [@@expect.uncaught_exn
      {|
      (* CR expect_test_collector: This test expectation appears to contain a backtrace.
         This is strongly discouraged as backtraces are fragile.
         Please change this test to not include a backtrace. *)

      "Assert_failure test.ml:5:6"
      Raised at file "test.ml", line 4, characters 6-18
      Called from file "collector/expect_test_collector.ml", line 225, characters 12-19 |}]

let _ =
  assert false ;
  [%expect.unreachable]
    [@@expect.uncaught_exn
      {|
      "Assert_failure test.ml:5:6"
      Raised at file "test.ml", line 4, characters 6-18
      Called from file "collector/expect_test_collector.ml", line 225, characters 12-19 |}]

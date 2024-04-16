let () =
  let open Alcotest in
  let id () = () in
  run __FILE__
    [
      ( "test-a",
        [
          test_case "Run test case" `Quick id;
          test_case "Skip test case" `Quick skip;
        ] );
    ]

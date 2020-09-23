let test_split_use_file =
  let make_test name ~input ~expected =
    let test_name = "Split.use_file: " ^ name in
    let test_fun () =
      let actual =
        Ocamlformat_lib.Split_parser.Split.fragment Use_file input
      in
      Alcotest.(check (list string)) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [ make_test "empty" ~input:"" ~expected:[]
  ; make_test "multi empty" ~input:"\n\n" ~expected:[]
  ; make_test "invalid let" ~input:"let x" ~expected:["let x"]
  ; make_test "valid let" ~input:"let x = 2" ~expected:["let x = 2"]
  ; make_test "2 let 1l split" ~input:"let x = x\nlet y = y"
      ~expected:["let x = x"; "let y = y"]
  ; make_test "2 let 2l split" ~input:"let x = x\n\nlet y = y"
      ~expected:["let x = x"; "let y = y"]
  ; make_test "2 let mix split" ~input:"let x =\n\nx\nlet y = y"
      ~expected:["let x =\n\nx"; "let y = y"] ]

let test_split_interface =
  let make_test name ~input ~expected =
    let test_name = "Split.interface: " ^ name in
    let test_fun () =
      let actual =
        Ocamlformat_lib.Split_parser.Split.fragment Signature input
      in
      Alcotest.(check (list string)) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [ make_test "empty" ~input:"" ~expected:[]
  ; make_test "multi empty" ~input:"\n\n" ~expected:[] ]

let tests = test_split_use_file @ test_split_interface

let test_check_version =
  let test name ~config ~exe expected =
    let test_name = "compare_version: " ^ name in
    ( test_name
    , `Quick
    , fun () ->
        let got = Ocamlformat_lib.Conf.check_version ~config ~exe in
        Alcotest.check Alcotest.(result unit string) test_name expected got
    )
  in
  [ test "empty config" ~config:"" ~exe:""
      (Error "malformed version number \"\".")
  ; test "invalid config" ~config:"0.33..foo" ~exe:""
      (Error "malformed version number \"0.33..foo\".")
  ; test "invalid exe" ~config:"0.14.2" ~exe:"0.33..foo"
      (Error
         "expected ocamlformat version to be \"0.14.2\" but got \
          \"0.33..foo\".")
  ; test "unknown 'exe'" ~config:"0.14.2" ~exe:"unknown"
      (Error
         "expected ocamlformat version to be \"0.14.2\" but got \
          \"unknown\". Could not determine current ocamlformat version.")
  ; test "equal" ~config:"0.14.1" ~exe:"0.14.1" (Ok ())
  ; test "accepted" ~config:"0.14.1" ~exe:"0.14.1-15-g273f6f6-dirty" (Ok ())
  ; test "too old" ~config:"0.14.1" ~exe:"0.14.0"
      (Error
         "expected ocamlformat version to be \"0.14.1\" but got \"0.14.0\". \
          Please upgrade.")
  ; test "too old 2" ~config:"0.14.1" ~exe:"0.14.0-15-g273f6f6-dirty"
      (Error
         "expected ocamlformat version to be \"0.14.1\" but got \
          \"0.14.0-15-g273f6f6-dirty\". Please upgrade.")
  ; test "too recent" ~config:"0.14.0" ~exe:"0.14.1"
      (Error
         "expected ocamlformat version to be \"0.14.0\" but got \"0.14.1\". \
          Please downgrade.")
  ; test "too recent 2" ~config:"0.14.0" ~exe:"0.14.1-15-g273f6f6-dirty"
      (Error
         "expected ocamlformat version to be \"0.14.0\" but got \
          \"0.14.1-15-g273f6f6-dirty\". Please downgrade.")
  ; test "same commit" ~config:"0.14.0-15-g273f6f6" ~exe:"0.14.0-15-g273f6f6"
      (Ok ())
  ; test "different commit" ~config:"0.14.0-15-g273f6f6"
      ~exe:"0.14.0-16-fooooooo"
      (Error
         "expected ocamlformat version to be \"0.14.0-15-g273f6f6\" but got \
          \"0.14.0-16-fooooooo\".")
  ; test "expected commit" ~config:"0.14.0-15-g273f6f6" ~exe:"0.14.0"
      (Error
         "expected ocamlformat version to be \"0.14.0-15-g273f6f6\" but got \
          \"0.14.0\".") ]

let tests = test_check_version

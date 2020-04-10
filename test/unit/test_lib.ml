open Ocamlformat_lib

let input_name = "test.ml"

let debug = false

let quiet = true

let conf = Conf.conventional_profile

let opts = Conf.{debug; margin_check= false; format_invalid_files= false}

let test_impl =
  let test ~name ~source ~expected =
    ( "test_impl"
    , `Quick
    , fun () ->
        let actual =
          match
            Translation_unit.parse_and_format_impl ~input_name ~source conf
              opts
          with
          | Ok formatted -> formatted
          | Error e ->
              let fmt = Format.str_formatter in
              Translation_unit.print_error ~fmt ~debug ~quiet ~input_name e ;
              Format.flush_str_formatter ()
        in
        Alcotest.(check string) name expected actual )
  in
  [ test ~name:"empty" ~source:"" ~expected:""
  ; test ~name:"two let-bindings" ~source:"let x = foo\nlet y =\n  bar"
      ~expected:"let x = foo\n\nlet y = bar\n" ]

let test_intf =
  let test ~name ~source ~expected =
    ( "test_intf"
    , `Quick
    , fun () ->
        let actual =
          match
            Translation_unit.parse_and_format_intf ~input_name ~source conf
              opts
          with
          | Ok formatted -> formatted
          | Error e ->
              let fmt = Format.str_formatter in
              Translation_unit.print_error ~fmt ~debug ~quiet ~input_name e ;
              Format.flush_str_formatter ()
        in
        Alcotest.(check string) name expected actual )
  in
  [ test ~name:"empty" ~source:"" ~expected:""
  ; test ~name:"two val" ~source:"val x : foo\nval y :\n  bar"
      ~expected:"val x : foo\n\nval y : bar\n" ]

let tests = test_impl @ test_intf

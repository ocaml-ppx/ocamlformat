open! Base
open Ocamlformat_lib

let test_parse_and_format kind_name ~fg test_name ~input ~expected =
  let test_name =
    Stdlib.Format.sprintf "parse_and_format %s: %s" kind_name test_name
  in
  ( test_name
  , `Quick
  , fun () ->
      let actual =
        Translation_unit.parse_and_format fg ~input_name:"<test>"
          ~source:input Conf.default_profile
          Conf.
            {debug= false; margin_check= false; format_invalid_files= false}
        |> Result.map_error ~f:(fun e ->
               Translation_unit.Error.print Stdlib.Format.str_formatter e ;
               Stdlib.Format.flush_str_formatter () )
      in
      Alcotest.(check (result string string)) test_name expected actual )

let test_parse_and_format_signature =
  let make_test = test_parse_and_format "signature" ~fg:Signature in
  [make_test "val" ~input:"val x :\n \nint" ~expected:(Ok "val x : int\n")]

let test_parse_and_format_use_file =
  let make_test = test_parse_and_format "use_file" ~fg:Use_file in
  [make_test "let" ~input:"let x =\n\n y" ~expected:(Ok "let x = y\n")]

let tests =
  List.concat
    [test_parse_and_format_signature @ test_parse_and_format_use_file]

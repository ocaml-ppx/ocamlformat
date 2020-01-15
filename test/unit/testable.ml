let error =
  let pp fs =
    Ocamlformat_lib.Translation_unit.print_error ~fmt:fs ~debug:false
      ~quiet:false ~input_name:"_" ~exe:"ocamlformat.exe"
  in
  Alcotest.testable pp ( = )

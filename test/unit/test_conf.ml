open Ocamlformat

let find name =
  List.find (fun Config_option.UI.{names; _} ->
      List.exists (String.equal name) names )

let test_ui =
  let test test_name opt ~names:expected_names =
    ( test_name
    , `Quick
    , fun () ->
        let Config_option.UI.{names; _} = opt in
        Alcotest.check Alcotest.(list string) test_name expected_names names
    )
  in
  [ test "margin" (find "margin" Conf.UI.fmt_opts) ~names:["m"; "margin"]
  ; test "range" (find "range" Conf.UI.opr_opts) ~names:["range"]
  ; test "profile" Conf.UI.profile ~names:["p"; "profile"] ]

let tests = test_ui

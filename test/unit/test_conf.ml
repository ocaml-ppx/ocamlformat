open Ocamlformat_lib

let find name =
  List.find (fun Conf_decl.UI.{names; _} ->
      List.exists (String.equal name) names )

let test_ui =
  let test test_name opt ~names:expected_names =
    ( test_name
    , `Quick
    , fun () ->
        let Conf_decl.UI.{names; _} = opt in
        Alcotest.check Alcotest.(list string) test_name expected_names names
    )
  in
  [ test "margin" (find "margin" Conf.UI.fmt_opts) ~names:["m"; "margin"]
  ; test "profile" Conf.UI.profile ~names:["p"; "profile"] ]

let tests = test_ui

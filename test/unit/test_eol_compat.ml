open Ocamlformat_lib

let normalize_eol_tests =
  let test name ~exclude_locs input ~lf ~crlf =
    let test_name = "normalize_eol: " ^ name in
    ( test_name
    , `Quick
    , fun () ->
        let f ~prefix ~line_endings ~expected =
          Alcotest.check Alcotest.string (prefix ^ test_name) expected
            (Eol_compat.normalize_eol ~exclude_locs ~line_endings input)
        in
        f ~prefix:"lf+" ~line_endings:`Lf ~expected:lf ;
        f ~prefix:"crlf+" ~line_endings:`Crlf ~expected:crlf )
  in
  [ test "empty" ~exclude_locs:[] "" ~lf:"" ~crlf:""
  ; test "linebreaks" ~exclude_locs:[]
      {|
let _ =
  fooooooooooooooooooooooooooooooooooooooooooooooooooooo
  fooooooooooooooooooooooooooooooooooooooooooooooooooooo
|}
      ~lf:
        "\n\
         let _ =\n\
        \  fooooooooooooooooooooooooooooooooooooooooooooooooooooo\n\
        \  fooooooooooooooooooooooooooooooooooooooooooooooooooooo\n"
      ~crlf:
        "\r\n\
         let _ =\r\n\
        \  fooooooooooooooooooooooooooooooooooooooooooooooooooooo\r\n\
        \  fooooooooooooooooooooooooooooooooooooooooooooooooooooo\r\n"
  ; test "linebreaks in string" ~exclude_locs:[]
      "\nlet _ = \"aaa\\n\n         e\"\n"
      ~lf:"\nlet _ = \"aaa\\n\n         e\"\n"
      ~crlf:"\r\nlet _ = \"aaa\\n\r\n         e\"\r\n"
  ; test "lf in string with exclude_locs (lf)"
      ~exclude_locs:[(9, 26)]
      "\nlet _ = \"aaa\\n\n         e\"\n"
      ~lf:"\nlet _ = \"aaa\\n\n         e\"\n"
      ~crlf:"\r\nlet _ = \"aaa\\n\n         e\"\r\n"
  ; test "crlf in string with exclude_locs (lf)"
      ~exclude_locs:[(9, 27)]
      "\nlet _ = \"aaa\\n\r\n         e\"\n"
      ~lf:"\nlet _ = \"aaa\\n\r\n         e\"\n"
      ~crlf:"\r\nlet _ = \"aaa\\n\r\n         e\"\r\n"
  ; test "lf in string with exclude_locs (crlf)"
      ~exclude_locs:[(10, 27)]
      "\r\nlet _ = \"aaa\\n\n         e\"\r\n"
      ~lf:"\nlet _ = \"aaa\\n\n         e\"\n"
      ~crlf:"\r\nlet _ = \"aaa\\n\n         e\"\r\n"
  ; test "crlf in string with exclude_locs (crlf)"
      ~exclude_locs:[(10, 28)]
      "\r\nlet _ = \"aaa\\n\r\n         e\"\r\n"
      ~lf:"\nlet _ = \"aaa\\n\r\n         e\"\n"
      ~crlf:"\r\nlet _ = \"aaa\\n\r\n         e\"\r\n" ]

let tests = normalize_eol_tests

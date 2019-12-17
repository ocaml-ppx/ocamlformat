open Parse_wyc

module Locations = struct
  let test_impl =
    let test name input expected =
      let test_name = "impl " ^ name in
      ( test_name,
        `Quick,
        fun () ->
          let lexbuf = Lexing.from_string input in
          let actual = implementation lexbuf in
          Alcotest.(check (list Alcotest_ext.location))
            test_name expected actual )
    in
    [ test "empty" "" [] ]

  let tests = test_impl
end

let tests = [ ("locations", Locations.tests) ]

let () = Alcotest.run "Parse_wyc" tests

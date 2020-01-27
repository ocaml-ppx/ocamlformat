let impl_tests =
  let valid_test =
    {|
let fooooooooooooooo =
  let fooooooooooooo =
    let foooooooooooooo =
      foooooooooooooo
    in
    fooooo, fooooooo
  in
  fooooooooooooooooo;
  foooooooooooooo
|}
  in
  let invalid_after_eq_test = {|let fooooooooooooooo =|} in
  let invalid_after_in_test =
    {| (* line 1 *)
let fooooooooooooooooooooo = (* line 2 *)
  let foooooooooooooooooooo =
    let foooooooooooooooo =
      foooooooo
    in
    foooooo
  in (* line 8 *)
|}
  in
  let invalid_seq_modules_test =
    {|
module M = struct
  let foooooo = foooo

  let foooooooooooooo = (* line 5 *)

  let foooooooooooo = fooooo
end
|}
  in
  let not_closed_module_test =
    {|
module M = struct
  let foo = foo
  let foo =
|}
  in
  let not_closed_module_test_2 = {|
module M = struct
  let foo = foo in
|} in
  let not_closed_sig = {|
module K : sig
  type t
|} in
  let not_closed_begin = {| let x = if x then begin a |} in
  let not_closed_if = {| let x = if k |} in
  let not_closed_if_2 = {| let x = if k then |} in
  let invalid_if = {| let x = if k then else |} in
  let invalid_if_2 = {| let x = if k then x else |} in
  let not_closed_class = {| class c = object |} in
  let not_closed_class_2 = {| class c |} in
  let not_closed_class_3 = {| class c = |} in
  let not_closed_class_4 = {| class |} in
  let binop = {| x + |} in
  let many_not_closed =
    {|
let foooooo
= fooooooooo

let foooooooo
= bar baaaaar barrr

module K = struct
  let k

;;
begin
  let x = in
  ()

;;
let foooooo =
  fooooo
  foooooooo

let k =
|}
  in
  [
    ("empty", "", []);
    ("valid", valid_test, []);
    ("invalid after eq", invalid_after_eq_test, [ ((1, 0), (1, 22)) ]);
    ("invalid after in", invalid_after_in_test, [ ((2, 0), (8, 4)) ]);
    ("invalid seq modules", invalid_seq_modules_test, [ ((2, 0), (7, 28)) ]);
    ("not closed module", not_closed_module_test, [ ((2, 0), (4, 11)) ]);
    ("not closed module 2", not_closed_module_test_2, [ ((2, 0), (3, 18)) ]);
    ("not closed sig", not_closed_sig, [ ((2, 0), (3, 8)) ]);
    ("not closed begin", not_closed_begin, [ ((1, 1), (1, 26)) ]);
    ("not closed if", not_closed_if, [ ((1, 1), (1, 13)) ]);
    ("not closed if 2", not_closed_if_2, [ ((1, 1), (1, 18)) ]);
    ("invalid if", invalid_if, [ ((1, 1), (1, 18)) ]);
    ("invalid if 2", invalid_if_2, [ ((1, 1), (1, 25)) ]);
    ("not closed class", not_closed_class, [ ((1, 1), (1, 17)) ]);
    ("not closed class 2", not_closed_class_2, [ ((1, 1), (1, 8)) ]);
    ("not closed class 3", not_closed_class_3, [ ((1, 1), (1, 10)) ]);
    ("not closed class 4", not_closed_class_4, [ ((1, 1), (1, 6)) ]);
    ("binop", binop, [ ((1, 1), (1, 4)) ]);
    ( "many not closed",
      many_not_closed,
      [ ((8, 0), (14, 4)); ((21, 0), (21, 7)) ] );
  ]

let intf_tests = [ ("empty", "", []) ]

let test_impl =
  let test name input expected =
    let test_name = "impl " ^ name in
    let test_fun () = Utils.check_impl ~name:test_name ~input ~expected in
    (test_name, `Quick, test_fun)
  in
  List.map (fun (name, input, expected) -> test name input expected) impl_tests

let test_intf =
  let test name input expected =
    let test_name = "intf " ^ name in
    let test_fun () = Utils.check_intf ~name:test_name ~input ~expected in
    (test_name, `Quick, test_fun)
  in
  List.map (fun (name, input, expected) -> test name input expected) intf_tests

let test_use_file =
  let test name input expected =
    let test_name = "use_file " ^ name in
    let test_fun () = Utils.check_use_file ~name:test_name ~input ~expected in
    (test_name, `Quick, test_fun)
  in
  List.map
    (fun (name, input, expected) -> test name input expected)
    (impl_tests @ intf_tests)

let tests =
  [ ("impl", test_impl); ("intf", test_intf); ("use_file", test_use_file) ]

let () = Alcotest.run "Parse_wyc" tests

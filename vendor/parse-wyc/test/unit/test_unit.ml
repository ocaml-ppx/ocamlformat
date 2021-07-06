module Structure = struct
  module Inputs = struct
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

    let invalid_after_eq_test = {|let fooooooooooooooo =|}

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

    let invalid_seq_modules_test =
      {|
module M = struct
  let foooooo = foooo

  let foooooooooooooo = (* line 5 *)

  let foooooooooooo = fooooo
end
|}

    let not_closed_module_test =
      {|
module M = struct
  let foo = foo
  let foo =
|}

    let not_closed_module_test_2 = {|
module M = struct
  let foo = foo in
|}

    let not_closed_sig = {|
module K : sig
  type t
|}

    let not_closed_begin = {| let x = if x then begin a |}
    let not_closed_if = {| let x = if k |}
    let not_closed_if_2 = {| let x = if k then |}
    let invalid_if = {| let x = if k then else |}
    let invalid_if_2 = {| let x = if k then x else |}
    let not_closed_class = {| class c = object |}
    let not_closed_class_2 = {| class c |}
    let not_closed_class_3 = {| class c = |}
    let not_closed_class_4 = {| class |}
    let binop = {| x + |}

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

    let _escape_error = {|
try foo () with ;;

(3 : );;

(3 :> );;
|}

    let _expecting =
      {|
let f = function
  | 3 as 3 -> ()
;;

let f = function
  | 3 :: -> ()
;;

let f = function
  | 3 | -> ()
;;

let f = function
  | List.( -> ()
;;

let f = function
  | (3 : 3) -> ()
;;

let f = function
  | (3,) -> ()
;;

let f = function
  | ( -> ()
;;

let f = function
  | (module -> ()
;;
|}

    let _pr7847 = {| external x : unit -> (int,int)`A.t = "x" |}

    let unclosed_class_simpl_expr1 = {|
class c = object
  method x = 1
|}

    let _unclosed_class_simpl_expr2 = {| class c = (object end : object end |}
    let _unclosed_class_simpl_expr3 = {| class c = (object end |}
    let _unclosed_object = {| let o = object |}
    let _unclosed_paren_module_expr1 = {| module M = (struct end : sig end |}
    let _unclosed_paren_module_expr2 = {| module M = (struct end |}
    let _unclosed_paren_module_expr3 = {| module M = (val 3 : |}
    let _unclosed_paren_module_expr4 = {| module M = (val 3 :> |}
    let _unclosed_paren_module_expr5 = {| module M = (val 3 |}

    let _unclosed_simple_expr =
      {|
(3; 2;;

begin 3; 2;;

List.(3; 2;;

simple_expr.(3; 2;;

simple_expr.[3; 2;;

simple_expr.%[3;;

simple_expr.%(3;;

simple_expr.%{3;;

foo.Bar.%[3;;

foo.Bar.%(3;;

foo.Bar.%{3;;

simple_expr.{3, 2;;

{ x = 3; y;;

List.{ x = 3; y ;;

[| 3; 2;;

List.[|3; 2;;

[3; 2;;

List.[3; 2;;

{< x = 3; y; ;;

List.{< x = 3; y ;;

(module struct end :;;

List.(module struct end :;;

(=;
|}

    let _unclosed_simple_pattern =
      {|
let f = function
  | List.(_
;;

let f = function
  | (_
;;

let f = function
  | (_ : int
;;

(* Impossible to get the "unclosed (" message here. This case gets absorbed by
   val_ident... *)

let f = function
  | (module Foo : sig end
;;

(* As with expressions, impossible to get the unclosed message for the following
   cases. *)

let f = function
  | { foo; bar;
;;

let f = function
  | [ 1; 2;
;;

let f = function
  | [| 3; 4;
;;
|}

    let unclosed_struct = {|
module M = struct
  type t = T
|}
  end

  let common_tests =
    let open Inputs in
    [
      ("empty", "", "");
      ( "valid",
        valid_test,
        {|let fooooooooooooooo =
  let fooooooooooooo =
    let foooooooooooooo = foooooooooooooo in (fooooo, fooooooo) in
  fooooooooooooooooo; foooooooooooooo|}
      );
      ( "invalid after eq",
        invalid_after_eq_test,
        "let fooooooooooooooo = [%merlin.hole ]" );
      ( "invalid after in",
        invalid_after_in_test,
        {|let fooooooooooooooooooooo =
  let foooooooooooooooooooo = let foooooooooooooooo = foooooooo in foooooo in
  [%merlin.hole ]|}
      );
      ( "invalid seq modules",
        invalid_seq_modules_test,
        {|module M =
  struct
    let foooooo = foooo
    let foooooooooooooo =
      let foooooooooooo = fooooo <- [%merlin.hole ] in [%merlin.hole ]
  end|}
      );
      ( "not closed module",
        not_closed_module_test,
        {|module M = struct let foo = foo
                  let foo = [%merlin.hole ] end|}
      );
      ( "not closed module 2",
        not_closed_module_test_2,
        "module M = struct ;;let foo = foo in [%merlin.hole ] end" );
      ( "not closed sig",
        not_closed_sig,
        {|module K : sig type t +=  
                 | <invalid-uident>  end = [%merlin.hole ] |}
      );
      ( "not closed begin",
        not_closed_begin,
        "let x = if x then a <- [%merlin.hole ]" );
      ( "not closed if",
        not_closed_if,
        "let x = if k <- [%merlin.hole ] then [%merlin.hole ]" );
      ("not closed if 2", not_closed_if_2, "let x = if k then [%merlin.hole ]");
      ("invalid if", invalid_if, "let x = if k then [%merlin.hole ]");
      ("invalid if 2", invalid_if_2, "let x = if k then x else [%merlin.hole ]");
      ("not closed class", not_closed_class, "class c = object  end");
      ("not closed class 2", not_closed_class_2, "class c = [%merlin.hole ]");
      ("not closed class 3", not_closed_class_3, "class c = [%merlin.hole ]");
      ( "not closed class 4",
        not_closed_class_4,
        "class <invalid-lident> = [%merlin.hole ]" );
      ("binop", binop, ";;x + ([%merlin.hole ])");
      (*
    ( "escape_error",
      escape_error,
      [ ((2, 0), (2, 18)); ((4, 0), (4, 8)); ((6, 0), (6, 9)) ] );
    ( "expecting",
      expecting,
      [
        ((6, 0), (8, 2));
        ((10, 0), (12, 2));
        ((14, 0), (16, 2));
        ((22, 0), (24, 2));
        ((26, 0), (28, 2));
        ((30, 0), (32, 2));
      ] );
    ("pr7847", pr7847, [ ((1, 1), (1, 41)) ]);
*)
      ( "unclosed class simpl expr1",
        unclosed_class_simpl_expr1,
        "class c = object method x = 1 end" );
    (*
    ( "unclosed class simpl expr2",
      unclosed_class_simpl_expr2,
      [ ((1, 1), (1, 35)) ] );
    ( "unclosed class simpl expr3",
      unclosed_class_simpl_expr3,
      [ ((1, 1), (1, 22)) ] );
    ("unclosed object", unclosed_object, [ ((1, 1), (1, 15)) ]);
    ( "unclosed paren module expr1",
      unclosed_paren_module_expr1,
      [ ((1, 1), (1, 33)) ] );
    ( "unclosed paren module expr2",
      unclosed_paren_module_expr2,
      [ ((1, 1), (1, 23)) ] );
    ( "unclosed paren module expr3",
      unclosed_paren_module_expr3,
      [ ((1, 1), (1, 20)) ] );
    ( "unclosed paren module expr4",
      unclosed_paren_module_expr4,
      [ ((1, 1), (1, 21)) ] );
    ( "unclosed paren module expr5",
      unclosed_paren_module_expr5,
      [ ((1, 1), (1, 19)) ] );
    ("unclosed simple expr", unclosed_simple_expr, [ ((2, 0), (46, 3)) ]);
    ("unclosed simple pattern", unclosed_simple_pattern, [ ((2, 0), (34, 2)) ]);
*)
      ( "unclosed struct",
        unclosed_struct,
        {|module M = struct type t =
                    | T  end|} );
    ]

  let specific_tests =
    let open Inputs in
    [
      ( "many not closed",
        many_not_closed,
        {|let foooooo = fooooooooo
let foooooooo = bar baaaaar barrr
module K =
  (struct let k = [%merlin.hole ]
          let x = [%merlin.hole ] end)(struct  end)
let foooooo = fooooo foooooooo
let k = [%merlin.hole ]|}
      );
    ]

  let tests = common_tests @ specific_tests
end

module Signature = struct
  module Inputs = struct
    let _unclosed_class_signature = {| class c : object |}
    let _unclosed_paren_module_type = {| module M : (sig end |}

    let unclosed_sig = {|
module M : sig
  type t = T
 |}
  end

  let tests =
    let open Inputs in
    let open Structure.Inputs in
    [
      ("empty", "", "");
    (*
    ("unclosed class signature", unclosed_class_signature, [ ((1, 1), (1, 17)) ]);
    ( "unclosed paren module type",
      unclosed_paren_module_type,
      [ ((1, 1), (1, 20)) ] );
*)
      ( "unclosed sig",
        unclosed_sig,
        {|module M : sig type t =
                 | T  end|} );
      ( "not closed sig",
        not_closed_sig,
        {|module K : sig type t +=  
                 | <invalid-uident>  end|}
      );
    ]
end

module Use_file = struct
  module Inputs = Structure.Inputs

  let common_tests =
    List.map
      (fun (name, input, output) ->
        match output with
        | "" -> (name, input, output)
        | _ -> (name, input, "\n" ^ output))
      Structure.common_tests

  let specific_tests =
    let open Inputs in
    [
      ( "many not closed",
        many_not_closed,
        {|
let foooooo = fooooooooo

let foooooooo = bar baaaaar barrr

module K =
  (struct let k = [%merlin.hole ]
          let x = [%merlin.hole ] end)(struct  end)

let foooooo = fooooo foooooooo

let k = [%merlin.hole ]|}
      );
    ]

  let tests = common_tests @ specific_tests
end

let check_tests parse pprint tests =
  List.map
    (fun (name, input, expected) ->
      let lexbuf = Lexing.from_string input in
      Format.fprintf Format.str_formatter "%a" pprint (parse lexbuf);
      let actual = Format.flush_str_formatter () in
      (name, `Quick, fun () -> Alcotest.(check string) name expected actual))
    tests

module Pp = struct
  let structure = Pprintast.structure

  let signature = Pprintast.signature

  let toplevel_phrase = Pprintast.toplevel_phrase

  let use_file fs lx =
    Format.pp_print_list
      ~pp_sep:(fun fs () -> Format.fprintf fs "@\n")
      (fun fs x -> Format.fprintf fs "%a" toplevel_phrase x)
      fs lx
end

let tests =
  [
    ("impl", check_tests Parse_wyc.structure Pp.structure Structure.tests);
    ("intf", check_tests Parse_wyc.signature Pp.signature Signature.tests);
    ("use_file", check_tests Parse_wyc.use_file Pp.use_file Use_file.tests);
  ]

let () = Alcotest.run "Parse_wyc" tests

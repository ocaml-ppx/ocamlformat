open Base
open Ocamlformat

let eval_fmt term =
  let buffer = Buffer.create 0 in
  let ppf = Format_.formatter_of_buffer buffer in
  Fmt.eval ppf term ;
  Format_.pp_print_flush ppf () ;
  Buffer.contents buffer

let tests_lazy =
  [ ( "lazy_: not using lazy"
    , `Quick
    , fun () ->
        let r = ref None in
        let pp s =
          r := Some s ;
          Fmt.str s
        in
        let term = Fmt.fmt_if_k false (pp "hello") in
        let expected = "" in
        let expected_r = Some "hello" in
        let got = eval_fmt term in
        let got_r = !r in
        Alcotest.check Alcotest.(string) Caml.__LOC__ expected got ;
        Alcotest.check Alcotest.(option string) Caml.__LOC__ expected_r got_r
    )
  ; ( "lazy_: using lazy"
    , `Quick
    , fun () ->
        let r = ref None in
        let pp s =
          Fmt.lazy_ (fun () ->
              r := Some s ;
              Fmt.str s )
        in
        let term = Fmt.fmt_if_k false (pp "hello") in
        let expected = "" in
        let expected_r = None in
        let got = eval_fmt term in
        let got_r = !r in
        Alcotest.check Alcotest.(string) Caml.__LOC__ expected got ;
        Alcotest.check Alcotest.(option string) Caml.__LOC__ expected_r got_r
    ) ]

let tests_list_pn =
  let test name ~expected ~expected_calls f =
    ( "list_pn: " ^ name
    , `Quick
    , fun () ->
        let calls = ref [] in
        let record_call (prev, x, next) =
          let opt_to_string so = Option.value so ~default:"-" in
          let call_str = opt_to_string prev ^ x ^ opt_to_string next in
          calls := call_str :: !calls
        in
        let pp_spy ~prev x ~next =
          record_call (prev, x, next) ;
          Fmt.str x
        in
        let term = f pp_spy in
        let got = eval_fmt term in
        Alcotest.check Alcotest.(string) Caml.__LOC__ expected got ;
        let got_calls = List.rev !calls in
        Alcotest.check
          Alcotest.(list string)
          Caml.__LOC__ expected_calls got_calls )
  in
  [ test "evaluation order" ~expected:"abcde"
      ~expected_calls:["-ab"; "abc"; "bcd"; "cde"; "de-"] (fun pp_spy ->
        let l = ["a"; "b"; "c"; "d"; "e"] in
        Fmt.list_pn l pp_spy )
  ; test "does not call pp if not formatting" ~expected:"" ~expected_calls:[]
      (fun pp_spy ->
        let l = ["a"; "b"; "c"; "d"; "e"] in
        Fmt.fmt_if_k false (Fmt.list_pn l pp_spy) ) ]

let tests_list_k =
  let test name ~expected ~expected_calls f =
    ( "list_k: " ^ name
    , `Quick
    , fun () ->
        let calls = ref [] in
        let record_call x = calls := x :: !calls in
        let pp_spy x = record_call x ; Fmt.str x in
        let term = f pp_spy in
        let got = eval_fmt term in
        Alcotest.check Alcotest.(string) Caml.__LOC__ expected got ;
        let got_calls = List.rev !calls in
        Alcotest.check
          Alcotest.(list string)
          Caml.__LOC__ expected_calls got_calls )
  in
  [ test "evaluation order" ~expected:"a b c d e"
      ~expected_calls:["a"; "b"; "c"; "d"; "e"] (fun pp_spy ->
        let l = ["a"; "b"; "c"; "d"; "e"] in
        Fmt.list_k l (Fmt.str " ") pp_spy )
  ; test "does not call pp if not formatting" ~expected:"" ~expected_calls:[]
      (fun pp_spy ->
        let l = ["a"; "b"; "c"; "d"; "e"] in
        Fmt.fmt_if_k false (Fmt.list_k l (Fmt.str " ") pp_spy) ) ]

let tests_sequence =
  let test name term ~expected =
    ( "sequence: " ^ name
    , `Quick
    , fun () ->
        let got = eval_fmt term in
        Alcotest.check Alcotest.string Caml.__LOC__ expected got )
  in
  [ test "1 element" (Fmt.sequence [Fmt.char 'c']) ~expected:"c"
  ; test "empty list" (Fmt.sequence []) ~expected:""
  ; test "list"
      (Fmt.sequence [Fmt.str "a"; Fmt.str "b"; Fmt.str "c"; Fmt.str "d"])
      ~expected:"abcd"
  ; test "long list"
      (Fmt.sequence (List.init 300_000 ~f:(fun _ -> Fmt.noop)))
      ~expected:"" ]

let tests = tests_lazy @ tests_list_pn @ tests_list_k @ tests_sequence

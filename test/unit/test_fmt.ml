open Base
open Ocamlformat_lib

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
              Fmt.str s)
        in
        let term = Fmt.fmt_if_k false (pp "hello") in
        let expected = "" in
        let expected_r = None in
        let got = eval_fmt term in
        let got_r = !r in
        Alcotest.check Alcotest.(string) Caml.__LOC__ expected got ;
        Alcotest.check Alcotest.(option string) Caml.__LOC__ expected_r got_r
    ) ]

let tests = tests_lazy

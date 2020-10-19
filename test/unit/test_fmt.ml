open Base
open Ocamlformat_lib

let eval_fmt term =
  let buffer = Buffer.create 0 in
  let ppf = Format_.formatter_of_buffer buffer in
  Fmt.eval ppf term ;
  Format_.pp_print_flush ppf () ;
  Buffer.contents buffer

let join l = List.fold_left l ~init:Fmt.noop ~f:Fmt.( $ )

let map_string s ~f = String.to_list s |> List.map ~f |> join

let tests_lazy =
  let test name term ~expected =
    ( "lazy_: " ^ name
    , `Quick
    , fun () ->
        let got = eval_fmt term in
        Alcotest.check Alcotest.(string) Caml.__LOC__ expected got )
  in
  let is_vowel = function
    | 'a' | 'e' | 'i' | 'o' | 'u' | 'y' -> true
    | _ -> false
  in
  [ test "not using lazy"
      (let i = ref 0 in
       let pp_letter c =
         let open Fmt in
         Int.incr i ;
         str (Int.to_string !i) $ char c
       in
       let pp_if_vowel c = Fmt.fmt_if_k (is_vowel c) (pp_letter c) in
       map_string "hello" ~f:pp_if_vowel)
      ~expected:"2e5o"
  ; test "using lazy"
      (let i = ref 0 in
       let pp_letter c =
         let open Fmt in
         lazy_ (fun () ->
             Int.incr i ;
             str (Int.to_string !i) $ char c)
       in
       let pp_if_vowel c = Fmt.fmt_if_k (is_vowel c) (pp_letter c) in
       map_string "hello" ~f:pp_if_vowel)
      ~expected:"1e2o" ]

let tests = tests_lazy

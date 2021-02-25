open! Import
open! Char

let%test _ = not (is_whitespace '\008')
(* backspace *)
let%test _ = is_whitespace '\009'
(* '\t': horizontal tab *)
let%test _ = is_whitespace '\010'
(* '\n': line feed *)
let%test _ = is_whitespace '\011'
(* '\v': vertical tab *)
let%test _ = is_whitespace '\012'
(* '\f': form feed *)
let%test _ = is_whitespace '\013'
(* '\r': carriage return *)
let%test _ = not (is_whitespace '\014')
(* shift out *)
let%test _ = is_whitespace '\032'

(* space *)

let%expect_test "hash coherence" =
  check_hash_coherence [%here] (module Char) [ min_value; 'a'; max_value ];
  [%expect {| |}]
;;

let%test_module "int to char conversion" =
  (module struct
    let%test_unit "of_int bounds" =
      let bounds_check i =
        [%test_result: t option] (of_int i) ~expect:None ~message:(Int.to_string i)
      in
      for i = 1 to 100 do
        bounds_check (-i);
        bounds_check (255 + i)
      done
    ;;

    let%test_unit "of_int_exn vs of_int" =
      for i = -100 to 300 do
        [%test_eq: t option]
          (of_int i)
          (Option.try_with (fun () -> of_int_exn i))
          ~message:(Int.to_string i)
      done
    ;;

    let%test_unit "unsafe_of_int vs of_int_exn" =
      for i = 0 to 255 do
        [%test_eq: t] (unsafe_of_int i) (of_int_exn i) ~message:(Int.to_string i)
      done
    ;;
  end)
;;

let%expect_test "all" =
  print_s [%sexp (all : t list)];
  [%expect
    {|
    ("\000"
     "\001"
     "\002"
     "\003"
     "\004"
     "\005"
     "\006"
     "\007"
     "\b"
     "\t"
     "\n"
     "\011"
     "\012"
     "\r"
     "\014"
     "\015"
     "\016"
     "\017"
     "\018"
     "\019"
     "\020"
     "\021"
     "\022"
     "\023"
     "\024"
     "\025"
     "\026"
     "\027"
     "\028"
     "\029"
     "\030"
     "\031"
     " "
     !
     "\""
     #
     $
     %
     &
     '
     "("
     ")"
     *
     +
     ,
     -
     .
     /
     0
     1
     2
     3
     4
     5
     6
     7
     8
     9
     :
     ";"
     <
     =
     >
     ?
     @
     A
     B
     C
     D
     E
     F
     G
     H
     I
     J
     K
     L
     M
     N
     O
     P
     Q
     R
     S
     T
     U
     V
     W
     X
     Y
     Z
     [
     \
     ]
     ^
     _
     `
     a
     b
     c
     d
     e
     f
     g
     h
     i
     j
     k
     l
     m
     n
     o
     p
     q
     r
     s
     t
     u
     v
     w
     x
     y
     z
     {
     |
     }
     ~
     "\127"
     "\128"
     "\129"
     "\130"
     "\131"
     "\132"
     "\133"
     "\134"
     "\135"
     "\136"
     "\137"
     "\138"
     "\139"
     "\140"
     "\141"
     "\142"
     "\143"
     "\144"
     "\145"
     "\146"
     "\147"
     "\148"
     "\149"
     "\150"
     "\151"
     "\152"
     "\153"
     "\154"
     "\155"
     "\156"
     "\157"
     "\158"
     "\159"
     "\160"
     "\161"
     "\162"
     "\163"
     "\164"
     "\165"
     "\166"
     "\167"
     "\168"
     "\169"
     "\170"
     "\171"
     "\172"
     "\173"
     "\174"
     "\175"
     "\176"
     "\177"
     "\178"
     "\179"
     "\180"
     "\181"
     "\182"
     "\183"
     "\184"
     "\185"
     "\186"
     "\187"
     "\188"
     "\189"
     "\190"
     "\191"
     "\192"
     "\193"
     "\194"
     "\195"
     "\196"
     "\197"
     "\198"
     "\199"
     "\200"
     "\201"
     "\202"
     "\203"
     "\204"
     "\205"
     "\206"
     "\207"
     "\208"
     "\209"
     "\210"
     "\211"
     "\212"
     "\213"
     "\214"
     "\215"
     "\216"
     "\217"
     "\218"
     "\219"
     "\220"
     "\221"
     "\222"
     "\223"
     "\224"
     "\225"
     "\226"
     "\227"
     "\228"
     "\229"
     "\230"
     "\231"
     "\232"
     "\233"
     "\234"
     "\235"
     "\236"
     "\237"
     "\238"
     "\239"
     "\240"
     "\241"
     "\242"
     "\243"
     "\244"
     "\245"
     "\246"
     "\247"
     "\248"
     "\249"
     "\250"
     "\251"
     "\252"
     "\253"
     "\254"
     "\255") |}]
;;

let%expect_test "predicates" =
  print_s [%sexp (List.filter all ~f:is_digit : t list)];
  [%expect {| (0 1 2 3 4 5 6 7 8 9) |}];
  print_s [%sexp (List.filter all ~f:is_lowercase : t list)];
  [%expect {| (a b c d e f g h i j k l m n o p q r s t u v w x y z) |}];
  print_s [%sexp (List.filter all ~f:is_uppercase : t list)];
  [%expect {| (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z) |}];
  print_s [%sexp (List.filter all ~f:is_alpha : t list)];
  [%expect
    {|
    (A
     B
     C
     D
     E
     F
     G
     H
     I
     J
     K
     L
     M
     N
     O
     P
     Q
     R
     S
     T
     U
     V
     W
     X
     Y
     Z
     a
     b
     c
     d
     e
     f
     g
     h
     i
     j
     k
     l
     m
     n
     o
     p
     q
     r
     s
     t
     u
     v
     w
     x
     y
     z) |}];
  print_s [%sexp (List.filter all ~f:is_alphanum : t list)];
  [%expect
    {|
    (0
     1
     2
     3
     4
     5
     6
     7
     8
     9
     A
     B
     C
     D
     E
     F
     G
     H
     I
     J
     K
     L
     M
     N
     O
     P
     Q
     R
     S
     T
     U
     V
     W
     X
     Y
     Z
     a
     b
     c
     d
     e
     f
     g
     h
     i
     j
     k
     l
     m
     n
     o
     p
     q
     r
     s
     t
     u
     v
     w
     x
     y
     z) |}];
  print_s [%sexp (List.filter all ~f:is_print : t list)];
  [%expect
    {|
    (" "
     !
     "\""
     #
     $
     %
     &
     '
     "("
     ")"
     *
     +
     ,
     -
     .
     /
     0
     1
     2
     3
     4
     5
     6
     7
     8
     9
     :
     ";"
     <
     =
     >
     ?
     @
     A
     B
     C
     D
     E
     F
     G
     H
     I
     J
     K
     L
     M
     N
     O
     P
     Q
     R
     S
     T
     U
     V
     W
     X
     Y
     Z
     [
     \
     ]
     ^
     _
     `
     a
     b
     c
     d
     e
     f
     g
     h
     i
     j
     k
     l
     m
     n
     o
     p
     q
     r
     s
     t
     u
     v
     w
     x
     y
     z
     {
     |
     }
     ~) |}];
  print_s [%sexp (List.filter all ~f:is_whitespace : t list)];
  [%expect {| ("\t" "\n" "\011" "\012" "\r" " ") |}]
;;

let%test_module "Caseless Comparable" =
  (module struct
    (* examples from docs *)
    let%test _ = Caseless.equal 'A' 'a'
    let%test _ = Caseless.('a' < 'B')
    let%test _ = Int.( <> ) (Caseless.compare 'a' 'B') (compare 'a' 'B')
    let%test _ = List.is_sorted ~compare:Caseless.compare [ 'A'; 'b'; 'C' ]

    let%expect_test _ =
      let x = Sys.opaque_identity 'a' in
      let y = Sys.opaque_identity 'b' in
      require_no_allocation [%here] (fun () ->
        ignore (Sys.opaque_identity (Caseless.equal x y) : bool));
      [%expect {||}]
    ;;
  end)
;;

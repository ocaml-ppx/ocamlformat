(* This is a separate test from [attributes.ml] because we want to be able to see the
   output on the [janestreet] profile. *)

let _ =
  very_long_function_name_that_causes_the_line_to_wrap_at_some_point [@alert
                                                                       "-turn-it-off"]
;;

let () =
  (f (fun () -> aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa) [@aaaaaa
                                                                                          "This \
                                                                                          formatting \
                                                                                          is \
                                                                                          a \
                                                                                          bit \
                                                                                          strange"])
;;

let f = function
  | A (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
                                                               | (B | C _) [@alert "-foo"]
    -> 0
  | D -> 0
;;

(* Meanwhile, these are fine *)
let f = function
  | A (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) | ((C _) [@alert "-foo"])
    -> 0
  | B | D -> 0
;;

let f = function
  | (B | C _) [@alert "-foo"] | A (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    -> 0
  | D -> 0
;;

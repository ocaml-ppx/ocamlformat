let _ = List.map ~f:(( + ) (M.f x))

let id x = x

let plus a ?(b = 0) c = a + b + c

;;
id (plus 1) ~b:1

(* The version above does not type-check, while the version below does
   type-check, and should not be formatted to the above. See
   https://caml.inria.fr/mantis/view.php?id=7832 for explanation on the
   type-checking (and dynamic semantics) distinction. *)

;;
(id (plus 1)) ~b:1

let ( !!! ) a ~b = a + b

let _ = ( !!! ) a b

let _ = ( !!! ) ~b

let _ = !!!!a b d

let _ = ( + ) a b c d

let cartesian_product l1 l2 =
  List.concat
    (l1 |> List.map (fun v1 -> l2 |> List.map (fun v2 -> (v1, v2))))

let cartesian_product' long_list_one long_list_two =
  List.concat
    ( long_list_one
    |> List.map (fun v1 -> long_list_two |> List.map (fun v2 -> (v1, v2)))
    )

let whatever a_function_name long_list_one some_other_thing =
  List.map
    (fun long_list_one_elt ->
      do_something_with_a_function_and_some_things a_function_name
        long_list_one_elt some_other_thing)
    long_list_one

let whatever_labelled a_function_name long_list_one some_other_thing =
  ListLabels.map long_list_one ~f:(fun long_list_one_elt ->
      do_something_with_a_function_and_some_things a_function_name
        long_list_one_elt some_other_thing)

[@@@ocamlformat "indicate-multiline-delimiters=closing-on-separate-line"]

let cartesian_product' long_list_one long_list_two =
  List.concat
    (long_list_one
    |> List.map (fun v1 -> long_list_two |> List.map (fun v2 -> (v1, v2)))
    )

let whatever a_function_name long_list_one some_other_thing =
  List.map
    (fun long_list_one_elt ->
      do_something_with_a_function_and_some_things a_function_name
        long_list_one_elt some_other_thing)
    long_list_one

let whatever_labelled a_function_name long_list_one some_other_thing =
  ListLabels.map long_list_one ~f:(fun long_list_one_elt ->
      do_something_with_a_function_and_some_things a_function_name
        long_list_one_elt some_other_thing)

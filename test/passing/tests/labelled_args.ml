let _ =
  let f ~y = y + 1 in
  f ~y:(y : int)

let () =
  very_long_function_name
    ~very_long_argument_label:(fun
        very_long_argument_name_one
        very_long_argument_name_two
        very_long_argument_name_three
      -> () )

let () =
  very_long_function_name
    ~very_long_argument_label:(* foo *)
      (fun
        very_long_argument_name_one
        very_long_argument_name_two
        very_long_argument_name_three
      -> () )

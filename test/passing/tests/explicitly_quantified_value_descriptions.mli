(* for comparison, a description with no explicit quantified types *)
val f :
     long_argument_1234567890
  -> long_argument_1234567890
  -> long_argument_1234567890
  -> long_argument_1234567890
  -> long_result_1234567890

(* most common case of a few short types with no layout annotations *)
val f :
  'a 'b 'c 'd.
     long_argument_1234567890
  -> long_argument_1234567890
  -> long_argument_1234567890
  -> long_argument_1234567890
  -> long_result_1234567890

(* common case of a few short types with at least one layout annotation *)
val f :
  ('a : layout) ('b : layout) ('c : layout) ('d : layout).
     long_argument_1234567890
  -> long_argument_1234567890
  -> long_argument_1234567890
  -> long_argument_1234567890
  -> long_result_1234567890

(* uncommon case where quantified types have to line wrap *)
val f :
  'long_quantified_variable_a 'long_quantified_variable_b
  'long_quantified_variable_c 'long_quantified_variable_d.
     long_argument_1234567890
  -> long_argument_1234567890
  -> long_argument_1234567890
  -> long_argument_1234567890
  -> long_result_1234567890

(* uncommon case where quantified types with layouts have to line wrap; note
   that the type and layout may be split onto multiple lines in the middle of
   wrapping *)
val f :
  ('long_quantified_variable_a : layout)
  ('long_quantified_variable_b : layout)
  ('long_quantified_variable_c : layout)
  ('long_quantified_variable_d : layout).
     long_argument_1234567890
  -> long_argument_1234567890
  -> long_argument_1234567890
  -> long_argument_1234567890
  -> long_result_1234567890

(* same as above, but for janestreet profile *)
val f :
  ('even_longer_quantified_variable_a : layout)
  ('even_longer_quantified_variable_b : layout)
  ('even_longer_quantified_variable_c : layout)
  ('even_longer_quantified_variable_d : layout).
  short_argument -> short_result

(* wrapping behavior when the main type doesn't need to wrap *)
val f :
  'long_quantified_variable_a 'long_quantified_variable_b
  'long_quantified_variable_c 'long_quantified_variable_d.
  short_argument -> short_result

(* wrapping behavior of layouts when the main type doesn't need to wrap *)
val f :
  ('long_quantified_variable_a : layout)
  ('long_quantified_variable_b : layout)
  ('long_quantified_variable_c : layout)
  ('long_quantified_variable_d : layout). short_argument -> short_result

(* behavior is the same between [val] and [external] descriptions *)
external f :
  'a 'b 'c 'd.
     long_argument_1234567890
  -> long_argument_1234567890
  -> long_argument_1234567890
  -> long_argument_1234567890
  -> long_result_1234567890 = ""

val state_float :
  ('a : float64) 'e 'f.
     name:string
  -> on_event:(local_ 'e -> 'a -> 'a)
  -> ('e Event.t -> ('a, 'f) t) Unregistered.t

(*********************************************************************
 * Lists *)

(* Pythagorean triples with components from 1 to 10, no duplicate triples *)
let pyth =
  [ (a, b, c)
    for a = 1 to 10
    for b = a to 10
    for c = b to 10
    when (a * a) + (b * b) = c * c ]

(* Let's describe some objects *)
let descriptions =
  [ Printf.sprintf "a %s %s" adjective noun
    for noun in ["light"; "pepper"]
    and adjective in ["red"; "yellow"; "green"] ]

(* Compute a list of reciprocals in increasing order *)
let reciprocals = [1. /. Float.of_int x for x = 5 downto 0 when x <> 0]

(* Flatten a nested array *)
let flattened =
  let sentences =
    [ ["hello"; "world"]
    ; ["how"; "are"; "you"; "doing"]
    ; ["please"; "enjoy"; "these"; "comprehensions"] ]
  in
  [word for sentence in sentences for word in sentence]

(* We could use comprehensions to reimplement map... *)
let map' ~f l = [f x for x in l]

(* ...and filter *)
let filter' ~f l = [x for x in l when f x]

(* We can put comprehensions inside other comprehensions... *)
let nested_comprehensions = [[i for i = 1 to n] for n = 1 to 10]

(* ...in multiple_places *)
let nested_comprehensions_rhs =
  [k for n in [i * i for i = 1 to 10] for k = 1 to n]

(* Sometimes comprehensions need to line-wrap *)
let wrapping_inside_individual_comprehension_pieces =
  [ this is a very long function_application so that we can see how the body
      of_a comprehension line wraps
    for
      thoroughness =
        we also want to_know how line wrapping looks inside the right hand
          side of_a clause downto its length
    and
      similarly = we want
        to know how line wrapping looks for_things_in every single part_of a
             clause
    for
      example in
        the sequence iteration case we also want_to test the line wrapping
          behavior
    when
      we have a conditional we also want_to test the line wrapping behavior
        for_it
    for
      some_patterns_can_get_so_long_that_after_them_they_force_wrapping = 0
        to 1
    and
      those_patterns_themselves_are_long_enough_that_they_can't_break in
        -ternally
    and
      can_even_grow_long_enough_to_force_wrapping_to_occur_right_before_the
        in keyword
    for
      ( Other (patterns, they, can, get, really, terribly)
      , Excessively (long, so, that, they, need, line, breaking) ) in
        their own right ]

(*********************************************************************
 * Arrays *)

(* Pythagorean triples with components from 1 to 10, no duplicate triples *)
let pyth =
  [| (a, b, c)
     for a = 1 to 10
     for b = a to 10
     for c = b to 10
     when (a * a) + (b * b) = c * c |]

(* Let's describe some objects *)
let descriptions =
  [| Printf.sprintf "a %s %s" adjective noun
     for noun in [|"light"; "pepper"|]
     and adjective in [|"red"; "yellow"; "green"|] |]

(* Compute a list of reciprocals in increasing order *)
let reciprocals = [|1. /. Float.of_int x for x = 5 downto 0 when x <> 0|]

(* Flatten a nested array *)
let flattened =
  let sentences =
    [| [|"hello"; "world"|]
     ; [|"how"; "are"; "you"; "doing"|]
     ; [|"please"; "enjoy"; "these"; "comprehensions"|] |]
  in
  [|word for sentence in sentences for word in sentence|]

(* We could use comprehensions to reimplement map... *)
let map' ~f l = [|f x for x in l|]

(* ...and filter *)
let filter' ~f l = [|x for x in l when f x|]

(* We can put comprehensions inside other comprehensions... *)
let nested_comprehensions = [|[|i for i = 1 to n|] for n = 1 to 10|]

(* ...in multiple_places *)
let nested_comprehensions_rhs =
  [|k for n in [|i * i for i = 1 to 10|] for k = 1 to n|]

(* Sometimes comprehensions need to line-wrap *)
let wrapping_inside_individual_comprehension_pieces =
  [| this is a very long function_application so that we can see how the body
       of_a comprehension line wraps
     for
       thoroughness =
         we also want to_know how line wrapping looks inside the right hand
           side of_a clause downto its length
     and
       similarly = we want
         to know how line wrapping looks for_things_in every single part_of a
              clause
     for
       example in
         the sequence iteration case we also want_to test the line wrapping
           behavior
     when
       we have a conditional we also want_to test the line wrapping behavior
         for_it
     for
       some_patterns_can_get_so_long_that_after_them_they_force_wrapping = 0
         to 1
     and
       those_patterns_themselves_are_long_enough_that_they_can't_break in
         -ternally
     and
       can_even_grow_long_enough_to_force_wrapping_to_occur_right_before_the
         in keyword
     for
       ( Other (patterns, they, can, get, really, terribly)
       , Excessively (long, so, that, they, need, line, breaking) ) in
         their own right |]

(*********************************************************************
 * Immutable arrays *)

(* Pythagorean triples with components from 1 to 10, no duplicate triples *)
let pyth =
  [: (a, b, c)
     for a = 1 to 10
     for b = a to 10
     for c = b to 10
     when (a * a) + (b * b) = c * c :]

(* Let's describe some objects *)
let descriptions =
  [: Printf.sprintf "a %s %s" adjective noun
     for noun in [:"light"; "pepper":]
     and adjective in [:"red"; "yellow"; "green":] :]

(* Compute a list of reciprocals in increasing order *)
let reciprocals = [:1. /. Float.of_int x for x = 5 downto 0 when x <> 0:]

(* Flatten a nested array *)
let flattened =
  let sentences =
    [: [:"hello"; "world":]
     ; [:"how"; "are"; "you"; "doing":]
     ; [:"please"; "enjoy"; "these"; "comprehensions":] :]
  in
  [:word for sentence in sentences for word in sentence:]

(* We could use comprehensions to reimplement map... *)
let map' ~f l = [:f x for x in l:]

(* ...and filter *)
let filter' ~f l = [:x for x in l when f x:]

(* We can put comprehensions inside other comprehensions... *)
let nested_comprehensions = [:[:i for i = 1 to n:] for n = 1 to 10:]

(* ...in multiple_places *)
let nested_comprehensions_rhs =
  [:k for n in [:i * i for i = 1 to 10:] for k = 1 to n:]

(* Sometimes comprehensions need to line-wrap *)
let wrapping_inside_individual_comprehension_pieces =
  [: this is a very long function_application so that we can see how the body
       of_a comprehension line wraps
     for
       thoroughness =
         we also want to_know how line wrapping looks inside the right hand
           side of_a clause downto its length
     and
       similarly = we want
         to know how line wrapping looks for_things_in every single part_of a
              clause
     for
       example in
         the sequence iteration case we also want_to test the line wrapping
           behavior
     when
       we have a conditional we also want_to test the line wrapping behavior
         for_it
     for
       some_patterns_can_get_so_long_that_after_them_they_force_wrapping = 0
         to 1
     and
       those_patterns_themselves_are_long_enough_that_they_can't_break in
         -ternally
     and
       can_even_grow_long_enough_to_force_wrapping_to_occur_right_before_the
         in keyword
     for
       ( Other (patterns, they, can, get, really, terribly)
       , Excessively (long, so, that, they, need, line, breaking) ) in
         their own right :]

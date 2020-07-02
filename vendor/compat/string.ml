open Base

let starts_with_whitespace s =
  (not (String.is_empty s)) && Char.is_whitespace s.[0]

let ends_with_whitespace s =
  let len = String.length s in
  len > 1 && Char.is_whitespace s.[len - 1]

include String

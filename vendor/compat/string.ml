open Base
include String

let starts_with_whitespace s =
  (not (String.is_empty s)) && Char.is_whitespace s.[0]

let ends_with_whitespace s =
  (not (String.is_empty s)) && Char.is_whitespace s.[String.length s - 1]

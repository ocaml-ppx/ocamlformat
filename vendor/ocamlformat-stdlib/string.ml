include Base
include String

let starts_with_whitespace s = (not (is_empty s)) && Char.is_whitespace s.[0]

let ends_with_whitespace s =
  (not (is_empty s)) && Char.is_whitespace s.[length s - 1]

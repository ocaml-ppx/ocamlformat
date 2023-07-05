include Base
include String

let starts_with_whitespace s = (not (is_empty s)) && Char.is_whitespace s.[0]

let ends_with_whitespace s =
  (not (is_empty s)) && Char.is_whitespace s.[length s - 1]

let rec _indent_of_line s i end_ =
    if Int.equal i end_ then None
    else
      match s.[i] with
      | ' ' | '\t' -> _indent_of_line s (i + 1) end_
      | '\n' | '\r' -> None
      | _ -> Some i

let indent_of_line s = _indent_of_line s 0 (String.length s)

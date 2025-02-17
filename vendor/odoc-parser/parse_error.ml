let capitalize_ascii = Astring.String.Ascii.capitalize

let bad_markup : ?suggestion:string -> string -> Loc.span -> Warning.t =
 fun ?suggestion -> Warning.make ?suggestion "'%s': bad markup."

let markup_should_not_be_used : what:string -> Loc.span -> Warning.t =
 fun ~what ->
  Warning.make "%s should not be used because it has no effect."
    (capitalize_ascii what)

let not_allowed :
    ?suggestion:string -> what:string -> in_what:string -> Loc.span -> Warning.t
    =
 fun ?suggestion ~what ~in_what ->
  Warning.make ?suggestion "%s is not allowed in %s." (capitalize_ascii what)
    in_what

let stray_at : Loc.span -> Warning.t = Warning.make "Stray '@'."

let stray_cr : Loc.span -> Warning.t =
  Warning.make "Stray '\\r' (carriage return character)."

let unpaired_right_brace : Loc.span -> Warning.t =
  Warning.make ~suggestion:"try '\\}'." "Unpaired '}' (end of markup)."

let unpaired_right_bracket : Loc.span -> Warning.t =
  Warning.make ~suggestion:"try '\\]'." "Unpaired ']' (end of code)."

let no_language_tag_in_meta : Loc.span -> Warning.t =
  Warning.make ~suggestion:"try '{[ ... ]}' or '{@ocaml[ ... ]}'."
    "'{@' should be followed by a language tag."

let language_tag_invalid_char lang_tag : char -> Loc.span -> Warning.t =
  let suggestion = "try '{@" ^ lang_tag ^ "[ ... ]}'." in
  Warning.make ~suggestion "Invalid character '%c' in language tag."

let truncated_code_block_meta : Loc.span -> Warning.t =
  Warning.make ~suggestion:"try '{@ocaml[ ... ]}'." "Missing end of code block."

let truncated_code_block : Loc.span -> Warning.t =
  Warning.make ~suggestion:"add ']}'." "Missing end of code block."

let truncated_verbatim_block : Loc.span -> Warning.t =
  Warning.make ~suggestion:"add 'v}'." "Missing end of verbatim block."

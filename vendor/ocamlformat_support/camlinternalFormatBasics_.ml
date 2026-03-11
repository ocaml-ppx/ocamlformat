include CamlinternalFormatBasics

type neutral_concat =
  { f:
      'a 'b 'c 'd 'e 'f. [`String of string | `Char of char ] ->
      ('a,'b,'c,'d,'e,'f) fmt -> ('a,'b,'c,'d,'e,'f) fmt
  }

let rec string_concat_map: type a b c d e f.
   neutral_concat -> (a,b,c,d,e,f) fmt -> (a,b,c,d,e,f) fmt =
  fun f -> function
  | String (pad, rest) -> String (pad, string_concat_map f rest)
  | Caml_string (pad, rest) -> Caml_string (pad, string_concat_map f rest)

  | Int (iconv, pad, prec, rest) ->
    Int (iconv, pad, prec,  string_concat_map f rest)
  | Int32 (iconv, pad, prec, rest) ->
    Int32 (iconv, pad, prec,  string_concat_map f rest)
  | Nativeint (iconv, pad, prec, rest) ->
    Nativeint (iconv, pad, prec,  string_concat_map f rest)
  | Int64 (iconv, pad, prec, rest) ->
    Int64 (iconv, pad, prec,  string_concat_map f rest)
  | Float (fconv, pad, prec, rest) ->
    Float (fconv, pad, prec,  string_concat_map f rest)

  | Char (rest) -> Char (string_concat_map f rest)
  | Caml_char rest ->
    Caml_char ( string_concat_map f rest)
  | Bool (pad, rest) -> Bool (pad,  string_concat_map f rest)
  | Alpha rest -> Alpha (string_concat_map f rest)
  | Theta rest -> Theta (string_concat_map f rest)
  | Custom (arity, fc, rest) ->
    Custom (arity, fc,  string_concat_map f rest)
  | Reader rest -> Reader (string_concat_map f rest)
  | Flush rest -> Flush (string_concat_map f rest)

  | String_literal (str, rest) -> f.f (`String str) (string_concat_map f rest)
  | Char_literal (chr, rest) ->
      f.f (`Char chr) (string_concat_map f rest)
  | Format_arg (pad, fmtty, rest) ->
    Format_arg   (pad, fmtty, string_concat_map f rest)
  | Format_subst (pad, fmtty, rest) ->
    Format_subst (pad, fmtty, string_concat_map f rest)

  | Scan_char_set (width_opt, char_set, rest) ->
    Scan_char_set (width_opt, char_set, string_concat_map f rest)
  | Scan_get_counter (counter, rest) ->
    Scan_get_counter (counter,string_concat_map f rest)
  | Scan_next_char (rest) ->
    Scan_next_char (string_concat_map f rest)
  | Ignored_param (ign, rest) ->
    Ignored_param (ign, string_concat_map f rest)

  | Formatting_lit (fmting_lit, rest) ->
    Formatting_lit (fmting_lit, string_concat_map f rest)
  | Formatting_gen (fmting_gen, rest) ->
    Formatting_gen (fmting_gen, string_concat_map f rest)

  | End_of_format -> End_of_format

val normalize_eol :
     ?exclude_locs:(int * int) list
  -> line_endings:[`Crlf | `Lf]
  -> string
  -> string

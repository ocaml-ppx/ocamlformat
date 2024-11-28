let tyvar_of_name s =
  if String.length s >= 2 && s.[1] = '\'' then
    (* without the space, this would be parsed as
       a character literal *)
    "' " ^ s
  else if Lexer.is_keyword s then
    "'\\#" ^ s
  else if String.equal s "_" then
    s
  else
    "'" ^ s

module Doc = struct
  let tyvar ppf s =
    Format_doc.fprintf ppf "%s" (tyvar_of_name s)
end

let tyvar ppf v = Format_doc.compat Doc.tyvar ppf v

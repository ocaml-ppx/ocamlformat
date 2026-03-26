open Mll_ast

let pp_located fmt x =
  Format.fprintf fmt "%s%a" x.value Location.print_loc x.loc

let pp_code fmt (c : ocaml_code) =
  Format.fprintf fmt "%S%a" c.value Location.print_loc c.loc

let rec pp_regexp fmt = function
  | Reof -> Format.fprintf fmt "eof"
  | Runderscore -> Format.fprintf fmt "_"
  | Rchar s -> Format.fprintf fmt "%s" s
  | Rstring s -> Format.fprintf fmt "%s" s
  | Rident s -> Format.fprintf fmt "%s" s
  | Rsequence rs ->
      Format.fprintf fmt "@[(%a)@]"
        (Format.pp_print_list ~pp_sep:(fun fmt () ->
             Format.fprintf fmt "@ ") pp_regexp) rs
  | Ralternative rs ->
      Format.fprintf fmt "@[(%a)@]"
        (Format.pp_print_list ~pp_sep:(fun fmt () ->
             Format.fprintf fmt "@ | ") pp_regexp) rs
  | Rrepeat r -> Format.fprintf fmt "%a*" pp_regexp r
  | Rplus r -> Format.fprintf fmt "%a+" pp_regexp r
  | Roption r -> Format.fprintf fmt "%a?" pp_regexp r
  | Rparen r -> Format.fprintf fmt "(%a)" pp_regexp r
  | Rcharset entries ->
      Format.fprintf fmt "[%a]"
        (Format.pp_print_list pp_char_entry) entries
  | Rcharset_neg entries ->
      Format.fprintf fmt "[^%a]"
        (Format.pp_print_list pp_char_entry) entries
  | Ras (r, id) -> Format.fprintf fmt "%a as %s" pp_regexp r id
  | Rhash (r1, r2) ->
      Format.fprintf fmt "%a # %a" pp_regexp r1 pp_regexp r2

and pp_char_entry fmt = function
  | Cchar s -> Format.fprintf fmt "%s" s
  | Crange (s1, s2) -> Format.fprintf fmt "%s-%s" s1 s2

let pp_case fmt c =
  Format.fprintf fmt "  | @[%a@ %a@]"
    pp_regexp c.pattern pp_code c.action

let pp_entry fmt e =
  Format.fprintf fmt "@[%a%a = %s@]@.%a"
    pp_located e.entry_name
    (Format.pp_print_list ~pp_sep:Format.pp_print_space
       (fun fmt a -> pp_located fmt a)) e.entry_args
    (if e.entry_is_shortest then "shortest" else "parse")
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_case)
    e.entry_cases

let pp fmt d =
  Format.fprintf fmt "@[<v>" ;
  Option.iter (fun h -> Format.fprintf fmt "header: %a@." pp_code h) d.header ;
  List.iter (fun nd ->
      Format.fprintf fmt "let %a = %a@."
        pp_located nd.def_name pp_regexp nd.def_body
  ) d.named_defs ;
  List.iteri (fun i e ->
      Format.fprintf fmt "%s %a@."
        (if i = 0 then "rule" else "and") pp_entry e
  ) d.rules ;
  Option.iter (fun t -> Format.fprintf fmt "trailer: %a@." pp_code t) d.trailer ;
  Format.fprintf fmt "@]"

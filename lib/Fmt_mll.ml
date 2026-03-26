open Fmt
open Ocamlformat_mll_parser.Mll_ast

type fmt_code =
     Conf.t
  -> offset:int
  -> set_margin:bool
  -> string
  -> (Fmt.t, [`Msg of string]) Result.t

type c =
  { fmt_code: fmt_code
  ; fmt_code_structure: fmt_code
  ; conf: Conf.t
  ; cmts: Cmts.t }

(* Strip delimiters from ocaml code block: { code } -> code *)
let strip_braces s =
  let len = String.length s in
  if len >= 2 && Char.equal s.[0] '{' && Char.equal s.[len - 1] '}' then
    String.sub s ~pos:1 ~len:(len - 2)
  else s

let fmt_ocaml_code c ~offset code =
  let raw = strip_braces code in
  let trimmed = String.strip raw in
  if String.is_empty trimmed then str "{ }"
  else
    match
      c.fmt_code c.conf ~offset:(offset + 2) ~set_margin:false trimmed
    with
    | Ok formatted -> hvbox 2 (str "{ " $ formatted $ str " }")
    | Error _ -> str code

let fmt_ocaml_block c code =
  let raw = strip_braces code in
  let trimmed = String.strip raw in
  if String.is_empty trimmed then str "{" $ force_newline $ str "}"
  else
    match
      c.fmt_code_structure c.conf ~offset:0 ~set_margin:false trimmed
    with
    | Ok formatted ->
        vbox 0 (str "{" $ force_newline $ formatted $ force_newline $ str "}")
    | Error _ -> str code

let fmt_char_entry = function
  | Cchar c -> str c
  | Crange (c1, c2) -> str c1 $ str "-" $ str c2

let rec fmt_regexp = function
  | Reof -> str "eof"
  | Runderscore -> str "_"
  | Rchar c -> str c
  | Rstring s -> str s
  | Rident id -> str id
  | Rsequence rs -> list rs (str " ") fmt_regexp
  | Ralternative rs -> list rs (str " | ") fmt_regexp
  | Rrepeat r -> fmt_regexp r $ str "*"
  | Rplus r -> fmt_regexp r $ str "+"
  | Roption r -> fmt_regexp r $ str "?"
  | Rparen r -> str "(" $ fmt_regexp r $ str ")"
  | Rcharset entries ->
      str "[" $ list entries (str " ") fmt_char_entry $ str "]"
  | Rcharset_neg entries ->
      str "[^" $ list entries (str " ") fmt_char_entry $ str "]"
  | Ras (r, id) -> fmt_regexp r $ str " as " $ str id
  | Rhash (r1, r2) -> fmt_regexp r1 $ str " # " $ fmt_regexp r2

let fmt_named_def _c (d : named_def) =
  hovbox 2
    ( str "let " $ str d.def_name.value $ str " =" $ break 1 2
    $ fmt_regexp d.def_body )

let fmt_before c loc = Cmts.fmt_before c.cmts c.conf ~fmt_code:c.fmt_code loc

let fmt_after c loc = Cmts.fmt_after c.cmts c.conf ~fmt_code:c.fmt_code loc

let fmt_case c (case : rule_case) =
  hovbox 2
    ( str "| " $ fmt_regexp case.pattern $ break 1 4
    $ lazy_ (fun () -> fmt_before c case.action.loc)
    $ fmt_ocaml_code c ~offset:4 case.action.value )

let fmt_rule_entry c (entry : rule_entry) =
  let args =
    match entry.entry_args with
    | [] -> noop
    | args ->
        str " "
        $ list args (str " ") (fun arg ->
            fmt_before c arg.loc $ str arg.value $ fmt_after c arg.loc )
  in
  let kind = if entry.entry_is_shortest then "shortest" else "parse" in
  vbox 2
    ( fmt_before c entry.entry_name.loc
    $ str entry.entry_name.value
    $ fmt_after c entry.entry_name.loc
    $ args $ str " = " $ str kind $ force_newline
    $
    let prev_action_loc = ref None in
    list_fl entry.entry_cases (fun ~first:_ ~last case ->
        lazy_ (fun () ->
            let drain_prev =
              match !prev_action_loc with
              | Some loc -> fmt_after c loc
              | None -> noop
            in
            prev_action_loc := Some case.action.loc ;
            drain_prev $ fmt_before c case.action.loc )
        $ fmt_case c case
        $ fmt_if last (lazy_ (fun () -> fmt_after c case.action.loc))
        $ fmt_if (not last) force_newline ) )

let collect_ocaml_codes (def : lexer_def) =
  let codes = ref [] in
  Option.iter def.header ~f:(fun h -> codes := (h, true) :: !codes) ;
  List.iter def.rules ~f:(fun entry ->
      List.iter entry.entry_cases ~f:(fun case ->
          codes := (case.action, false) :: !codes ) ) ;
  Option.iter def.trailer ~f:(fun t -> codes := (t, true) :: !codes) ;
  List.rev !codes

let fmt_lexer_def conf ~cmts ~fmt_code ~fmt_code_structure (def : lexer_def)
    =
  let c = {fmt_code; fmt_code_structure; conf; cmts} in
  vbox 0
    ( (* Header *)
        ( match def.header with
        | Some h ->
            fmt_before c h.loc $ fmt_ocaml_block c h.value
            $ fmt_after c h.loc $ force_newline
        | None -> noop )
    $ force_newline
    $
    (* Named definitions *)
    ( match def.named_defs with
      | [] -> noop
      | defs ->
          list defs force_newline (fun (d : named_def) ->
              lazy_ (fun () ->
                  fmt_before c d.def_loc $ fmt_before c d.def_name.loc )
              $ fmt_named_def c d
              $ fmt_after c d.def_name.loc
              $ fmt_after c d.def_loc )
          $ force_newline $ force_newline )
    $
    (* Rules *)
    ( match def.rules with
      | [] -> noop
      | first :: rest ->
          str "rule " $ fmt_rule_entry c first
          $ list rest noop (fun entry ->
              force_newline $ force_newline $ str "and "
              $ fmt_rule_entry c entry ) )
    $
    (* Trailer *)
    opt def.trailer (fun t ->
        force_newline $ force_newline $ fmt_before c t.loc
        $ fmt_ocaml_block c t.value $ fmt_after c t.loc ) )

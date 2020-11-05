open Ocamlformat_lib

let rec lex_source acc lexbuf =
  let tok = Lexer.token_with_comments lexbuf in
  let tok = Token_latest.of_compiler_libs tok in
  let acc = (tok, lexbuf.lex_start_p.pos_cnum) :: acc in
  if tok = Migrate_ast.Parser.EOF then acc else lex_source acc lexbuf

(** Return every tokens and the offset at which they start. *)
let lex_source source =
  let lexbuf = Lexing.from_string source in
  Lexer.init () ;
  Lexer.skip_hash_bang lexbuf ;
  List.rev (lex_source [] lexbuf)

let insert_at_every_offsets insert toffs source ~f =
  let ilen = String.length insert and slen = String.length source in
  let buff = Bytes.create (ilen + slen) in
  (* Keep room on the left for [insert]. *)
  Bytes.blit_string source 0 buff ilen slen ;
  let rec loop head_i = function
    | hd :: tl ->
        (* Move previous token from after [insert] to before (overriding it). *)
        Bytes.blit buff (head_i + ilen) buff head_i (hd - head_i) ;
        (* Blit again [insert]. *)
        Bytes.blit_string insert 0 buff hd (String.length insert) ;
        f hd (Bytes.unsafe_to_string buff) ;
        loop hd tl
    | [] -> ()
  in
  loop 0 toffs

let unlex_quoted_string ?op str del =
  let op_sep = match (op, del) with Some _, Some _ -> " " | _ -> ""
  and op = Option.value ~default:"" op
  and del = Option.value ~default:"" del in
  Format.sprintf "{%s%s%s|%s|%s}" op op_sep del str del

let unlex_token = function
  | Migrate_ast.Parser.AMPERAMPER -> "&&"
  | AMPERSAND -> "&"
  | AND -> "and"
  | AS -> "as"
  | ASSERT -> "assert"
  | BACKQUOTE -> "`"
  | BANG -> "!"
  | BAR -> "|"
  | BARBAR -> "||"
  | BARRBRACKET -> "|]"
  | BEGIN -> "begin"
  | CHAR c -> Format.sprintf "'%c'" c
  | CLASS -> "class"
  | COLON -> ":"
  | COLONCOLON -> "::"
  | COLONEQUAL -> ":="
  | COLONGREATER -> ":>"
  | COMMA -> ","
  | COMMENT (cmt, _) -> Format.sprintf "(*%s*)" cmt
  | CONSTRAINT -> "constraint"
  | DOCSTRING d -> Format.sprintf "(**%s*)" (Docstrings.docstring_body d)
  | DO -> "do"
  | DONE -> "done"
  | DOT -> "."
  | DOTDOT -> ".."
  | DOTOP op -> "." ^ op
  | DOWNTO -> "downto"
  | ELSE -> "else"
  | END -> "end"
  | EOF -> ""
  | EOL -> "\n"
  | EQUAL -> "="
  | EXCEPTION -> "exception"
  | EXTERNAL -> "external"
  | FALSE -> "false"
  | FLOAT (r, None) | INT (r, None) -> r
  | FLOAT (r, Some m) | INT (r, Some m) -> r ^ String.make 1 m
  | FOR -> "for"
  | FUNCTION -> "function"
  | FUNCTOR -> "functor"
  | FUN -> "fun"
  | GREATER -> ">"
  | GREATERRBRACE -> ">}"
  | GREATERRBRACKET -> ">]"
  | HASH -> "#"
  | IF -> "if"
  | INCLUDE -> "include"
  | INHERIT -> "inherit"
  | IN -> "in"
  | INITIALIZER -> "initializer"
  | LABEL n -> Format.sprintf "~%s:" n
  | LAZY -> "lazy"
  | LBRACE -> "{"
  | LBRACELESS -> "{<"
  | LBRACKET -> "["
  | LBRACKETAT -> "[@"
  | LBRACKETATAT -> "[@@"
  | LBRACKETATATAT -> "[@@@"
  | LBRACKETBAR -> "[|"
  | LBRACKETGREATER -> "[>"
  | LBRACKETLESS -> "[<"
  | LBRACKETPERCENT -> "[%"
  | LBRACKETPERCENTPERCENT -> "[%%"
  | LESS -> "<"
  | LESSMINUS -> "<-"
  | LET -> "let"
  | LETOP op
   |ANDOP op
   |PREFIXOP op
   |INFIXOP4 op
   |INFIXOP3 op
   |INFIXOP2 op
   |INFIXOP1 op
   |INFIXOP0 op
   |HASHOP op ->
      op
  | LIDENT _1 | UIDENT _1 -> _1
  | LPAREN -> "("
  | MATCH -> "match"
  | METHOD -> "method"
  | MINUS -> "-"
  | MINUSDOT -> "-."
  | MINUSGREATER -> "->"
  | MODULE -> "module"
  | MUTABLE -> "mutable"
  | NEW -> "new"
  | NONREC -> "nonrec"
  | OBJECT -> "object"
  | OF -> "of"
  | OPEN -> "open"
  | OPTLABEL n -> Format.sprintf "?%s:" n
  | OR -> "or"
  | PERCENT -> "%"
  | PLUS -> "+"
  | PLUSDOT -> "+."
  | PLUSEQ -> "+="
  | PRIVATE -> "private"
  | QUESTION -> "?"
  | QUOTE -> "\""
  | QUOTED_STRING_EXPR (id, _, str, _, del) ->
      unlex_quoted_string ~op:("@" ^ id) str del
  | QUOTED_STRING_ITEM (id, _, str, _, del) ->
      unlex_quoted_string ~op:("%" ^ id) str del
  | RBRACE -> "}"
  | RBRACKET -> "]"
  | REC -> "rec"
  | RPAREN -> ")"
  | SEMI -> ";"
  | SEMISEMI -> ";;"
  | SIG -> "sig"
  | STAR -> "*"
  | STRING (str, _, None) -> Format.sprintf "%S" str
  | STRING (str, _, (Some _ as del)) -> unlex_quoted_string str del
  | STRUCT -> "struct"
  | THEN -> "then"
  | TILDE -> "~"
  | TO -> "to"
  | TRUE -> "true"
  | TRY -> "try"
  | TYPE -> "type"
  | UNDERSCORE -> "_"
  | VAL -> "val"
  | VIRTUAL -> "virtual"
  | WHEN -> "when"
  | WHILE -> "while"
  | WITH -> "with"

(** Ignore location diff *)
let token_equal a b =
  let open Migrate_ast.Parser in
  match (a, b) with
  | STRING (a, _, a'), STRING (b, _, b') -> a = b && a' = b'
  | ( QUOTED_STRING_EXPR (a, _, a', _, a'')
    , QUOTED_STRING_EXPR (b, _, b', _, b'') )
   |( QUOTED_STRING_ITEM (a, _, a', _, a'')
    , QUOTED_STRING_ITEM (b, _, b', _, b'') ) ->
      a = b && a' = b' && a'' = b''
  | COMMENT (a, _), COMMENT (b, _) -> a = b
  | a, b -> a = b

let diff_sources a b =
  let a, a_offsets = List.split (lex_source a)
  and b = List.map fst (lex_source b) in
  let a_offsets = Array.of_list a_offsets
  and a = Array.of_list a
  and b = Array.of_list b in
  let add i tok = (`Add, a_offsets.(i), tok)
  and del i tok = (`Del, a_offsets.(i), tok) in
  let diff =
    Diff.(levenshtein_script Array) ~equal:token_equal a b
    |> List.fold_left
         (fun acc -> function
           | Diff.Insert {expected; actual} -> add expected b.(actual) :: acc
           | Delete {expected} -> del expected a.(expected) :: acc
           | Substitute {expected; actual} ->
               del expected a.(expected) :: add expected b.(actual) :: acc )
         []
  in
  diff

(** On top of [unlex_token] with special cases. *)
let print_token = function
  | Migrate_ast.Parser.EOL -> "<NL>"
  | EOF -> "<EOF>"
  | tok -> unlex_token tok

let print_diff insert_offset insert_len diff =
  let pp_diff =
    let pp_log fmt (op, offset, tok) =
      let op = match op with `Add -> '+' | `Del -> '-' in
      Format.fprintf fmt "%d: %c %s" offset op (print_token tok)
    in
    Format.pp_print_list pp_log
  in
  Format.printf "@[<v 2>DIFF (inserted at %d-%d):@ %a@]@\n" insert_offset
    (insert_offset + insert_len)
    pp_diff diff

let run file =
  let conf = Conf.conventional_profile
  and opts =
    Conf.{debug= false; margin_check= false; format_invalid_files= false}
  in
  let initial_source = Stdio.In_channel.read_all file in
  let toffs = List.map snd (lex_source initial_source) in
  let insert = "(* toto *)" in
  insert_at_every_offsets insert toffs initial_source
    ~f:(fun insert_offset source ->
      match
        Translation_unit.parse_and_format Migrate_ast.Traverse.Structure
          ~input_name:file ~source conf opts
      with
      | Ok formatted -> (
        match diff_sources source formatted with
        | [] -> ()
        | diff -> print_diff insert_offset (String.length insert) diff )
      | Error err ->
          Translation_unit.print_error ~debug:false ~quiet:true
            ~input_name:file err )

open Cmdliner

let cmd =
  let a_file = Arg.(required & pos 0 (some file) None & info []) in
  let doc =
    "Repeatedly test ocamlformat with a comment before every tokens."
  in
  Term.(const run $ a_file, info ~doc "test_comments")

let () = Term.exit (Term.eval cmd)

/* Adapted from OCaml's lex/parser.mly (OCaml 5.4) */
/* Modifications: produce Mll_ast types, preserve named_defs, keep text */

%{
open Mll_ast

let mk_loc startpos endpos =
  { Location.loc_start = startpos; loc_end = endpos; loc_ghost = false }

let located startpos endpos value =
  { value; loc = mk_loc startpos endpos }
%}

%token <string> IDENT
%token <string> CHAR
%token <string> STRING
%token <string> OCAML_CODE
%token RULE PARSE SHORTEST AND EQUAL PIPE UNDERSCORE EOF_KW
       LBRACKET RBRACKET
%token STAR QUESTION PLUS LPAREN RPAREN CARET DASH LET AS HASH
%token EOF

/* Precedence — matches upstream ocamllex exactly */
%right AS
%left PIPE
%nonassoc CONCAT
%nonassoc QUESTION STAR PLUS
%left HASH
%nonassoc IDENT CHAR STRING UNDERSCORE EOF_KW LBRACKET LPAREN

%start <Mll_ast.lexer_def> lexer_def
%%

lexer_def:
    header named_regexps refill_handler RULE definition other_definitions
    trailer EOF
        { { header = $1;
            named_defs = List.rev $2;
            rules = $5 :: List.rev_map snd $6;
            trailer = $7;
            comments = [] } }
;
header:
    OCAML_CODE
        { Some (located $startpos $endpos $1) }
  | /*epsilon*/
        { None }
;
named_regexps:
    named_regexps LET IDENT EQUAL regexp
        { { def_name = located $startpos($3) $endpos($3) $3;
            def_body = $5;
            def_loc = mk_loc $startpos($2) $endpos } :: $1 }
  | /*epsilon*/
        { [] }
;
other_definitions:
    other_definitions AND definition
        { ($startpos($2), $3) :: $1 }
  | /*epsilon*/
        { [] }
;
refill_handler:
  | IDENT OCAML_CODE
        { (* "refill" { handler } — skip, collected as comment by lexer *) }
  | /*empty*/
        { () }
;
definition:
    IDENT arguments EQUAL PARSE entry
        { { entry_name = located $startpos($1) $endpos($1) $1;
            entry_args = $2;
            entry_is_shortest = false;
            entry_cases = $5;
            } }
  | IDENT arguments EQUAL SHORTEST entry
        { { entry_name = located $startpos($1) $endpos($1) $1;
            entry_args = $2;
            entry_is_shortest = true;
            entry_cases = $5;
            } }
;

arguments:
    IDENT arguments
        { (located $startpos($1) $endpos($1) $1) :: $2 }
  | /*epsilon*/
        { [] }
;

trailer:
    OCAML_CODE
        { Some (located $startpos $endpos $1) }
  | /*epsilon*/
        { None }
;

entry:
    case rest_of_entry
        { $1 :: List.rev $2 }
  | PIPE case rest_of_entry
        { $2 :: List.rev $3 }
;

rest_of_entry:
    rest_of_entry PIPE case
        { $3 :: $1 }
  |
        { [] }
;
case:
    regexp OCAML_CODE
        { { pattern = $1;
            action = located $startpos($2) $endpos($2) $2 } }
;
regexp:
    UNDERSCORE
        { Runderscore }
  | EOF_KW
        { Reof }
  | CHAR
        { Rchar $1 }
  | STRING
        { Rstring $1 }
  | LBRACKET char_class RBRACKET
        { $2 }
  | regexp STAR
        { Rrepeat $1 }
  | regexp QUESTION
        { Roption $1 }
  | regexp PLUS
        { Rplus $1 }
  | regexp HASH regexp
        { Rhash ($1, $3) }
  | regexp PIPE regexp
        { match $1, $3 with
          | Ralternative l1, Ralternative l2 -> Ralternative (l1 @ l2)
          | Ralternative l, r -> Ralternative (l @ [r])
          | l, Ralternative r -> Ralternative (l :: r)
          | l, r -> Ralternative [l; r] }
  | regexp regexp %prec CONCAT
        { match $1, $2 with
          | Rsequence l1, Rsequence l2 -> Rsequence (l1 @ l2)
          | Rsequence l, r -> Rsequence (l @ [r])
          | l, Rsequence r -> Rsequence (l :: r)
          | l, r -> Rsequence [l; r] }
  | LPAREN regexp RPAREN
        { Rparen $2 }
  | IDENT
        { Rident $1 }
  | regexp AS IDENT
        { Ras ($1, $3) }
;

char_class:
    CARET char_class1
        { Rcharset_neg (List.rev $2) }
  | char_class1
        { Rcharset (List.rev $1) }
;

char_class1:
    CHAR DASH CHAR
        { [Crange ($1, $3)] }
  | CHAR
        { [Cchar $1] }
  | char_class1 char_class1 %prec CONCAT
        { $2 @ $1 }
;

%%

{

let unescape_word : string -> string = fun s ->
  (* The common case is that there are no escape sequences. *)
  match String.index s '\\' with
  | exception Not_found -> s
  | _ ->
    let buffer = Buffer.create (String.length s) in
    let rec scan_word index =
      if index >= String.length s then
        ()
      else
        let c = s.[index] in
        let c, increment =
          match c with
          | '\\' ->
            if index + 1 < String.length s then
              match s.[index + 1] with
              | '{' | '}' | '[' | ']' | '@' as c -> c, 2
              | _ -> c, 1
            else c, 1
          | _ -> c, 1
        in
        Buffer.add_char buffer c;
        scan_word (index + increment)
    in
    scan_word 0;
    Buffer.contents buffer


type math_kind =
  Inline | Block

let math_constr kind x =
  match kind with
  | Inline -> `Math_span x
  | Block -> `Math_block x

type input = {
  file : string;
  offset_to_location : int -> Loc.point;
  warnings : Warning.t list ref;
  lexbuf : Lexing.lexbuf;
  string_buffer : Buffer.t;
}

let with_location_adjustments
    k input ?start_offset ?adjust_start_by ?end_offset ?adjust_end_by value =

  let start =
    match start_offset with
    | None -> Lexing.lexeme_start input.lexbuf
    | Some s -> s
  in
  let start =
    match adjust_start_by with
    | None -> start
    | Some s -> start + String.length s
  in
  let end_ =
    match end_offset with
    | None -> Lexing.lexeme_end input.lexbuf
    | Some e -> e
  in
  let end_ =
    match adjust_end_by with
    | None -> end_
    | Some s -> end_ - String.length s
  in
  let location = {
    Loc.file = input.file;
    start = input.offset_to_location start;
    end_ = input.offset_to_location end_;
  }
  in
  k input location value

let emit =
  with_location_adjustments (fun _ -> Loc.at)

let warning_loc =
  fun input location error ->
    input.warnings := (error location) :: !(input.warnings)

let warning =
  with_location_adjustments warning_loc

(* From ocaml.git/parsing/lexer.mll *)
let digit_value c =
  match c with
  | 'a' .. 'f' -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' -> 10 + Char.code c - Char.code 'A'
  | '0' .. '9' -> Char.code c - Char.code '0'
  | _ -> assert false

let num_value lexbuf ~base ~first ~last =
  let c = ref 0 in
  for i = first to last do
    let v = digit_value (Lexing.lexeme_char lexbuf i) in
    assert(v < base);
    c := (base * !c) + v
  done;
  !c

let char_for_decimal_code input lexbuf i =
  let c = num_value lexbuf ~base:10 ~first:i ~last:(i+2) in
  if (c < 0 || c > 255) then
    (warning input (Parse_error.invalid_char_code c);
    'x')
  else Char.chr c

let reference_token media start target input lexbuf =
  match start with
  | "{!" -> `Simple_reference target
  | "{{!" -> `Begin_reference_with_replacement_text target
  | "{:" -> `Simple_link (target)
  | "{{:" -> `Begin_link_with_replacement_text (target)

  | "{image!" -> `Simple_media (`Reference target, `Image)
  | "{image:" -> `Simple_media (`Link target, `Image)
  | "{audio!" -> `Simple_media (`Reference target, `Audio)
  | "{audio:" -> `Simple_media (`Link target, `Audio)
  | "{video!" -> `Simple_media (`Reference target, `Video)
  | "{video:" -> `Simple_media (`Link target, `Video)

  | _ ->
     let target, kind =
       match start with
       | "{{image!" -> `Reference target, `Image
       | "{{image:" -> `Link target, `Image
       | "{{audio!" -> `Reference target, `Audio
       | "{{audio:" -> `Link target, `Audio
       | "{{video!" -> `Reference target, `Video
       | "{{video:" -> `Link target, `Video
       | _ -> assert false
     in
     let token_descr = Token.describe (`Media_with_replacement_text (target, kind, "")) in
     let content = media token_descr (Buffer.create 1024) 0 (Lexing.lexeme_start lexbuf) input lexbuf in
     `Media_with_replacement_text (target, kind, content)

(** Verbatims' content must be separated from their delimiters: [{v content v}]
    and not [{vcontentv}]. Such leading space is not part of the content. *)
let verbatim_whitespace_first input start_offset text =
  match text.[0] with
  | ' ' -> String.sub text 1 (String.length text - 1)
  | '\t' | '\r' | '\n' -> text
  | exception Invalid_argument _ -> ""
  | _ ->
    warning
      input
      ~start_offset
      ~end_offset:(start_offset + 2)
      Parse_error.no_leading_whitespace_in_verbatim;
    text

(** Verbatims' content must be separated from their delimiters: [{v content v}]
    and not [{vcontentv}]. Such leading space is not part of the content. *)
let verbatim_whitespace_last text =
  match text.[String.length text - 1] with
  | ' ' -> String.sub text 0 (String.length text - 1)
  | '\t' | '\r' | '\n' -> text
  | exception Invalid_argument _ -> ""
  | _ -> text

let emit_verbatim input start_offset buffer =
  let t = Buffer.contents buffer in
  let t = verbatim_whitespace_first input start_offset t in
  let t = verbatim_whitespace_last t in
  emit input (`Verbatim t) ~start_offset

(* The locations have to be treated carefully in this function. We need to ensure that
   the [`Code_block] location matches the entirety of the block including the terminator,
   and the content location is precisely the location of the text of the code itself.
   Note that the location reflects the content _without_ stripping of whitespace, whereas
   the value of the content in the tree has whitespace stripped from the beginning,
   and trailing empty lines removed. *)
let emit_code_block ~start_offset content_offset input metadata delim terminator
    c has_results =
  let c = Buffer.contents c in
  (* We first handle the case wehere there is no line at the beginning, then
     remove trailing, leading lines and deindent *)
  let c =
    with_location_adjustments ~adjust_end_by:terminator
      ~start_offset:content_offset
      (fun _ -> Loc.at)
      input c
  in
  emit ~start_offset input (`Code_block (metadata, delim, c, has_results))

let heading_level input level =
  if String.length level >= 2 && level.[0] = '0' then begin
    warning
      input ~start_offset:1 (Parse_error.leading_zero_in_heading_level level)
  end;
  int_of_string level

let buffer_add_lexeme buffer lexbuf =
  Buffer.add_string buffer (Lexing.lexeme lexbuf)

}

let markup_char =
  ['{' '}' '[' ']' '@' '|']
let space_char =
  [' ' '\t' '\n' '\r']
let bullet_char =
  ['-' '+']

let word_char =
  (_ # markup_char # space_char # bullet_char) | ('\\' markup_char)

let horizontal_space =
  [' ' '\t']
let newline =
  '\n' | "\r\n"

let media_start =
    "{!" | "{{!" | "{:" | "{{:"
  | "{image!" | "{{image!" | "{image:" | "{{image:"
  | "{video!" | "{{video!" | "{video:" | "{{video:"
  | "{audio!" | "{{audio!" | "{audio:" | "{{audio:"

let raw_markup =
  ([^ '%'] | '%'+ [^ '%' '}'])* '%'*

let raw_markup_target =
  ([^ ':' '%'] | '%'+ [^ ':' '%' '}'])* '%'*

let language_tag_char =
  ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' ]

let delim_char =
  ['a'-'z' 'A'-'Z' '0'-'9' '_' ]

let tag_unquoted_char = (_ # '=' # '"' # space_char # '[')
let tag_unquoted_atom = tag_unquoted_char+

let tag_escape = '\\' '"'
let tag_quoted_char = _ # '"'
let tag_quoted_atom = (tag_quoted_char | tag_escape)*

rule reference_paren_content input start ref_offset start_offset depth_paren
  buffer =
  parse
  | '('
    {
      buffer_add_lexeme buffer lexbuf ;
      reference_paren_content input start ref_offset start_offset
        (depth_paren + 1) buffer lexbuf }
  | ')'
    {
      buffer_add_lexeme buffer lexbuf ;
      if depth_paren = 0 then
        reference_content input start ref_offset buffer lexbuf
      else
        reference_paren_content input start ref_offset start_offset
          (depth_paren - 1) buffer lexbuf }
  | eof
    { warning
        input
        ~start_offset
        (Parse_error.unclosed_bracket ~bracket:"(") ;
      Buffer.contents buffer }
  | _
    {
      buffer_add_lexeme buffer lexbuf ;
      reference_paren_content input start ref_offset start_offset depth_paren
        buffer lexbuf }

and reference_content input start start_offset buffer = parse
  | '}'
    {
      Buffer.contents buffer
    }
  | '('
    {
      buffer_add_lexeme buffer lexbuf ;
      reference_paren_content input start start_offset
        (Lexing.lexeme_start lexbuf) 0 buffer lexbuf
    }
  | '"' [^ '"']* '"'
    {
      buffer_add_lexeme buffer lexbuf ;
      reference_content input start start_offset buffer lexbuf
    }
  | eof
    { warning
        input
        ~start_offset
        (Parse_error.unclosed_bracket ~bracket:start) ;
      Buffer.contents buffer }
  | _
    {
      buffer_add_lexeme buffer lexbuf ;
      reference_content input start start_offset buffer lexbuf }

and token input = parse
  | horizontal_space* eof
    { emit input `End }

  | ((horizontal_space* newline as prefix)
    horizontal_space* ((newline horizontal_space*)+ as suffix) as ws)
    { emit input (`Blank_line ws) ~adjust_start_by:prefix ~adjust_end_by:suffix }

  | (horizontal_space* newline horizontal_space* as ws)
    { emit input (`Single_newline ws) }

  | (horizontal_space+ as ws)
    { emit input (`Space ws) }

  | (horizontal_space* (newline horizontal_space*)? as p) '}'
    { emit input `Right_brace ~adjust_start_by:p }

  | '|'
    { emit input `Bar }

  | word_char (word_char | bullet_char | '@')*
  | bullet_char (word_char | bullet_char | '@')+ as w
    { emit input (`Word (unescape_word w)) }

  | '['
    { code_span
        (Buffer.create 1024) 0 (Lexing.lexeme_start lexbuf) input lexbuf }

  | '-'
    { emit input `Minus }

  | '+'
    { emit input `Plus }

  | "{b"
    { emit input (`Begin_style `Bold) }

  | "{i"
    { emit input (`Begin_style `Italic) }

  | "{e"
    { emit input (`Begin_style `Emphasis) }

  | "{L"
    { emit input (`Begin_paragraph_style `Left) }

  | "{C"
    { emit input (`Begin_paragraph_style  `Center) }

  | "{R"
    { emit input (`Begin_paragraph_style  `Right) }

  | "{^"
    { emit input (`Begin_style `Superscript) }

  | "{_"
    { emit input (`Begin_style `Subscript) }

  | "{math" space_char
    { math Block (Buffer.create 1024) 0 (Lexing.lexeme_start lexbuf) input lexbuf }

  | "{m" horizontal_space
    { math Inline (Buffer.create 1024) 0 (Lexing.lexeme_start lexbuf) input lexbuf }


  | "{!modules:" ([^ '}']* as modules) '}'
    { emit input (`Modules modules) }

  | (media_start as start)
    {
      let start_offset = Lexing.lexeme_start lexbuf in
      let target =
        reference_content input start start_offset (Buffer.create 16) lexbuf
      in
      let token = reference_token media start target input lexbuf in
      emit ~start_offset input token }

  | "{["
    { code_block false (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf) None (Buffer.create 256) "" input lexbuf }

  | (("{" (delim_char* as delim) "@" horizontal_space*) as prefix) (language_tag_char+ as lang_tag_)
    {
      let start_offset = Lexing.lexeme_start lexbuf in
      let lang_tag =
        with_location_adjustments ~adjust_start_by:prefix (fun _ -> Loc.at) input lang_tag_
      in
      let emit_truncated_code_block () =
        let empty_content = with_location_adjustments (fun _ -> Loc.at) input "" in
        emit ~start_offset input (`Code_block (Some (lang_tag, []), delim, empty_content, false))
      in
      (* Disallow result block sections for code blocks without a delimiter.
         This avoids the surprising parsing of '][' ending the code block. *)
      let allow_result_block = delim <> "" in
      let code_block_with_metadata metadata =
        let content_offset = Lexing.lexeme_end lexbuf in
        let metadata = Some (lang_tag, metadata) in
        let prefix = Buffer.create 256 in
        code_block allow_result_block start_offset content_offset metadata
          prefix delim input lexbuf
      in
      match code_block_metadata_tail input None [] lexbuf with
      | `Ok metadata -> code_block_with_metadata metadata
      | `Eof ->
          warning input ~start_offset Parse_error.truncated_code_block_meta;
          emit_truncated_code_block ()
    }

  | "{@" horizontal_space* '['
    {
      warning input Parse_error.no_language_tag_in_meta;
      code_block false (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf) None (Buffer.create 256) "" input lexbuf
    }

  | "{v"
    { verbatim
        (Buffer.create 1024) None (Lexing.lexeme_start lexbuf) input lexbuf }

  | "{%" ((raw_markup_target as target) ':')? (raw_markup as s)
    ("%}" | eof as e)
    { let token = `Raw_markup (target, s) in
      if e <> "%}" then
        warning
          input
          ~start_offset:(Lexing.lexeme_end lexbuf)
          (Parse_error.not_allowed
            ~what:(Token.describe `End)
            ~in_what:(Token.describe token));
      emit input token }

  | "{ul"
    { emit input (`Begin_list `Unordered) }

  | "{ol"
    { emit input (`Begin_list `Ordered) }

  | "{li"
    { emit input (`Begin_list_item `Li) }

  | "{-"
    { emit input (`Begin_list_item `Dash) }

  | "{table"
    { emit input (`Begin_table_heavy) }

  | "{t"
    { emit input (`Begin_table_light) }

  | "{tr"
    { emit input `Begin_table_row }

  | "{th"
    { emit input (`Begin_table_cell `Header) }

  | "{td"
    { emit input (`Begin_table_cell `Data) }

  | '{' (['0'-'9']+ as level) ':' (([^ '}'] # space_char)* as label)
    { emit
        input (`Begin_section_heading (heading_level input level, Some label)) }

  | '{' (['0'-'9']+ as level)
    { emit input (`Begin_section_heading (heading_level input level, None)) }

  | "@author" ((horizontal_space+ [^ '\r' '\n']*)? as author)
    { emit input (`Tag (`Author author)) }

  | "@deprecated"
    { emit input (`Tag `Deprecated) }

  | "@param" horizontal_space+ ((_ # space_char)+ as name)
    { emit input (`Tag (`Param name)) }

  | ("@raise" | "@raises") horizontal_space+ ((_ # space_char)+ as name)
    { emit input (`Tag (`Raise name)) }

  | ("@return" | "@returns")
    { emit input (`Tag `Return) }

  | ("@children_order")
    { emit input (`Tag `Children_order) }

  | ("@toc_status")
    { emit input (`Tag `Toc_status) }

  | ("@order_category")
    { emit input (`Tag `Order_category) }

  | ("@short_title")
    { emit input (`Tag `Short_title) }

  | "@see" horizontal_space* '<' ([^ '>']* as url) '>'
    { emit input (`Tag (`See (`Url, url))) }

  | "@see" horizontal_space* '\'' ([^ '\'']* as filename) '\''
    { emit input (`Tag (`See (`File, filename))) }

  | "@see" horizontal_space* '"' ([^ '"']* as name) '"'
    { emit input (`Tag (`See (`Document, name))) }

  | "@since" ((horizontal_space+ [^ '\r' '\n']*)? as version)
    { emit input (`Tag (`Since version)) }

  | "@before" horizontal_space+ ((_ # space_char)+ as version)
    { emit input (`Tag (`Before version)) }

  | "@version" ((horizontal_space+ [^ '\r' '\n']*)? as version)
    { emit input (`Tag (`Version version)) }

  | "@canonical" ((horizontal_space+ [^ '\r' '\n']*)? as identifier)
    { emit input (`Tag (`Canonical identifier)) }

  | "@inline"
    { emit input (`Tag `Inline) }

  | "@open"
    { emit input (`Tag `Open) }

  | "@closed"
    { emit input (`Tag `Closed) }

  | "@hidden"
    { emit input (`Tag `Hidden) }

  | "]}"
    { emit input `Right_code_delimiter}

  | '{'
    { try bad_markup_recovery (Lexing.lexeme_start lexbuf) input lexbuf
      with Failure _ ->
        warning
          input
          (Parse_error.bad_markup
            "{" ~suggestion:"escape the brace with '\\{'.");
        emit input (`Word "{") }

  | ']'
    { warning input Parse_error.unpaired_right_bracket;
      emit input (`Word "]") }

  | "@param"
    { warning input Parse_error.truncated_param;
      emit input (`Tag (`Param "")) }

  | ("@raise" | "@raises") as tag
    { warning input (Parse_error.truncated_raise tag);
      emit input (`Tag (`Raise "")) }

  | "@before"
    { warning input Parse_error.truncated_before;
      emit input (`Tag (`Before "")) }

  | "@see"
    { warning input Parse_error.truncated_see;
      emit input (`Word "@see") }

  | '@' ['a'-'z' 'A'-'Z']+ as tag
    { warning input (Parse_error.unknown_tag tag);
      emit input (`Word tag) }

  | '@'
    { warning input Parse_error.stray_at;
      emit input (`Word "@") }

  | '\r'
    { warning input Parse_error.stray_cr;
      token input lexbuf }

  | "{!modules:" ([^ '}']* as modules) eof
    { warning
        input
        ~start_offset:(Lexing.lexeme_end lexbuf)
        (Parse_error.not_allowed
          ~what:(Token.describe `End)
          ~in_what:(Token.describe (`Modules "")));
      emit input (`Modules modules) }

and code_span buffer nesting_level start_offset input = parse
  | ']'
    { if nesting_level = 0 then
        emit input (`Code_span (Buffer.contents buffer)) ~start_offset
      else begin
        Buffer.add_char buffer ']';
        code_span buffer (nesting_level - 1) start_offset input lexbuf
      end }

  | '['
    { Buffer.add_char buffer '[';
      code_span buffer (nesting_level + 1) start_offset input lexbuf }

  | '\\' ('[' | ']' as c)
    { Buffer.add_char buffer c;
      code_span buffer nesting_level start_offset input lexbuf }

  | newline horizontal_space* (newline horizontal_space*)+
    { warning
        input
        (Parse_error.not_allowed
          ~what:(Token.describe (`Blank_line "\n\n"))
          ~in_what:(Token.describe (`Code_span "")));
      Buffer.add_char buffer ' ';
      code_span buffer nesting_level start_offset input lexbuf }
  | newline horizontal_space*
    { Buffer.add_char buffer ' ';
      code_span buffer nesting_level start_offset input lexbuf }

  | eof
    { warning
        input
        (Parse_error.not_allowed
          ~what:(Token.describe `End)
          ~in_what:(Token.describe (`Code_span "")));
      emit input (`Code_span (Buffer.contents buffer)) ~start_offset }

  | _ as c
    { Buffer.add_char buffer c;
      code_span buffer nesting_level start_offset input lexbuf }

and math kind buffer nesting_level start_offset input = parse
  | '}'
    { if nesting_level == 0 then
        emit input (math_constr kind (Buffer.contents buffer)) ~start_offset
      else begin
        Buffer.add_char buffer '}';
        math kind buffer (nesting_level - 1) start_offset input lexbuf
      end
      }
  | '{'
    { Buffer.add_char buffer '{';
      math kind buffer (nesting_level + 1) start_offset input lexbuf }
  | ("\\{" | "\\}") as s
    { Buffer.add_string buffer s;
      math kind buffer nesting_level start_offset input lexbuf }
  | (newline) as s
    {
      match kind with
      | Inline ->
        warning
          input
          (Parse_error.not_allowed
            ~what:(Token.describe (`Blank_line "\n"))
            ~in_what:(Token.describe (math_constr kind "")));
        Buffer.add_char buffer '\n';
        math kind buffer nesting_level start_offset input lexbuf
      | Block ->
        Buffer.add_string buffer s;
        math kind buffer nesting_level start_offset input lexbuf
    }
  | eof
    { warning
        input
        (Parse_error.not_allowed
          ~what:(Token.describe `End)
          ~in_what:(Token.describe (math_constr kind "")));
      emit input (math_constr kind (Buffer.contents buffer)) ~start_offset }
  | _ as c
    { Buffer.add_char buffer c;
      math kind buffer nesting_level start_offset input lexbuf }

and media tok_descr buffer nesting_level start_offset input = parse
  | '}'
    { if nesting_level == 0 then
        Buffer.contents buffer
      else begin
        Buffer.add_char buffer '}';
        media tok_descr buffer (nesting_level - 1) start_offset input lexbuf
      end
      }
  | '{'
    { Buffer.add_char buffer '{';
      media tok_descr buffer (nesting_level + 1) start_offset input lexbuf }
  | ("\\{" | "\\}") as s
    { Buffer.add_string buffer s;
      media tok_descr buffer nesting_level start_offset input lexbuf }
  | eof
    { warning
        input
        (Parse_error.not_allowed
          ~what:(Token.describe `End)
          ~in_what:tok_descr);
      Buffer.contents buffer}
  | (newline)
    { Buffer.add_char buffer ' ';
      media tok_descr buffer nesting_level start_offset input lexbuf }
  | _ as c
    { Buffer.add_char buffer c;
      media tok_descr buffer nesting_level start_offset input lexbuf }

and verbatim buffer last_false_terminator start_offset input = parse
  | (space_char as c) "v}"
    { Buffer.add_char buffer c;
      emit_verbatim input start_offset buffer }

  | "v}"
    { Buffer.add_string buffer "v}";
      verbatim
        buffer (Some (Lexing.lexeme_start lexbuf)) start_offset input lexbuf }

  | eof
    { begin match last_false_terminator with
      | None ->
        warning
          input
          (Parse_error.not_allowed
            ~what:(Token.describe `End)
            ~in_what:(Token.describe (`Verbatim "")))
      | Some location ->
        warning
          input
          ~start_offset:location
          ~end_offset:(location + 2)
          Parse_error.no_trailing_whitespace_in_verbatim
      end;
      emit_verbatim input start_offset buffer }

  | _ as c
    { Buffer.add_char buffer c;
      verbatim buffer last_false_terminator start_offset input lexbuf }

and bad_markup_recovery start_offset input = parse
  | [^ '}']+ as text '}' as rest
    { let suggestion =
        Printf.sprintf "did you mean '{!%s}' or '[%s]'?" text text in
      warning
        input
        ~start_offset
        (Parse_error.bad_markup ("{" ^ rest) ~suggestion);
      emit input (`Code_span text) ~start_offset}

(* Based on OCaml's parsing/lexer.mll
   We're missing a bunch of cases here, and can add them
   if necessary. Using the missing cases will cause a warning *)
and string input = parse
 | '\"'
   { let result = Buffer.contents input.string_buffer in
     Buffer.clear input.string_buffer;
     result }
 | '\\' newline [' ' '\t']*
   { string input lexbuf }
 | '\\' (['\\' '\'' '\"' 'n' 't' 'b' 'r' ' '] as c)
   { Buffer.add_char input.string_buffer
       (match c with
        | '\\' -> '\\'
        | '\'' -> '\''
        | '\"' -> '\"'
        | 'n' -> '\n'
        | 't' -> '\t'
        | 'b' -> '\b'
        | 'r' -> '\r'
        | ' ' -> ' '
        | _ -> assert false);
     string input lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
    { Buffer.add_char input.string_buffer (char_for_decimal_code input lexbuf 1);
      string input lexbuf }
  | '\\' (_ as c)
    { warning input (Parse_error.should_not_be_escaped c);
      Buffer.add_char input.string_buffer c;
      string input lexbuf }
  | eof
    { warning input Parse_error.truncated_string;
      Buffer.contents input.string_buffer }
  | (_ as c)
    { Buffer.add_char input.string_buffer c;
      string input lexbuf }
  
and code_block_metadata_atom input = parse
 | '"'
   {
    let start_offset = Lexing.lexeme_start input.lexbuf in
    Buffer.clear input.string_buffer;
    let s = string input lexbuf in
    with_location_adjustments ~start_offset (fun _ -> Loc.at) input s }
 | (tag_unquoted_atom as value)
   { with_location_adjustments (fun _ -> Loc.at) input value }
 | ('=' as c) 
   { warning input (Parse_error.code_block_tag_invalid_char c);
     with_location_adjustments (fun _ -> Loc.at) input "" }

and code_block_metadata_tail input tag acc = parse
 | space_char+
   { let acc = match tag with | Some t -> `Tag t :: acc | None -> acc in
     let tag = code_block_metadata_atom input lexbuf in
     code_block_metadata_tail input (Some tag) acc lexbuf }
 | space_char* '[' (* Nb this will be a longer match than the above case! *)
   {
     let acc = match tag with | Some t -> `Tag t :: acc | None -> acc in
     `Ok (List.rev acc) }
 | '='
   { match tag with
     | Some t ->
       let value = code_block_metadata_atom input lexbuf in
       code_block_metadata_tail input None (`Binding (t, value) :: acc) lexbuf
     | None ->
       warning input (Parse_error.code_block_tag_invalid_char '=');
       code_block_metadata_tail input None acc lexbuf }
 | (_ # space_char # '[' # '=' as c) (_ # space_char # '[')*
   {
      let start_offset = Lexing.lexeme_start input.lexbuf in
      let end_offset = start_offset + 1 in
      warning input ~start_offset ~end_offset (Parse_error.code_block_tag_invalid_char c);
    code_block_metadata_tail input None acc lexbuf }
 | eof
    { `Eof }

and code_block allow_result_block start_offset content_offset metadata prefix delim input = parse
  | ("]" (delim_char* as delim') "[") as terminator
    { if delim = delim' && allow_result_block
      then emit_code_block ~start_offset content_offset input metadata delim terminator prefix true
      else (
        Buffer.add_string prefix terminator;
        code_block allow_result_block start_offset content_offset metadata
          prefix delim input lexbuf
      )
    }
  | ("]" (delim_char* as delim') "}") as terminator
    { 
      if delim = delim'
      then emit_code_block ~start_offset content_offset input metadata delim terminator prefix false
      else (
        Buffer.add_string prefix terminator;
        code_block allow_result_block start_offset content_offset metadata
          prefix delim input lexbuf
      )
    }
  | eof
    {
      warning input ~start_offset Parse_error.truncated_code_block;
      emit_code_block ~start_offset content_offset input metadata delim "" prefix false
    }
  | (_ as c)
    {
      Buffer.add_char prefix c;
      code_block allow_result_block start_offset content_offset metadata
        prefix delim input lexbuf
    }

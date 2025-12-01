module Ast = Ast
module Loc = Loc
module Warning = Warning

type t = {
  ast : Ast.t;
  warnings : Warning.t list;
  reversed_newlines : (int * int) list;
  original_pos : Lexing.position;
}

(* odoc uses an ocamllex lexer. The "engine" for such lexers is the standard
   [Lexing] module.

   As the [Lexing] module reads the input, it keeps track of only the byte
   offset into the input. It is normally the job of each particular lexer
   implementation to decide which character sequences count as newlines, and
   keep track of line/column locations. This is usually done by writing several
   extra regular expressions, and calling [Lexing.new_line] at the right time.

   Keeping track of newlines like this makes the odoc lexer somewhat too
   diffiult to read, however. To factor the aspect of keeping track of newlines
   fully out of the odoc lexer, instead of having it keep track of newlines as
   it's scanning the input, the input is pre-scanned before feeding it into the
   lexer. A table of all the newlines is assembled, and used to convert offsets
   into line/column pairs after the lexer emits tokens.

   [reversed_newlines ~input ~comment_location offset] returns a list of pairs
   of (line number * offset), allowing the easy conversion from the byte
   [offset], relative to the beginning of a comment, into a location, relative
   to the beginning of the file containing the comment. This can then be used
   to convert from byte offset to line number / column number - a Loc.point,
   and additionally for converting back from a Loc.point to a Lexing.position.
*)

let reversed_newlines : input:string -> (int * int) list =
 fun ~input ->
  let rec find_newlines line_number input_index newlines_accumulator =
    if input_index >= String.length input then newlines_accumulator
    else if
      (* This is good enough to detect CR-LF also. *)
      input.[input_index] = '\n'
    then
      find_newlines (line_number + 1) (input_index + 1)
        ((line_number + 1, input_index + 1) :: newlines_accumulator)
    else find_newlines line_number (input_index + 1) newlines_accumulator
  in
  find_newlines 1 0 [ (1, 0) ]

(* [offset_to_location] converts from an offset within the comment text, where
   [reversed_newlines] is the result of the above function and [comment_location]
   is the location of the comment within its file. The function is meant to be
   partially applied to its first two arguments, at which point it is passed to
   the lexer, so it can apply the table to its emitted tokens. *)

let offset_to_location :
    reversed_newlines:(int * int) list ->
    comment_location:Lexing.position ->
    int ->
    Loc.point =
 fun ~reversed_newlines ~comment_location byte_offset ->
  let rec scan_to_last_newline reversed_newlines_prefix =
    match reversed_newlines_prefix with
    | [] -> assert false
    | (line_in_comment, line_start_offset) :: prefix ->
        if line_start_offset > byte_offset then scan_to_last_newline prefix
        else
          let column_in_comment = byte_offset - line_start_offset in
          let line_in_file =
            line_in_comment + comment_location.Lexing.pos_lnum - 1
          in
          let column_in_file =
            if line_in_comment = 1 then
              column_in_comment + comment_location.Lexing.pos_cnum
              - comment_location.Lexing.pos_bol
            else column_in_comment
          in
          { Loc.line = line_in_file; column = column_in_file }
  in
  scan_to_last_newline reversed_newlines

(* Given a Loc.point and the result of [parse_comment], this function returns
   a valid Lexing.position *)
let position_of_point : t -> Loc.point -> Lexing.position =
 fun v point ->
  let { reversed_newlines; original_pos; _ } = v in
  let line_in_comment = point.Loc.line - original_pos.pos_lnum + 1 in
  let rec find_pos_bol reversed_newlines_prefix =
    match reversed_newlines_prefix with
    | [] -> assert false
    | [ _ ] -> original_pos.pos_bol
    | (line_number, line_start_offset) :: prefix ->
        if line_number > line_in_comment then find_pos_bol prefix
        else line_start_offset + original_pos.pos_cnum
  in
  let pos_bol = find_pos_bol reversed_newlines in
  let pos_lnum = point.Loc.line in
  let pos_cnum = point.column + pos_bol in
  let pos_fname = original_pos.pos_fname in
  { Lexing.pos_bol; pos_lnum; pos_cnum; pos_fname }

(* The main entry point for this module *)
let parse_comment ~location ~text =
  let warnings = ref [] in
  let reversed_newlines = reversed_newlines ~input:text in
  let string_buffer = Buffer.create 256 in
  let token_stream =
    let lexbuf = Lexing.from_string text in
    let offset_to_location =
      offset_to_location ~reversed_newlines ~comment_location:location
    in
    let input : Lexer.input =
      {
        file = location.Lexing.pos_fname;
        offset_to_location;
        warnings;
        lexbuf;
        string_buffer;
      }
    in
    Stream.from (fun _token_index -> Some (Lexer.token input lexbuf))
  in
  let ast, warnings = Syntax.parse warnings token_stream in
  { ast; warnings; reversed_newlines; original_pos = location }

(* Accessor functions, as [t] is opaque *)
let warnings t = t.warnings
let ast t = t.ast

(** [deindent ~what input ~start_offset s] "deindents" [s] by an offset computed
    from [start_offset] and [input], corresponding to the begining of a code
    block or verbatim. If that is not possible (eg there is a non-whitespace
    line starting with less than [offset] whitespaces), it unindents as much as
    possible and raises a warning. *)
let deindent : what:string -> loc:Loc.span -> string -> string * Warning.t list
    =
 fun ~what ~loc s ->
  let offset = loc.start.column in
  (* Whitespace-only lines do not count, so they return [None]. *)
  let count_leading_whitespace line =
    let rec count_leading_whitespace' index len =
      if index = len then None
      else
        match line.[index] with
        | ' ' | '\t' -> count_leading_whitespace' (index + 1) len
        | _ -> Some index
    in
    let len = String.length line in
    (* '\r' may remain because we only split on '\n' below. This is important
       for the first line, which would be considered not empty without this check. *)
    let len = if len > 0 && line.[len - 1] = '\r' then len - 1 else len in
    count_leading_whitespace' 0 len
  in

  let lines = Astring.String.cuts ~sep:"\n" s in

  let least_amount_of_whitespace =
    List.fold_left
      (fun least_so_far line ->
        match (count_leading_whitespace line, least_so_far) with
        | Some n, least when n < least -> n
        | _ -> least_so_far)
      offset lines
  in
  let warning =
    if least_amount_of_whitespace < offset then
      [ Parse_error.not_enough_indentation_in_code_block ~what loc ]
    else []
  in
  let drop n line =
    (* Since blank lines were ignored when calculating
       [least_amount_of_whitespace], their length might be less than the
       amount. *)
    if String.length line < n then ""
    else String.sub line n (String.length line - n)
  in
  let lines = List.map (drop least_amount_of_whitespace) lines in
  (String.concat "\n" lines, warning)

(** Implements the rules for code block as specified in [odoc_for_authors],
    section on code blocks and indentation. *)
let code_block_content ~what ~loc s =
  let indent = loc.Loc.start.column in
  (* Remove the first line (to first \n char, included) if it's whitespace only.
     Otherwise, indent at [indent] level to account for offset. *)
  let rec handle_first_newline index =
    if index >= String.length s then String.make indent ' ' ^ s
    else
      match s.[index] with
      | ' ' | '\t' | '\r' -> handle_first_newline (index + 1)
      | '\n' -> String.sub s (index + 1) (String.length s - index - 1)
      | _ -> String.make indent ' ' ^ s
  in
  let s = handle_first_newline 0 in
  (* Remove the last line (from last \n char, included) if it's whitespace
     only. *)
  let rec handle_last_newline index =
    if index < 0 then s
    else
      match s.[index] with
      | ' ' | '\t' | '\r' -> handle_last_newline (index - 1)
      | '\n' -> String.sub s 0 index
      | _ -> s
  in
  let s = handle_last_newline (String.length s - 1) in
  deindent ~what ~loc s

let verbatim_content loc c =
  let what = "verbatim" in
  code_block_content ~what ~loc c
let codeblock_content loc c =
  let what = "code block" in
  code_block_content ~what ~loc c

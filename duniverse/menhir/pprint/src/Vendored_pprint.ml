(******************************************************************************)
(*                                                                            *)
(*                                    PPrint                                  *)
(*                                                                            *)
(*                        François Pottier, Inria Paris                       *)
(*                              Nicolas Pouillard                             *)
(*                                                                            *)
(*         Copyright 2007-2022 Inria. All rights reserved. This file is       *)
(*        distributed under the terms of the GNU Library General Public       *)
(*        License, with an exception, as described in the file LICENSE.       *)
(*                                                                            *)
(******************************************************************************)

include PPrintEngine

(* -------------------------------------------------------------------------- *)

(* Predefined single-character documents. *)

let lparen          = char '('
let rparen          = char ')'
let langle          = char '<'
let rangle          = char '>'
let lbrace          = char '{'
let rbrace          = char '}'
let lbracket        = char '['
let rbracket        = char ']'
let squote          = char '\''
let dquote          = char '"'
let bquote          = char '`'
let semi            = char ';'
let colon           = char ':'
let comma           = char ','
let dot             = char '.'
let sharp           = char '#'
let slash           = char '/'
let backslash       = char '\\'
let equals          = char '='
let qmark           = char '?'
let tilde           = char '~'
let at              = char '@'
let percent         = char '%'
let dollar          = char '$'
let caret           = char '^'
let ampersand       = char '&'
let star            = char '*'
let plus            = char '+'
let minus           = char '-'
let underscore      = char '_'
let bang            = char '!'
let bar             = char '|'

(* -------------------------------------------------------------------------- *)

(* Repetition. *)

let[@inline] twice doc =
  doc ^^ doc

let repeat n doc =
  let rec loop n doc accu =
    if n = 0 then
      accu
    else
      loop (n - 1) doc (doc ^^ accu)
  in
  loop n doc empty

(* -------------------------------------------------------------------------- *)

(* Delimiters. *)

let[@inline] precede   l x   = l ^^ x
let[@inline] terminate r x   = x ^^ r
let[@inline] enclose l r x   = l ^^ x ^^ r

let[@inline] squotes  x = enclose squote squote x
let[@inline] dquotes  x = enclose dquote dquote x
let[@inline] bquotes  x = enclose bquote bquote x
let[@inline] braces   x = enclose lbrace rbrace x
let[@inline] parens   x = enclose lparen rparen x
let[@inline] angles   x = enclose langle rangle x
let[@inline] brackets x = enclose lbracket rbracket x

(* -------------------------------------------------------------------------- *)

(* Some functions on lists. *)

(* A variant of [fold_left] that keeps track of the element index. *)

let foldli (f : int -> 'b -> 'a -> 'b) (accu : 'b) (xs : 'a list) : 'b =
  let r = ref 0 in
  List.fold_left (fun accu x ->
    let i = !r in
    r := i + 1;
    f i accu x
  ) accu xs

(* -------------------------------------------------------------------------- *)

(* Working with lists of documents. *)

let concat docs =
  (* We take advantage of the fact that [^^] operates in constant
     time, regardless of the size of its arguments. The document
     that is constructed is essentially a reversed list (i.e., a
     tree that is biased towards the left). This is not a problem;
     when pretty-printing this document, the engine will descend
     along the left branch, pushing the nodes onto its stack as
     it goes down, effectively reversing the list again. *)
  List.fold_left (^^) empty docs

let separate sep docs =
  foldli (fun i accu doc ->
    if i = 0 then
      doc
    else
      accu ^^ sep ^^ doc
  ) empty docs

let concat_map f xs =
  List.fold_left (fun accu x ->
    accu ^^ f x
  ) empty xs

let separate_map sep f xs =
  foldli (fun i accu x ->
    if i = 0 then
      f x
    else
      accu ^^ sep ^^ f x
  ) empty xs

let separate2 sep last_sep docs =
  let n = List.length docs in
  foldli (fun i accu doc ->
    if i = 0 then
      doc
    else
      accu ^^ (if i < n - 1 then sep else last_sep) ^^ doc
  ) empty docs

let optional f = function
  | None ->
      empty
  | Some x ->
      f x

(* -------------------------------------------------------------------------- *)

(* Text. *)

(* This variant of [String.index_from] returns an option. *)

let index_from s i c =
  try
    Some (String.index_from s i c)
  with Not_found ->
    None

(* [lines s] chops the string [s] into a list of lines, which are turned
   into documents. *)

let lines s =
  let rec chop accu i =
    match index_from s i '\n' with
    | Some j ->
        let accu = substring s i (j - i) :: accu in
	chop accu (j + 1)
    | None ->
        substring s i (String.length s - i) :: accu
  in
  List.rev (chop [] 0)

let arbitrary_string s =
  separate (break 1) (lines s)

(* [split ok s] splits the string [s] at every occurrence of a character
   that satisfies the predicate [ok]. The substrings thus obtained are
   turned into documents, and a list of documents is returned. No information
   is lost: the concatenation of the documents yields the original string.
   This code is not UTF-8 aware. *)

let split ok s =
  let n = String.length s in
  let rec index_from i =
    if i = n then
      None
    else if ok s.[i] then
      Some i
    else
      index_from (i + 1)
  in
  let rec chop accu i =
    match index_from i with
    | Some j ->
        let accu = substring s i (j - i) :: accu in
	let accu = char s.[j] :: accu in
	chop accu (j + 1)
    | None ->
        substring s i (String.length s - i) :: accu
  in
  List.rev (chop [] 0)

(* [words s] chops the string [s] into a list of words, which are turned
   into documents. *)

let words s =
  let n = String.length s in
  (* A two-state finite automaton. *)
  (* In this state, we have skipped at least one blank character. *)
  let rec skipping accu i =
    if i = n then
      (* There was whitespace at the end. Drop it. *)
      accu
    else match s.[i] with
    | ' '
    | '\t'
    | '\n'
    | '\r' ->
        (* Skip more whitespace. *)
	skipping accu (i + 1)
    | _ ->
        (* Begin a new word. *)
	word accu i (i + 1)
  (* In this state, we have skipped at least one non-blank character. *)
  and word accu i j =
    if j = n then
      (* Final word. *)
      substring s i (j - i) :: accu
    else match s.[j] with
    | ' '
    | '\t'
    | '\n'
    | '\r' ->
        (* A new word has been identified. *)
        let accu = substring s i (j - i) :: accu in
	skipping accu (j + 1)
    | _ ->
        (* Continue inside the current word. *)
	word accu i (j + 1)
  in
  List.rev (skipping [] 0)

let flow_map sep f docs =
  foldli (fun i accu doc ->
    if i = 0 then
      f doc
    else
      accu ^^
      (* This idiom allows beginning a new line if [doc] does not
	 fit on the current line. *)
      group (sep ^^ f doc)
  ) empty docs

let flow sep docs =
  flow_map sep (fun x -> x) docs

let url s =
  flow (break 0) (split (function '/' | '.' -> true | _ -> false) s)

(* -------------------------------------------------------------------------- *)
(* Alignment and indentation. *)

let hang i d =
  align (nest i d)

let ( !^ ) = string

let[@inline] ( ^/^ ) x y =
  x ^^ break 1 ^^ y

let prefix n b x y =
  group (x ^^ nest n (break b ^^ y))

let[@inline] (^//^) x y =
  prefix 2 1 x y

let jump n b y =
  group (nest n (break b ^^ y))

let infix n b op x y =
  prefix n b (x ^^ blank b ^^ op) y

let surround n b opening contents closing =
  group (opening ^^ nest n (       break b  ^^ contents) ^^        break b ^^ closing )

let soft_surround n b opening contents closing =
  group (opening ^^ nest n (group (break b) ^^ contents) ^^ group (break b ^^ closing))

let surround_separate n b void opening sep closing docs =
  match docs with
  | [] ->
      void
  | _ :: _ ->
      surround n b opening (separate sep docs) closing

let surround_separate_map n b void opening sep closing f xs =
  match xs with
  | [] ->
      void
  | _ :: _ ->
      surround n b opening (separate_map sep f xs) closing

(* -------------------------------------------------------------------------- *)
(* Printing OCaml values. *)

module OCaml = struct

open Printf

type constructor = string
type type_name = string
type record_field = string
type tag = int

(* -------------------------------------------------------------------------- *)

(* This internal [sprintf]-like function produces a document. We use [string],
   as opposed to [arbitrary_string], because the strings that we produce will
   never contain a newline character. *)

let[@inline] dsprintf format =
  ksprintf string format

(* -------------------------------------------------------------------------- *)

(* Nicolas prefers using this code as opposed to just [sprintf "%g"] or
   [sprintf "%f"]. The latter print [inf] and [-inf], whereas OCaml
   understands [infinity] and [neg_infinity]. [sprintf "%g"] does not add a
   trailing dot when the number happens to be an integral number.  [sprintf
   "%F"] seems to lose precision and ignores the precision modifier. *)

let valid_float_lexeme (s : string) : string =
  let l = String.length s in
  let rec loop i =
    if i >= l then
      (* If we reach the end of the string and have found only characters in
	 the set '0' .. '9' and '-', then this string will be considered as an
	 integer literal by OCaml. Adding a trailing dot makes it a float
	 literal. *)
      s ^ "."
    else
      match s.[i] with
      | '0' .. '9' | '-' -> loop (i + 1)
      | _ -> s
  in loop 0

(* This function constructs a string representation of a floating point
   number. This representation is supposed to be accepted by OCaml as a
   valid floating point literal. *)

let float_representation (f : float) : string =
  match classify_float f with
  | FP_nan ->
    "nan"
  | FP_infinite ->
      if f < 0.0 then "neg_infinity" else "infinity"
  | _ ->
      (* Try increasing precisions and validate. *)
      let s = sprintf "%.12g" f in
      if f = float_of_string s then valid_float_lexeme s else
      let s = sprintf "%.15g" f in
      if f = float_of_string s then valid_float_lexeme s else
      sprintf "%.18g" f

(* -------------------------------------------------------------------------- *)

(* A few constants and combinators, used below. *)

let some =
  string "Some"

let none =
  string "None"

let lbracketbar =
  string "[|"

let rbracketbar =
  string "|]"

let seq1 opening separator closing =
  surround_separate 2 0
    (opening ^^ closing) opening (separator ^^ break 1) closing

let seq2 opening separator closing =
  surround_separate_map 2 1
    (opening ^^ closing) opening (separator ^^ break 1) closing

(* -------------------------------------------------------------------------- *)

(* The following functions are printers for many types of OCaml values. *)

(* There is no protection against cyclic values. *)

let tuple =
  seq1 lparen comma rparen

let variant _ cons _ args =
  match args with
  | [] ->
      !^cons
  | _ :: _ ->
      !^cons ^^ tuple args

let record _ fields =
  seq2 lbrace semi rbrace (fun (k, v) -> infix 2 1 equals !^k v) fields

let option f = function
  | None ->
      none
  | Some x ->
      some ^^ tuple [f x]

let list f xs =
  seq2 lbracket semi rbracket f xs

let flowing_list f xs =
  group (lbracket ^^ space ^^ nest 2 (
    flow_map (semi ^^ break 1) f xs
  ) ^^ space ^^ rbracket)

let array f xs =
  seq2 lbracketbar semi rbracketbar f (Array.to_list xs)

let flowing_array f xs =
  group (lbracketbar ^^ space ^^ nest 2 (
    flow_map (semi ^^ break 1) f (Array.to_list xs)
  ) ^^ space ^^ rbracketbar)

let ref f x =
  record "ref" ["contents", f !x]

let float f =
  string (float_representation f)

let int =
  dsprintf "%d"

let int32 =
  dsprintf "%ld"

let int64 =
  dsprintf "%Ld"

let nativeint =
  dsprintf "%nd"

let char =
  dsprintf "%C"

let bool =
  dsprintf "%B"

let unit =
  dsprintf "()"

let string =
  dsprintf "%S"

let unknown tyname _ =
  dsprintf "<abstr:%s>" tyname

type representation =
  document

end (* OCaml *)

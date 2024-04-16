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

include module type of PPrintEngine (** @inline *)

(** {1:combinators High-Level Combinators} *)

(** {2 Single Characters} *)

(**The following atomic documents consist of a single character. Each of
   them is a synonym for the application of {!char} to some constant
   character. For instance, {!lparen} is a synonym for [char '(']. *)

val lparen: document
val rparen: document
val langle: document
val rangle: document
val lbrace: document
val rbrace: document
val lbracket: document
val rbracket: document
val squote: document
val dquote: document
val bquote: document
val semi: document
val colon: document
val comma: document
val dot: document
val sharp: document
val slash: document
val backslash: document
val equals: document
val qmark: document
val tilde: document
val at: document
val percent: document
val dollar: document
val caret: document
val ampersand: document
val star: document
val plus: document
val minus: document
val underscore: document
val bang: document
val bar: document

(** {2 Delimiters} *)

(**[precede l x] is [l ^^ x]. *)
val precede: document -> document -> document

(**[terminate r x] is [x ^^ r]. *)
val terminate: document -> document -> document

(**[enclose l r x] is [l ^^ x ^^ r]. *)
val enclose: document -> document -> document -> document

(**The following combinators enclose a document within a pair of delimiters.
   They are partial applications of [enclose]. No whitespace or line break
   is introduced. *)

val squotes: document -> document
val dquotes: document -> document
val bquotes: document -> document
val braces: document -> document
val parens: document -> document
val angles: document -> document
val brackets: document -> document

(** {2 Repetition} *)

(**[twice doc] is the document obtained by concatenating two copies of
   the document [doc]. *)
val twice: document -> document

(**[repeat n doc] is the document obtained by concatenating [n] copies of
   the document [doc]. *)
val repeat: int -> document -> document

(** {2 Lists and Options} *)

(**[concat docs] is the concatenation of the documents in the list [docs]. *)
val concat: document list -> document

(**[separate sep docs] is the concatenation of the documents in the list
   [docs]. The separator [sep] is inserted between every two adjacent
   documents. *)
val separate: document -> document list -> document

(**[concat_map f xs] is equivalent to [concat (List.map f xs)]. *)
val concat_map: ('a -> document) -> 'a list -> document

(**[separate_map sep f xs] is equivalent to [separate sep (List.map f xs)]. *)
val separate_map: document -> ('a -> document) -> 'a list -> document

(**[separate2 sep last_sep docs] is the concatenation of the documents in
   the list [docs]. The separator [sep] is inserted between every two
   adjacent documents, except between the last two documents, where the
   separator [last_sep] is used instead. *)
val separate2: document -> document -> document list -> document

(**[optional f None] is the empty document. [optional f (Some x)] is
   the document [f x]. *)
val optional: ('a -> document) -> 'a option -> document

(** {2 Text} *)

(**[lines s] is the list of documents obtained by splitting [s] at newline
   characters, and turning each line into a document via [substring]. This
   code is not UTF-8 aware. *)
val lines: string -> document list

(**[arbitrary_string s] is equivalent to [separate (break 1) (lines s)].
   It is analogous to [string s], but is valid even if the string [s]
   contains newline characters. *)
val arbitrary_string: string -> document

(**[words s] is the list of documents obtained by splitting [s] at whitespace
   characters, and turning each word into a document via [substring]. All
   whitespace is discarded. This code is not UTF-8 aware. *)
val words: string -> document list

(**[split ok s] splits the string [s] before and after every occurrence of a
   character that satisfies the predicate [ok]. The substrings thus obtained
   are turned into documents, and a list of documents is returned. No
   information is lost: the concatenation of the documents yields the
   original string. This code is not UTF-8 aware. *)
val split: (char -> bool) -> string -> document list

(**[flow sep docs] separates the documents in the list [docs] with the
   separator [sep] and arranges for a new line to begin whenever a document
   does not fit on the current line. This is useful for typesetting
   free-flowing, ragged-right text. A typical choice of [sep] is [break b],
   where [b] is the number of spaces that must be inserted between two
   consecutive words (when displayed on the same line). *)
val flow: document -> document list -> document

(**[flow_map sep f docs] is equivalent to [flow sep (List.map f docs)]. *)
val flow_map: document -> ('a -> document) -> 'a list -> document

(**[url s] is a possible way of displaying the URL [s]. A potential line
   break is inserted immediately before and immediately after every slash
   and dot character. *)
val url: string -> document

(** {2 Alignment and Indentation} *)

(**[hang n doc] is analogous to [align], but additionally indents all lines,
   except the first one, by [n]. Thus, the text in the box forms a hanging
   indent. *)
val hang: int -> document -> document

(**[prefix n b left right] has the following flat layout:
   {[
     left right
   ]}
   and the following non-flat layout:
   {[
     left
       right
   ]}
   The parameter [n] controls the nesting of [right] (when not flat).
   The parameter [b] controls the number of spaces between [left] and [right]
   (when flat). *)
val prefix: int -> int -> document -> document -> document

(**[jump n b right] is equivalent to [prefix n b empty right]. *)
val jump: int -> int -> document -> document

(**[infix n b middle left right] has the following flat layout:
   {[
     left middle right
   ]}
   and the following non-flat layout:
   {[
     left middle
       right
   ]}
   The parameter [n] controls the nesting of [right] (when not flat).
   The parameter [b] controls the number of spaces between [left] and [middle]
   (always) and between [middle] and [right] (when flat). *)
val infix: int -> int -> document -> document -> document -> document

(**[surround n b opening contents closing] has the following flat layout:
   {[
     opening contents closing
   ]}
   and the following non-flat layout:
   {[
     opening
       contents
       closing
   ]}
   The parameter [n] controls the nesting of [contents] (when not flat).
   The parameter [b] controls the number of spaces between [opening] and
   [contents] and between [contents] and [closing] (when flat).
*)
val surround: int -> int -> document -> document -> document -> document

(**[soft_surround] is analogous to [surround], but involves more than one
   group, so it offers possibilities other than the completely flat layout
   (where [opening], [contents], and [closing] appear on a single line) and
   the completely developed layout (where [opening], [contents], and
   [closing] appear on separate lines). It tries to place the beginning of
   [contents] on the same line as [opening], and to place [closing] on the
   same line as the end of [contents], if possible. *)
val soft_surround: int -> int -> document -> document -> document -> document

(**[surround_separate n b void opening sep closing docs] is equivalent to
   [surround n b opening (separate sep docs) closing], except when the list
   [docs] is empty, in which case it reduces to [void]. *)
val surround_separate:
  int -> int ->
  document -> document -> document -> document ->
  document list -> document

(**[surround_separate_map n b void opening sep closing f xs] is equivalent
   to [surround_separate n b void opening sep closing (List.map f xs)]. *)
val surround_separate_map:
  int -> int ->
  document -> document -> document -> document ->
  ('a -> document) -> 'a list -> document

(** {2 Short-Hands} *)

(**[!^s] is a short-hand for [string s]. *)
val ( !^ ) : string -> document

(**[x ^/^ y] separates [x] and [y] with a breakable space.
   It is a short-hand for [x ^^ break 1 ^^ y]. *)
val ( ^/^ ) : document -> document -> document

(**[x ^//^ y] is a short-hand for [prefix 2 1 x y]. *)
val ( ^//^ ) : document -> document -> document

(** {1:ocaml Printing OCaml Values} *)

(**This module offers document combinators that help print OCaml values. The
   strings produced by rendering these documents are supposed to be accepted
   by the OCaml parser as valid values.

   These functions do {i not} distinguish between mutable and immutable
   values. They do {i not} recognize sharing, and do {i not} incorporate a
   protection against cyclic values. *)
module OCaml : sig

(* The signature of this module is compatible with that expected by the
   [camlp4] generator [Camlp4RepresentationGenerator]. This explains why
   some functions have unused parameters. This is also the reason why
   there is a type [representation]. *)

type constructor = string
type type_name = string
type record_field = string
type tag = int

(**[variant _ dc _ args] represents a constructed value whose data
   constructor is [dc] and whose arguments are [args]. The other two
   parameters are presently unused. *)
val variant : type_name -> constructor -> tag -> document list -> document

(**[record _ fields] represents a record value whose fields are [fields].
   The other parameter is presently unused. *)
val record : type_name -> (record_field * document) list -> document

(**[tuple args] represents a tuple value whose components are [args]. *)
val tuple : document list -> document

(**[string s] represents the literal string [s]. *)
val string : string -> document

(**[int i] represents the literal integer [i]. *)
val int : int -> document

(**[int32 i] represents the literal 32-bit integer [i]. *)
val int32 : int32 -> document

(**[int64 i] represents the literal 64-bit integer [i]. *)
val int64 : int64 -> document

(**[nativeint i] represents the literal native integer [i]. *)
val nativeint : nativeint -> document

(**[float f] represents the literal floating-point number [f]. *)
val float : float -> document

(**[char c] represents the literal character [c]. *)
val char : char -> document

(**[bool b] represents the Boolean value [b]. *)
val bool : bool -> document

(**[unit] represents the unit constant [()]. *)
val unit : document

(**[option f o] represents the option [o]. The representation of the
   element, if present, is computed by the function [f]. *)
val option : ('a -> document) -> 'a option -> document

(**[list f xs] represents the list [xs]. The representation of each element
   is computed by the function [f]. If the whole list fits on a single line,
   then it is printed on a single line; otherwise each element is printed on
   a separate line. *)
val list : ('a -> document) -> 'a list -> document

(**[flowing_list f xs] represents the list [xs]. The representation of each
   element is computed by the function [f]. As many elements are possible
   are printed on each line. *)
val flowing_list : ('a -> document) -> 'a list -> document

(**[array f xs] represents the array [xs]. The representation of each
   element is computed by the function [f]. If the whole array fits on a
   single line, then it is printed on a single line; otherwise each element
   is printed on a separate line. *)
val array : ('a -> document) -> 'a array -> document

(**[flowing_array f xs] represents the array [xs]. The representation of
   each element is computed by the function [f]. As many elements are
   possible are printed on each line. *)
val flowing_array : ('a -> document) -> 'a array -> document

(**[ref r] represents the reference [r]. The representation of the content
   is computed by the function [f]. *)
val ref : ('a -> document) -> 'a ref -> document

(** [unknown t _] represents an unknown value of type [t]. It is rendered
    as a string of the form [<abstr:t>]. *)
val unknown : type_name -> 'a -> document

(**/**)

type representation =
  document

end (* OCaml *)

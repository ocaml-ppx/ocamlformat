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

(**[PPrint] is an OCaml library for {b pretty-printing textual documents}.
   It takes care of {b indentation and line breaks}, and is typically used
   to {b pretty-print code}. *)

(** {1:building Building Documents} *)

(**The abstract type of documents. *)
type document

(** {2 Atomic Documents} *)

(**[empty] is the empty document. *)
val empty: document

(**[char c] is an atomic document that consists of the single character [c].
   This character must not be a newline character. *)
val char: char -> document

(**[string s] is an atomic document that consists of the string [s]. This
   string must not contain a newline. The printing engine assumes that the
   ideal width of this string is [String.length s]. This assumption is safe
   if this is an ASCII string. Otherwise, {!fancystring} or {!utf8string}
   should be preferred. *)
val string: string -> document

(**[substring s ofs len] is an atomic document that consists of the portion
   of the string [s] delimited by the offset [ofs] and the length [len].
   This portion must not contain a newline. [substring s ofs len] is
   equivalent to [string (String.sub s ofs len)], but is expected to be more
   efficient, as the substring is not actually extracted. *)
val substring: string -> int -> int -> document

(**[fancystring s alen] is an atomic document that consists of the string
   [s]. This string must not contain a newline. The string may contain fancy
   characters: color escape characters, UTF-8 characters, etc. Thus, its
   apparent length (which measures how many columns the text will take up on
   screen) differs from its length in bytes. The printing engine assumes
   that its apparent length is [alen]. *)
val fancystring: string -> int -> document

(**[fancysubstring s ofs len alen] is equivalent to [fancystring (String.sub
   s ofs len) alen]. *)
val fancysubstring : string -> int -> int -> int -> document

(**[utf8string s] is an atomic document that consists of the UTF-8-encoded
   string [s]. This string must not contain a newline. [utf8string s] is
   equivalent to [fancystring s (utf8_length s)], where [utf8_length s] is
   the apparent length of the UTF-8-encoded string [s]. *)
val utf8string: string -> document

(** [utf8format format <args>...] is equivalent to
    [utf8string (Printf.sprintf format <args>...)]. *)
val utf8format: ('a, unit, string, document) format4 -> 'a

(** {2 Blanks and Newlines} *)

(**The atomic document [hardline] represents a forced newline. This document
   has infinite ideal width: thus, if there is a choice between printing it
   in flat mode and printing it in normal mode, normal mode is preferred. In
   other words, when [hardline] is placed directly inside a group, this
   group is dissolved: [group hardline] is equivalent to [hardline]. This
   combinator should be seldom used; consider using {!break} instead. *)
val hardline: document

(**The atomic document [blank n] consists of [n] blank characters. A blank
   character is like an ordinary ASCII space character [char ' '], except
   that blank characters that appear at the end of a line are automatically
   suppressed. *)
val blank: int -> document

(**[space] is a synonym for [blank 1]. It consists of one blank character.
   It is therefore not equivalent to [char ' ']. *)
val space: document

(**The document [break n] is a breakable blank of width [n]. It produces [n]
   blank characters if the printing engine is in flat mode, and a single
   newline character if the printing engine is in normal mode. [break 1] is
   equivalent to [ifflat (blank 1) hardline]. *)
val break: int -> document

(** {2 Composite Documents} *)

(**[doc1 ^^ doc2] is the concatenation of the documents [doc1] and [doc2]. *)
val (^^): document -> document -> document

(**[group doc] encodes a choice. If the document [doc] fits on the current
   line, then it is rendered on a single line, in flat mode. (All [group]
   combinators inside it are then ignored.) Otherwise, this group is
   dissolved, and [doc] is rendered in normal mode. There might be more
   groups within [doc], whose presence leads to further choices being
   explored. *)
val group: document -> document

(**[ifflat doc1 doc2] is rendered as [doc1] if the printing engine is in
   flat mode, that is, if the printing engine has determined that some
   enclosing group fits on the current line. Otherwise, it is rendered as
   [doc2]. Use this combinator with caution! Because the printing engine is
   free to choose between [doc1] and [doc2], these documents must be
   semantically equivalent. It is up to the user to enforce this property. *)
val ifflat: document -> document -> document

(**To render the document [nest j doc], the printing engine temporarily
   increases the current indentation level by [j], then renders [doc]. The
   effect of the current indentation level is as follows: every time a
   newline character is emitted, it is immediately followed by [n] blank
   characters, where [n] is the current indentation level. Thus, one may
   think of [nest j doc] roughly as the document [doc] in which [j] blank
   characters have been inserted after every newline character. *)
val nest: int -> document -> document

(**To render [align doc], the printing engine sets the current indentation
   level to the current column, then renders [doc]. In other words, the
   document [doc] is rendered within a box whose upper left corner is the
   current position of the printing engine. *)
val align: document -> document

(**A point is a pair of a line number and a column number. *)
type point =
  int * int

(**A range is a pair of points. *)
type range =
  point * point

(**The document [range hook doc] is printed like the document [doc], but
   allows the caller to register a hook that is applied, when the document
   is printed, to the range occupied by this document in the output text.
   This offers a way of mapping positions in the output text back to
   (sub)documents. *)
val range: (range -> unit) -> document -> document

(** {1:inspecting Inspecting Documents} *)

(**Documents are abstract, and cannot be inspected. Nevertheless, it is
   possible to test whether a document is empty. *)

(**[is_empty doc] determines whether the document [doc] is empty. Most ways
   of constructing empty documents, such as [empty], [empty ^^ empty],
   [nest j empty], and so on, are recognized as such. However, a document
   constructed by {!val-custom} or {!val-range} is never considered empty. *)
val is_empty: document -> bool

(** {1:rendering Rendering Documents} *)

(**Three renderers are available. They offer the same API, described
   by the signature {!RENDERER}, and differ only in the nature of the
   output channel that they use. *)

(**This signature describes the document renderers in a manner that
   is independent of the type of the output channel. *)
module type RENDERER = sig

  (**The type of the output channel. *)
  type channel

  (**The type of documents. *)
  type document

  (** [pretty rfrac width channel document] pretty-prints the document
      [document] into the output channel [channel]. The parameter [width] is
      the maximum number of characters per line. The parameter [rfrac] is the
      ribbon width, a fraction relative to [width]. The ribbon width is the
      maximum number of non-indentation characters per line. *)
  val pretty: float -> int -> channel -> document -> unit

  (** [compact channel document] prints the document [document] to the output
      channel [channel]. No indentation is used. All newline instructions are
      respected, that is, no groups are flattened. *)
  val compact: channel -> document -> unit

end

(**This renderer sends its output into an output channel. *)
module ToChannel : RENDERER
  with type channel = out_channel
   and type document = document

(**This renderer sends its output into a memory buffer. *)
module ToBuffer : RENDERER
  with type channel = Buffer.t
   and type document = document

(**This renderer sends its output into a formatter channel. *)
module ToFormatter : RENDERER
  with type channel = Format.formatter
   and type document = document

(** {1:defining Defining Custom Documents} *)

(**It is possible to define custom document constructors, provided they meet
   the expectations of the printing engine. In short, the custom document
   combinator {!val-custom} expects an object of class {!class-type-custom}.
   This object must provide three methods. The method [requirement] must
   compute the ideal width of the custom document. The methods [pretty] and
   [compact] must render the custom document. For this purpose, they have
   access to the {{!output}output channel} and to the {{!state}state} of the
   printing engine. *)

(** A width requirement is expressed as an integer. The value [max_int]
    is reserved and represents infinity. *)
type requirement = int

(**[infinity] represents an infinite width requirement. *)
val infinity : requirement

(**An output channel is abstractly represented as an object equipped with
   methods for displaying one character and for displaying a substring. *)
class type output = object

  (**[char c] sends the character [c] to the output channel. *)
  method char: char -> unit

  (**[substring s ofs len] sends the substring of [s] delimited by the
     offset [ofs] and the length [len] to the output channel. *)
  method substring: string -> int (* offset *) -> int (* length *) -> unit

end

(**The internal state of the rendering engine is exposed to the user who
   wishes to define custom documents. However, its structure is subject to
   change in future versions of the library. *)
type state = {

    width: int;
    (** The line width. This parameter is fixed throughout the execution of
        the renderer. *)

    ribbon: int;
    (** The ribbon width. This parameter is fixed throughout the execution of
        the renderer. *)

    mutable last_indent: int;
    (** The number of blanks that were printed at the beginning of the current
        line. This field is updated (only) when a hardline is emitted. It is
        used (only) to determine whether the ribbon width constraint is
        respected. *)

    mutable line: int;
    (** The current line. This field is updated (only) when a hardline is
        emitted. It is not used by the pretty-printing engine itself. *)

    mutable column: int;
    (** The current column. This field must be updated whenever something is
        sent to the output channel. It is used (only) to determine whether the
        width constraint is respected. *)

  }

(**A custom document is defined by implementing an object of class
   {!class-type-custom}. *)
class type custom = object

  (**A custom document must publish the width (i.e., the number of columns)
     that it would like to occupy if printed on a single line (in flat
     mode). The special value [infinity] means that this document cannot be
     printed on a single line; this value causes any groups that contain
     this document to be dissolved. This method should in principle work in
     constant time. *)
  method requirement: requirement

  (**The method [pretty] is used by the main rendering algorithm. It has
     access to the output channel and to the printing engine's internal
     state. In addition, it receives the current indentation level and a
     Boolean flag that tells whether the engine is currently in flat mode.
     If the engine is in flat mode, then the document must be printed on a
     single line, in a manner that is consistent with the width requirement
     that was published ahead of time. If the engine is in normal mode, then
     there is no such obligation. The state must be updated in a manner that
     is consistent with what is sent to the output channel. *)
  method pretty: output -> state -> int -> bool -> unit

  (**The method [compact] is used by the compact rendering algorithm. It
     has access to the output channel only. *)
  method compact: output -> unit

end

(**[custom] constructs a custom document out an object of type
   {!class-type-custom}. *)
val custom: custom -> document

(**Some of the key functions of the library are exposed, in the hope that
   they may be useful to authors of custom (leaf and composite) documents.
   In the case of a leaf document, they can help perform certain basic
   functions; for instance, applying the function {!pretty} to the document
   {!hardline} is a simple way of printing a hardline, while respecting the
   indentation parameters and updating the state in a correct manner.
   Similarly, applying {!pretty} to the document [blank n] is a simple way
   of printing [n] blank characters. In the case of a composite document
   (one that contains subdocuments), these functions are essential: they
   allow computing the width requirement of a subdocument and displaying a
   subdocument. *)

(**[requirement doc] computes the width requirement of the document [doc].
   It runs in constant time. *)
val requirement: document -> requirement

(**[pretty output state indent flatten doc] prints the document [doc]. See
   the documentation of the method [pretty] in the class
   {!class-type-custom}. *)
val pretty: output -> state -> int -> bool -> document -> unit

(**[compact output doc] prints the document [doc]. See the documentation of
   the method [compact] in the class {!class-type-custom}. *)
val compact: output -> document -> unit

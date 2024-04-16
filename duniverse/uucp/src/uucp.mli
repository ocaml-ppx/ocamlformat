(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Unicode character properties.

    [Uucp] provides efficient access to a selection of character
    {{!props}properties} of the Unicode character database.

    Consult {{!page-unicode}this page} for a minimal Unicode
    introduction and OCaml Unicode tips. Individual modules have
    sample code related to the properties.

    {b References.}
    {ul
    {- {{:http://www.unicode.org/faq/}The Unicode FAQ.}}
    {- The Unicode Consortium.
    {e {{:http://www.unicode.org/versions/latest}The Unicode Standard}}.
    (latest version)}
    {- Mark Davis, Ken Whistler.
    {e {{:http://www.unicode.org/reports/tr44/}UAX #44 Unicode Character
    Database}}. (latest version)}} *)

(** {1:props Properties}

    Consult information about the {{!distrib_omit}property distribution
    in modules and omissions}. *)

val unicode_version : string
(** [unicode_version] is the Unicode version supported by the library. *)

module Age = Uucp__age
module Alpha = Uucp__alpha
module Block = Uucp__block
module Break = Uucp__break
module Case = Uucp__case
module Cjk = Uucp__cjk
module Emoji = Uucp__emoji
module Func = Uucp__func
module Gc = Uucp__gc
module Gen = Uucp__gen
module Hangul = Uucp__hangul
module Id = Uucp__id
module Name = Uucp__name
module Num = Uucp__num
module Script = Uucp__script
module White = Uucp__white

(** {1:distrib_omit Property module distribution and omissions}

    Properties are approximatively distributed in modules by scope of use
    like in this
    {{:http://www.unicode.org/reports/tr44/#Property_Index_Table}property
    index table}. However some subset of properties
    live in their own modules.

    Obsolete and
    {{:http://www.unicode.org/reports/tr44/#Deprecated_Property_Table}
    deprecated} properties are
    omitted.  So are those related to normalization, shaping and
    bidirectionality. Here is the full list of omitted properties,
    if you think one of these property should be added get in touch
    with a rationale.
    {ul
    {- Case.
       {{:http://www.unicode.org/reports/tr44/#Simple_Lowercase_Mapping}
       Simple_Lowercase_Mapping},
       {{:http://www.unicode.org/reports/tr44/#Simple_Uppercase_Mapping}
       Simple_Uppercase_Mapping},
       {{:http://www.unicode.org/reports/tr44/#Simple_Titlecase_Mapping}
       Simple_Titlecase_Mapping},
       {{:http://www.unicode.org/reports/tr44/#Simple_Case_Folding}
       Simple_Case_folding},
       {{:http://www.unicode.org/reports/tr44/#CWL}
       Changes_When_Lowercased},
       {{:http://www.unicode.org/reports/tr44/#CWU}
       Changes_When_Uppercased},
       {{:http://www.unicode.org/reports/tr44/#CWT}
       Changes_When_Titlecased},
       {{:http://www.unicode.org/reports/tr44/#CWCF}
       Changes_When_Casefolded},
       {{:http://www.unicode.org/reports/tr44/#CWCM}
       Changes_When_Casemapped},
       {{:https://www.unicode.org/reports/tr44/proposed.html#CWKCF}
       Changes_When_NFKC_Casefolded}.}
    {- Normalization. All properties under that section name in
       {{:http://www.unicode.org/reports/tr44/#Property_Index_Table}
       this table}.}
    {- Shaping and rendering.
       {{:http://www.unicode.org/reports/tr44/#Joining_Group}Joining_Group},
       {{:http://www.unicode.org/reports/tr44/#Joining_Type}Joining_Type},
       {{:http://www.unicode.org/reports/tr44/#Vertical_Orientation}Vertical_Orientation},
       {{:http://www.unicode.org/reports/tr44/#Indic_Syllabic_Category}
       Indic_Syllabic_Category},
       {{:http://www.unicode.org/reports/tr44/#Indic_Positional_Category}
       Indic_Positional_Category},
       {{:http://www.unicode.org/reports/tr44/#Prepended_Concatenation_Mark}
       Prepended_Concatenation_Mark}}
    {- Bidirectional. All properties under that section name in
       {{:http://www.unicode.org/reports/tr44/#Property_Index_Table}
       this table}.}
    {- CJK.
      {{:http://www.unicode.org/reports/tr44/#Unicode_Radical_Stroke}
      Unicode_Radical_Stroke},
      {{:http://www.unicode.org/reports/tr44/#Equivalent_Unified_Ideograph}
      Equivalent_Unified_Ideograph} and all the properties of the
      {{:http://www.unicode.org/reports/tr38/}Unicode HAN Database}.}
    {- Miscellaneous.
       {{:http://www.unicode.org/reports/tr44/#STerm}STerm}.}
    {- Contributory properties. All properties under that section in
       {{:http://www.unicode.org/reports/tr44/#Property_Index_Table}this
       table.}}} *)

(**/**)

(* Warning this is not part of the public API and subject
   to change without notice. *)

module Cmap : sig
  type 'a tree = Empty | C of int * 'a | Cn of 'a tree * 'a tree * int * 'a
  type 'a t = { default : 'a; tree : 'a tree; }
  val get : 'a t -> int -> 'a
  val of_sorted_list : 'a -> [ `C of int * 'a ] list -> 'a t
  val height : 'a t -> int
  val word_size : ('a -> int) -> 'a t -> int
  val dump :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module Rmap : sig
  type 'a tree =
    | Empty
    | R of int * int * 'a
    | Rn of 'a tree * 'a tree * int * int * 'a
  type 'a t = { default : 'a; tree : 'a tree; }
  val get : 'a t -> int -> 'a
  val of_sorted_list : 'a -> [ `R of int * int * 'a ] list -> 'a t
  val height : 'a t -> int
  val word_size : ('a -> int) -> 'a t -> int
  val dump :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module Tmap : sig
  type 'a t = { default : 'a; l0 : 'a array array array; }
  val nil : 'a array
  val create : 'a -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val word_size : ('a -> int) -> 'a t -> int
  val dump :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module Tmapbool : sig
  type t = { default : bool; l0 : string array array; }
  val nil : 'a array
  val snil : string
  val create : bool -> t
  val get : t -> int -> bool
  val set : t -> int -> bool -> unit
  val word_size : t -> int
  val dump : Format.formatter -> t -> unit
end

module Tmapbyte : sig
  type t = { default : int; l0 : string array array; }
  val nil : 'a array
  val snil : string
  val create : int -> t
  val get : t -> int -> int
  val set : t -> int -> int -> unit
  val word_size : t -> int
  val dump : Format.formatter -> t -> unit
end

(**/**)

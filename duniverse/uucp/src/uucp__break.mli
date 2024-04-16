(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Break properties.

    These properties are mainly for the Unicode text segmentation and line
    breaking algorithm.

    {b References.}
    {ul
    {- Mark Davis.
    {e {{:http://www.unicode.org/reports/tr29/}UAX #29 Unicode Text
    Segmentation}}. (latest version)}
    {- Andy Heninger.
    {e {{:http://www.unicode.org/reports/tr14/}UAX #14 Unicode Line Breaking
    Algorithm}}. (latest version)}
    {- Ken Lunde 小林劍.
    {e {{:http://www.unicode.org/reports/tr11/}UAX #11 East Asian width.}
    (latest version)}}} *)


(** {1:line_break Line break} *)

type line =
  [ `AI | `AK | `AL | `AP | `AS | `B2 | `BA | `BB | `BK | `CB | `CJ | `CL
  | `CM | `CP | `CR | `EX | `EB | `EM | `GL | `H2 | `H3 | `HL | `HY | `ID
  | `IN | `IS | `JL | `JT | `JV | `LF | `NL | `NS | `NU | `OP | `PO | `PR
  | `QU | `RI | `SA | `SG | `SP | `SY | `VF | `VI | `WJ | `XX | `ZW | `ZWJ ]
(** The type for line breaks. *)

val pp_line : Format.formatter -> line -> unit
(** [pp_line ppf l] prints an unspecified representation of [l] on [ppf]. *)

val line : Uchar.t -> line
(** [line u] is [u]'s
    {{:http://www.unicode.org/reports/tr44/#Line_Break}line break}
    property. *)

(** {1:grapheme_cluster_break Grapheme cluster break} *)

type grapheme_cluster =
  [ `CN | `CR | `EX | `EB | `EBG | `EM | `GAZ | `L | `LF | `LV | `LVT | `PP
  | `RI | `SM | `T | `V | `XX | `ZWJ ]
(** The type for grapheme cluster breaks. *)

val pp_grapheme_cluster : Format.formatter -> grapheme_cluster -> unit
(** [pp_grapheme_cluster ppf g] prints an unspecified representation of [g]
    on [ppf]. *)

val grapheme_cluster : Uchar.t -> grapheme_cluster
(** [grapheme_cluster u] is [u]'s
    {{:http://www.unicode.org/reports/tr44/#Grapheme_Cluster_Break}grapheme
    cluster break} property. *)

(** {1:word_break Word break} *)

type word =
  [ `CR | `DQ | `EX | `EB | `EBG | `EM | `Extend | `FO | `GAZ | `HL | `KA
  | `LE | `LF | `MB | `ML | `MN | `NL | `NU | `RI | `SQ | `WSegSpace
  | `XX | `ZWJ ]
(** The type for word breaks. *)

val pp_word : Format.formatter -> word -> unit
(** [pp_word ppf b] prints an unspecified representation of [b] on [ppf]. *)

val word : Uchar.t -> word
(** [word u] is [u]'s
    {{:http://www.unicode.org/reports/tr44/#Word_Break}word break}
    property. *)

(** {1:sentence_break Sentence break} *)

type sentence =
  [ `AT | `CL | `CR | `EX | `FO | `LE | `LF | `LO | `NU | `SC | `SE | `SP
  | `ST | `UP | `XX ]
(** The type for sentence breaks. *)

val pp_sentence : Format.formatter -> sentence -> unit
(** [pp_sentence ppf b] prints an unspecified representation of [b] on [ppf]. *)

val sentence : Uchar.t -> sentence
(** [sentence u] is [u]'s
    {{:http://www.unicode.org/reports/tr44/#Sentence_Break}sentence break}
    property. *)

(** {1:indic_conjunct_break Indic conjunct break} *)

type indic_conjunct_break = [ `Consonant | `Extend | `Linker | `None ]
(** The type for Indic Conjunct Break. *)

val pp_indic_conjunct_break : Format.formatter -> indic_conjunct_break -> unit
(** [pp_indic_conjunct_break ppf b] prints an unspecified representation of [b]
    on [ppf]. *)

val indic_conjunct_break : Uchar.t -> indic_conjunct_break
(** [indic_conjunct_break u] is [u]'s
    {{:http://www.unicode.org/reports/tr44/#Indic_Conjunct_Break}
    Indic conjunct break} property. *)

(** {1:east_asian_width East Asian width} *)

type east_asian_width = [ `A | `F | `H | `N | `Na | `W ]
(** The type for East Asian widths. *)

val pp_east_asian_width : Format.formatter -> east_asian_width -> unit
(** [pp_east_asian_width ppf w] prints an unspecified representation of
    [w] on [ppf]. *)

val east_asian_width : Uchar.t -> east_asian_width
(** [east_asian_width u] is [u]'s
    {{:http://www.unicode.org/reports/tr44/#East_Asian_Width}East
    Asian width} property. *)

(** {1:terminal_width Terminal width} *)

val tty_width_hint : Uchar.t -> int
(** [tty_width_hint u] approximates [u]'s column width as rendered by a
    typical character terminal.

    The current implementation of the function returns either [0],
    [1], [2] or [-1]. The value [-1] is only returned for scalar
    values for which the property is non-sensical; clients are
    expected to sanitize their inputs and not to use the function
    with these scalar values which are those in range U+0001-U+001F
    ({b C0} controls without U+0000) and U+007F-U+009F (DELETE and
    {b C1} controls).

    {b Note.} Converting a string to
    {{:http://unicode.org/glossary/#normalization_form_c}normalization
    form C} before folding this function over its scalar values
    will, in general, yield better approximations (e.g. on Hangul).

    {b Warning.} This is not a normative property and only a
    heuristic. If you find yourself using this function please read
    carefully the following lines.

    This function is the moral equivalent of POSIX
    {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/wcwidth.html}
    [wcwidth]}, in that its purpose is to help align text displayed by a
    character terminal. It mimics [wcwidth], as widely implemented, in yet
    another way: it is {e mostly wrong}.

    Computing column width is a surprisingly difficult task in general. Much
    of the software infrastructure still carries legacy assumptions about the
    nature of text harking back to the ASCII era. Different terminal emulators
    attempt to cope with general Unicode text in different ways, creating a
    fundamental problem: width of text fragments will vary across terminal
    emulators, with no way of getting feedback from the output layer back
    into the text-producing layer.

    For example: on a modern Linux system, a collection of terminals
    will disagree on some or all of U+00AD, U+0CBF, and
    U+2029. They will likewise disagree about unassigned
    characters (category {e Cn}), sometimes contradicting the
    system's [wcwidth] (e.g. U+0378, U+0530).  Terminals using
    bare {{:http://cgit.freedesktop.org/xorg/lib/libXft}libxft}
    will display complex scripts differently from terminals using
    {{:http://www.freedesktop.org/wiki/Software/HarfBuzz}HarfBuzz},
    and the rendering on OS X will be slightly different from both.

    [tty_width_hint] uses a simple and predictable width algorithm, based
    on Markus Kuhn's {{:https://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c}
    portable [wcwidth]}:

    {ul
    {- Scalar values in the ranges U+0001-U+001F ({b C0} controls without
       U+0000) and U+007F-U+009F (DELETE and {b C1} controls) have undefined
       width ([-1]).}
    {- Characters with {{:http://www.unicode.org/reports/tr11/tr11-29.html}
       East Asian Width} {e Fullwidth} or {e Wide} have a width of [2].}
    {- Characters with
       {{:http://unicode.org/glossary/#general_category}General Category}
       {e Mn}, {e Me}, {e Cf} and U+0000 have a width of [0].}
    {- {e Most} other characters have a width of [1], including {e Cn}.}}

    This approach works well, in that it gives results generally consistent
    with a wide range of terminals, for
    {{:https://en.wikipedia.org/wiki/Alphabet}alphabetic} scripts, and for
    east Asian {{:https://en.wikipedia.org/wiki/Syllabary}syllabic} and
    {{:https://en.wikipedia.org/wiki/Logogram}logographic} scripts in
    non-decomposed form. Support varies for
    {{:https://en.wikipedia.org/wiki/Abjad}abjad} scripts in the presence of
    vowel marks, and it mostly breaks down on
    {{:https://en.wikipedia.org/wiki/Abugida}abugidas}.

    Moreover, non-text symbols like
    {{:http://unicode.org/emoji/charts/full-emoji-list.html}Emoji}
    or {{:http://unicode.org/charts/PDF/U4DC0.pdf}Yijing hexagrams}
    will be incorrectly classified as [1]-wide, but this in fact
    agrees with their rendering on many terminals.

    Clients should not over-rely on [tty_width_hint]. It provides a
    best-effort approximation which will sometimes fail in
    practice. *)


(** {1:break_low Low level interface} *)

(** Low level interface.

    This interface may be useful for table based implementers of
    segmenters. For each kind of break, property values are
    assigned integer values starting from [0]. An array
    allows to recover the high-level representation of the
    corresponding property value. *)
module Low : sig

  (** {1:low Low level access to break properties}

      {b Warning.} Do not mutate these array. *)

  val line : Uchar.t -> int
  (** [line u] is an integer that can be used with {!line_of_int}. *)

  val line_max : int
  (** [line_max] is the maximal value returned by {!val-line}. *)

  val line_of_int : line array
  (** [line_of_int.(i)] is the line break property value corresponding
      to [i]. *)

  val grapheme_cluster : Uchar.t -> int
  (** [grapheme_cluster u] is an integer that can be used with
      {!grapheme_cluster_of_int}. *)

  val grapheme_cluster_max : int
  (** [grapheme_cluster_max] is the maximal value returned by
      {!val-grapheme_cluster}. *)

  val grapheme_cluster_of_int : grapheme_cluster array
  (** [grapheme_cluster_of_int.(i)] is the grapheme cluster break property
      value corresponding to [i]. *)

  val word : Uchar.t -> int
  (** [word u] is an integer that can be used with {!word_of_int}. *)

  val word_max : int
  (** [word_max] is the maximal value returned by {!val-word}. *)

  val word_of_int : word array
  (** [word_of_int.(i)] is the word break property value
      corresponding to [i]. *)

  val sentence : Uchar.t -> int
  (** [sentence u] is an integer that can be used with {!sentence_of_int}. *)

  val sentence_max : int
  (** [sentence_max] is the maximal value returned by {!val-sentence}. *)

  val sentence_of_int : sentence array
  (** [sentence_of_int.(i)] is the sentence break property value
      corresponding to [i]. *)

  val indic_conjunct_break : Uchar.t -> int
  (** [indic_conjunct_break u] is an integer that can be used with
      {!indic_conjunct_break_of_int}. *)

  val indic_conjunct_break_max : int
  (** [indic_conjunct_break_max] is the maximal value returned by
      {!val-indic_conjunct_break_of_int}. *)

  val indic_conjunct_break_of_int : indic_conjunct_break array
  (** [indic_conjunct_break.(i)] is the Indic conjunct break property
      value corresponding to [i]. *)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Unicode character properties.

    [Uucp] provides efficient access to a selection of character
    {{!props}properties} of the Unicode character database.

    Consult {{!page-unicode}this page} for minimal Unicode
    introduction and OCaml Unicode tips. Individual modules have
    sample code related to the properties.

    {e Unicode version %%UNICODE_VERSION%%}

    {3 References}
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

(** Age property. *)
module Age : sig

  (** {1:ageprop Age property} *)

  type t = [ `Unassigned | `Version of int * int ]
  (** The type for character age. *)

  val compare : t -> t -> int
  (** [compare a a'] is [Stdlib.compare a a'] *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf a] prints an unspecified representation of [a] on [ppf]. *)

  val age : Uchar.t -> t
  (** [age u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Age}Age} property. *)
end

(** Alphabetic property. *)
module Alpha : sig

  (** {1:alphaprop Alphabetic property} *)

  val is_alphabetic : Uchar.t -> bool
  (** [is_alphabetic u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Alphabetic}Alphabetic}
      property. *)
end

(** Block property and block ranges.

    {3 References}
    {ul
    {- {{:http://www.unicode.org/faq/blocks_ranges.html}The Unicode
    blocks and ranges FAQ}.}} *)
module Block : sig

(** {1:blockprop Blocks} *)

  type t = [
    | `ASCII
    | `Adlam
    | `Aegean_Numbers
    | `Ahom
    | `Alchemical
    | `Alphabetic_PF
    | `Anatolian_Hieroglyphs
    | `Ancient_Greek_Music
    | `Ancient_Greek_Numbers
    | `Ancient_Symbols
    | `Arabic
    | `Arabic_Ext_A
    | `Arabic_Math
    | `Arabic_PF_A
    | `Arabic_PF_B
    | `Arabic_Sup
    | `Armenian
    | `Arrows
    | `Avestan
    | `Balinese
    | `Bamum
    | `Bamum_Sup
    | `Bassa_Vah
    | `Batak
    | `Bengali
    | `Bhaiksuki
    | `Block_Elements
    | `Bopomofo
    | `Bopomofo_Ext
    | `Box_Drawing
    | `Brahmi
    | `Braille
    | `Buginese
    | `Buhid
    | `Byzantine_Music
    | `CJK
    | `CJK_Compat
    | `CJK_Compat_Forms
    | `CJK_Compat_Ideographs
    | `CJK_Compat_Ideographs_Sup
    | `CJK_Ext_A
    | `CJK_Ext_B
    | `CJK_Ext_C
    | `CJK_Ext_D
    | `CJK_Ext_E
    | `CJK_Ext_F
    | `CJK_Ext_G
    | `CJK_Radicals_Sup
    | `CJK_Strokes
    | `CJK_Symbols
    | `Carian
    | `Caucasian_Albanian
    | `Chakma
    | `Cham
    | `Cherokee
    | `Cherokee_Sup
    | `Chess_Symbols
    | `Chorasmian
    | `Compat_Jamo
    | `Control_Pictures
    | `Coptic
    | `Coptic_Epact_Numbers
    | `Counting_Rod
    | `Cuneiform
    | `Cuneiform_Numbers
    | `Currency_Symbols
    | `Cypriot_Syllabary
    | `Cyrillic
    | `Cyrillic_Ext_A
    | `Cyrillic_Ext_B
    | `Cyrillic_Ext_C
    | `Cyrillic_Sup
    | `Deseret
    | `Devanagari
    | `Devanagari_Ext
    | `Diacriticals
    | `Diacriticals_Ext
    | `Diacriticals_For_Symbols
    | `Diacriticals_Sup
    | `Dingbats
    | `Dives_Akuru
    | `Dogra
    | `Domino
    | `Duployan
    | `Early_Dynastic_Cuneiform
    | `Egyptian_Hieroglyph_Format_Controls
    | `Egyptian_Hieroglyphs
    | `Elbasan
    | `Elymaic
    | `Emoticons
    | `Enclosed_Alphanum
    | `Enclosed_Alphanum_Sup
    | `Enclosed_CJK
    | `Enclosed_Ideographic_Sup
    | `Ethiopic
    | `Ethiopic_Ext
    | `Ethiopic_Ext_A
    | `Ethiopic_Sup
    | `Geometric_Shapes
    | `Geometric_Shapes_Ext
    | `Georgian
    | `Georgian_Ext
    | `Georgian_Sup
    | `Glagolitic
    | `Glagolitic_Sup
    | `Gothic
    | `Grantha
    | `Greek
    | `Greek_Ext
    | `Gujarati
    | `Gunjala_Gondi
    | `Gurmukhi
    | `Half_And_Full_Forms
    | `Half_Marks
    | `Hangul
    | `Hanifi_Rohingya
    | `Hanunoo
    | `Hatran
    | `Hebrew
    | `Hiragana
    | `IDC
    | `IPA_Ext
    | `Ideographic_Symbols
    | `Imperial_Aramaic
    | `Indic_Number_Forms
    | `Indic_Siyaq_Numbers
    | `Inscriptional_Pahlavi
    | `Inscriptional_Parthian
    | `Jamo
    | `Jamo_Ext_A
    | `Jamo_Ext_B
    | `Javanese
    | `Kaithi
    | `Kana_Ext_A
    | `Kana_Sup
    | `Kanbun
    | `Kangxi
    | `Kannada
    | `Katakana
    | `Katakana_Ext
    | `Kayah_Li
    | `Kharoshthi
    | `Khitan_Small_Script
    | `Khmer
    | `Khmer_Symbols
    | `Khojki
    | `Khudawadi
    | `Lao
    | `Latin_1_Sup
    | `Latin_Ext_A
    | `Latin_Ext_Additional
    | `Latin_Ext_B
    | `Latin_Ext_C
    | `Latin_Ext_D
    | `Latin_Ext_E
    | `Lepcha
    | `Letterlike_Symbols
    | `Limbu
    | `Linear_A
    | `Linear_B_Ideograms
    | `Linear_B_Syllabary
    | `Lisu
    | `Lisu_Sup
    | `Lycian
    | `Lydian
    | `Mahajani
    | `Mahjong
    | `Makasar
    | `Malayalam
    | `Mandaic
    | `Manichaean
    | `Marchen
    | `Masaram_Gondi
    | `Math_Alphanum
    | `Math_Operators
    | `Mayan_Numerals
    | `Medefaidrin
    | `Meetei_Mayek
    | `Meetei_Mayek_Ext
    | `Mende_Kikakui
    | `Meroitic_Cursive
    | `Meroitic_Hieroglyphs
    | `Miao
    | `Misc_Arrows
    | `Misc_Math_Symbols_A
    | `Misc_Math_Symbols_B
    | `Misc_Pictographs
    | `Misc_Symbols
    | `Misc_Technical
    | `Modi
    | `Modifier_Letters
    | `Modifier_Tone_Letters
    | `Mongolian
    | `Mongolian_Sup
    | `Mro
    | `Multani
    | `Music
    | `Myanmar
    | `Myanmar_Ext_A
    | `Myanmar_Ext_B
    | `NB (** Non_block *)
    | `NKo
    | `Nabataean
    | `Nandinagari
    | `New_Tai_Lue
    | `Newa
    | `Number_Forms
    | `Nushu
    | `Nyiakeng_Puachue_Hmong
    | `OCR
    | `Ogham
    | `Ol_Chiki
    | `Old_Hungarian
    | `Old_Italic
    | `Old_North_Arabian
    | `Old_Permic
    | `Old_Persian
    | `Old_Sogdian
    | `Old_South_Arabian
    | `Old_Turkic
    | `Oriya
    | `Ornamental_Dingbats
    | `Osage
    | `Osmanya
    | `Ottoman_Siyaq_Numbers
    | `PUA
    | `Pahawh_Hmong
    | `Palmyrene
    | `Pau_Cin_Hau
    | `Phags_Pa
    | `Phaistos
    | `Phoenician
    | `Phonetic_Ext
    | `Phonetic_Ext_Sup
    | `Playing_Cards
    | `Psalter_Pahlavi
    | `Punctuation
    | `Rejang
    | `Rumi
    | `Runic
    | `Samaritan
    | `Saurashtra
    | `Sharada
    | `Shavian
    | `Shorthand_Format_Controls
    | `Siddham
    | `Sinhala
    | `Sinhala_Archaic_Numbers
    | `Small_Forms
    | `Small_Kana_Ext
    | `Sogdian
    | `Sora_Sompeng
    | `Soyombo
    | `Specials
    | `Sundanese
    | `Sundanese_Sup
    | `Sup_Arrows_A
    | `Sup_Arrows_B
    | `Sup_Arrows_C
    | `Sup_Math_Operators
    | `Sup_PUA_A
    | `Sup_PUA_B
    | `Sup_Punctuation
    | `Sup_Symbols_And_Pictographs
    | `Super_And_Sub
    | `Sutton_SignWriting
    | `Syloti_Nagri
    | `Symbols_And_Pictographs_Ext_A
    | `Symbols_For_Legacy_Computing
    | `Syriac
    | `Syriac_Sup
    | `Tagalog
    | `Tagbanwa
    | `Tags
    | `Tai_Le
    | `Tai_Tham
    | `Tai_Viet
    | `Tai_Xuan_Jing
    | `Takri
    | `Tamil
    | `Tamil_Sup
    | `Tangut
    | `Tangut_Components
    | `Tangut_Sup
    | `Telugu
    | `Thaana
    | `Thai
    | `Tibetan
    | `Tifinagh
    | `Tirhuta
    | `Transport_And_Map
    | `UCAS
    | `UCAS_Ext
    | `Ugaritic
    | `VS
    | `VS_Sup
    | `Vai
    | `Vedic_Ext
    | `Vertical_Forms
    | `Wancho
    | `Warang_Citi
    | `Yezidi
    | `Yi_Radicals
    | `Yi_Syllables
    | `Yijing
    | `Zanabazar_Square
  ]
  (** The type for blocks. The value [`NB] is for characters that are not
      yet assigned to a block. *)

  val compare : t -> t -> int
  (** [compare b b'] is [Stdlib.compare b b']. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf b] prints an unspecified representation of [b] on [ppf]. *)

  val blocks : (t * (Uchar.t * Uchar.t)) list
  (** [blocks] is the list of blocks sorted by increasing range order.
      Each block appears exactly once in the list except
      [`NB] which is not part of this list as it is not a block. *)

  val block : Uchar.t -> t
  (** [block u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Block}Block} property. *)
end

(** Break properties.

    These properties are mainly for the Unicode text segmentation and line
    breaking algorithm.

    {3 References}
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
module Break : sig

  (** {1:line_break Line break} *)

  type line =
    [ `AI | `AL | `B2 | `BA | `BB | `BK | `CB | `CJ | `CL | `CM | `CP
    | `CR | `EX | `EB | `EM | `GL | `H2 | `H3 | `HL | `HY | `ID | `IN
    | `IS | `JL | `JT | `JV | `LF | `NL | `NS | `NU | `OP | `PO | `PR
    | `QU | `RI | `SA | `SG | `SP | `SY | `WJ | `XX | `ZW | `ZWJ ]
  (** The type for line breaks. *)

  val pp_line : Format.formatter -> line -> unit
  (** [pp_line ppf l] prints an unspecified representation of [l] on
      [ppf]. *)

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
  (** [pp_grapheme_cluster ppf g] prints an unspecified representation of [g]
      on [ppf]. *)

  val word : Uchar.t -> word
  (** [world u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Word_Break}word break}
      property. *)

  (** {1:sentence_break Sentence break} *)

  type sentence =
    [ `AT | `CL | `CR | `EX | `FO | `LE | `LF | `LO | `NU | `SC | `SE | `SP
    | `ST | `UP | `XX ]
  (** The type for sentence breaks. *)

  val pp_sentence : Format.formatter -> sentence -> unit
  (** [pp_grapheme_cluster ppf g] prints an unspecified representation of [g]
      on [ppf]. *)

  val sentence : Uchar.t -> sentence
  (** [sentence u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Sentence_Break}sentence break}
      property. *)

  (** {1:east_asian_width East Asian width} *)

  type east_asian_width = [ `A | `F | `H | `N | `Na | `W ]
  (** The type for East Asian widths. *)

  val pp_east_asian_width : Format.formatter -> east_asian_width -> unit
  (** [pp_east_asian_width ppf w] prints an unspecified representation of
      [w] on [ppf]. *)

  val east_asian_width : Uchar.t -> east_asian_width
  (** [east_asian_width u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#East_Asian_Width}East Asian
      width} property. *)

  (** {1:terminal_width Terminal width} *)

  val tty_width_hint: Uchar.t -> int
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

    (** {1 Low level access to break properties}

        {b Warning.} Do not mutate these array. *)

    val line : Uchar.t -> int
    (** [line u] is an integer that can be used with {!line_of_int}. *)

    val line_max : int
    (** [line_max] is the maximal value returned by {!line}. *)

    val line_of_int : line array
    (** [line_of_int.(i)] is the line break property value corresponding
        to [i]. *)

    val grapheme_cluster : Uchar.t -> int
    (** [grapheme_cluster u] is an integer that can be used with
        {!grapheme_cluster_of_int}. *)

    val grapheme_cluster_max : int
    (** [grapheme_cluster_max] is the maximal value returned by
        {!grapheme_cluster}. *)

    val grapheme_cluster_of_int : grapheme_cluster array
    (** [grapheme_cluster_of_int.(i)] is the grapheme cluster break property
        value corresponding to [i]. *)

    val word : Uchar.t -> int
    (** [word u] is an integer that can be used with {!word_of_int}. *)

    val word_max : int
    (** [word_max] is the maximal value returned by {!word}. *)

    val word_of_int : word array
    (** [word_of_int.(i)] is the word break property value
        corresponding to [i]. *)

    val sentence : Uchar.t -> int
    (** [sentence u] is an integer that can be used with {!sentence_of_int}. *)

    val sentence_max : int
    (** [sentence_max] is the maximal value returned by {!sentence}. *)

    val sentence_of_int : sentence array
    (** [sentence_of_int.(i)] is the sentence break property value
        corresponding to [i]. *)
  end
end

(** Case properties, mappings and foldings.

    These properties can implement Unicode's default case detection,
    case conversion and caseless equality over Unicode text, see the
    {{!caseexamples}examples}.

    {3 References}
    {ul
    {- {{:http://unicode.org/faq/casemap_charprop.html#casemap}
        The Unicode case mapping FAQ.}}
    {- {{:http://www.unicode.org/charts/case/}The Unicode case mapping
       charts.}}} *)
module Case : sig

  (** {1:caseprops Case properties} *)

  val is_lower : Uchar.t -> bool
  (** [is_lower u] is [true] iff [u] has the
      {{:http://www.unicode.org/reports/tr44/#Lowercase}Lowercase} derived
      property. *)

  val is_upper : Uchar.t -> bool
  (** [is_upper u] is [true] iff [u] has the
      {{:http://www.unicode.org/reports/tr44/#Uppercase}Uppercase} derived
      property. *)

  val is_cased : Uchar.t -> bool
  (** [is_cased u] is [true] iff [u] has the
      {{:http://www.unicode.org/reports/tr44/#Cased}Cased} derived property. *)

  val is_case_ignorable : Uchar.t -> bool
  (** [is_case_ignorable] is [true] iff [u] has the
      {{:http://www.unicode.org/reports/tr44/#Case_Ignorable}Case_Ignorable}
      derived property. *)

  (** {1:casemapfold Case mappings and foldings}

      These character mapping functions return [`Self]
      whenever a character maps to itself. *)

  (** Case mappings.  *)
  module Map : sig

    (** {1:casemaps Case mappings} *)

    val to_lower : Uchar.t -> [ `Self | `Uchars of Uchar.t list ]
    (** [to_lower u] is [u]'s
        {{:http://www.unicode.org/reports/tr44/#Lowercase_Mapping}
        Lowercase_Mapping} property. *)

    val to_upper : Uchar.t -> [ `Self | `Uchars of Uchar.t list ]
    (** [to_upper u] is [u]'s
        {{:http://www.unicode.org/reports/tr44/#Uppercase_Mapping}
        Uppercase_Mapping} property. *)

    val to_title : Uchar.t -> [ `Self | `Uchars of Uchar.t list ]
    (** [to_title u] is [u]'s
        {{:http://www.unicode.org/reports/tr44/#Titlecase_Mapping}
        Titlecase_Mapping} property. *)
  end

  (** Case folding. *)
  module Fold : sig

    (** {1:casefolding Case folding} *)

    val fold : Uchar.t -> [ `Self | `Uchars of Uchar.t list ]
    (** [fold u] is [u]'s
        {{:http://www.unicode.org/reports/tr44/#Case_Folding}Case_Folding}
        property. *)
  end

  (** NFKC case folding. *)
  module Nfkc_fold : sig

    (** {1:nfkcfold NFKC Case folding} *)

    val fold : Uchar.t -> [ `Self | `Uchars of Uchar.t list ]
    (** [fold u] is [u]'s
        {{:http://www.unicode.org/reports/tr44/#NFKC_Casefold}NFKC_Casefold}
        property. *)
  end

  (** {1:caseexamples Examples}

      These examples use {!Uutf} to fold over the characters of UTF-8
      encoded OCaml strings and to UTF-8 encode mapped characters in
      an OCaml {!Buffer.t} value.

      {2:caseconversion Default case conversion on UTF-8 strings}

      The value [casemap_utf_8 cmap s] is the UTF-8 encoded string
      resulting from applying the character map [cmap] to every character
      of the UTF-8 encoded string [s].
{[
let cmap_utf_8 cmap s =
  let b = Buffer.create (String.length s * 2) in
  let rec add_map _ _ u =
    let u = match u with `Malformed _ -> Uutf.u_rep | `Uchar u -> u in
    match cmap u with
    | `Self -> Uutf.Buffer.add_utf_8 b u
    | `Uchars us -> List.iter (Uutf.Buffer.add_utf_8 b) us
  in
  Uutf.String.fold_utf_8 add_map () s; Buffer.contents b
]}
      Using the function [cmap_utf_8], Unicode's default case
      conversions can be implemented with:
{[
let lowercase_utf_8 s = cmap_utf_8 Uucp.Case.Map.to_lower s
let uppercase_utf_8 s = cmap_utf_8 Uucp.Case.Map.to_upper s
]}
      However strictly speaking [lowercase_utf_8] is not conformant
      as it doesn't handle the context sensitive mapping of capital
      sigma U+03A3 to final sigma U+03C2.

      Note that applying Unicode's default case algorithms to a normalized
      string does not preserve its normalization form.

      {2:caselesseq Default caseless matching (equality) on UTF-8 strings}

      These examples use {!Uunf} to normalize character sequences

      Unicode canonical caseless matching (D145) is defined by
      normalizing to NFD, applying the Case_Folding mapping, normalizing
      again to NFD and test the result for binary equality:

{[
let canonical_caseless_key s =
  let b = Buffer.create (String.length s * 2) in
  let to_nfd_and_utf_8 =
    let n = Uunf.create `NFD in
    let rec add v = match Uunf.add n v with
    | `Await | `End -> ()
    | `Uchar u -> Uutf.Buffer.add_utf_8 b u; add `Await
    in
    add
  in
  let add =
    let n = Uunf.create `NFD in
    let rec add v = match Uunf.add n v with
    | `Await | `End -> ()
    | `Uchar u ->
        begin match Uucp.Case.Fold.fold u with
        | `Self -> to_nfd_and_utf_8 (`Uchar u)
        | `Uchars us -> List.iter (fun u -> to_nfd_and_utf_8 (`Uchar u)) us
        end;
        add `Await
    in
    add
  in
  let add_uchar _ _ = function
  | `Malformed  _ -> add (`Uchar Uutf.u_rep)
  | `Uchar _ as u -> add u
  in
  Uutf.String.fold_utf_8 add_uchar () s;
  add `End;
  to_nfd_and_utf_8 `End;
  Buffer.contents b

let canonical_caseless_eq s0 s1 =
  canonical_caseless_key s0 = canonical_caseless_key s1
]}
      Unicode's caseless matching for identifiers (D147, see also
      {{:http://www.unicode.org/reports/tr31/}UAX 31}) is defined
      by normalizing to NFD, applying the NFKC_Casefold mapping and test
      the result for binary equality:
{[
let id_caseless_key s =
  let b = Buffer.create (String.length s * 3) in
  let n = Uunf.create `NFD in
  let rec add v = match Uunf.add n v with
  | `Await | `End -> ()
  | `Uchar u ->
      begin match Uucp.Case.Nfkc_fold.fold u with
      | `Self -> Uutf.Buffer.add_utf_8 b u; add `Await
      | `Uchars us -> List.iter (Uutf.Buffer.add_utf_8 b) us; add `Await
      end
  in
  let add_uchar _ _ = function
  | `Malformed  _ -> add (`Uchar Uutf.u_rep)
  | `Uchar _ as u -> add u
  in
  Uutf.String.fold_utf_8 add_uchar () s;
  add `End;
  Buffer.contents b

let id_caseless_eq s0 s1 = id_caseless_key s0 = id_caseless_key s1
]}
*)
end

(** CJK properties.

    {3 References}
    {ul
    {- {{:http://www.unicode.org/faq/han_cjk.html}
    The Unicode Chinese and Japanese FAQ.}}
    {- {{:http://www.unicode.org/faq/korean.html}
    The Unicode Korean FAQ.}}} *)
module Cjk : sig

  (**  {1:cjkprops CJK properties} *)

  val is_ideographic : Uchar.t -> bool
  (** [is_ideographic u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Ideographic}Ideographic}
      property. *)

  val is_ids_bin_op : Uchar.t -> bool
  (** [is_ids_bin_op u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#IDS_Binary_Operator}
      IDS_Binary_Operator} property. *)

  val is_ids_tri_op : Uchar.t -> bool
  (** [is_ids_tri_op u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#IDS_Trinary_Operator}
      IDS_Trinary_Operator} property. *)

  val is_radical : Uchar.t -> bool
  (** [is_radical u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Radical}Radical}
      property. *)

  val is_unified_ideograph : Uchar.t -> bool
  (** [is_unified_ideograph u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Unified_Ideograph}
      Unified_Ideograph} property. *)
end

(** Emoji properties. *)
module Emoji : sig

  val is_emoji : Uchar.t -> bool
  (** [is_emoji u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Emoji}Emoji} property. *)

  val is_emoji_presentation : Uchar.t -> bool
  (** [is_emoji_presentation u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Emoji_Presentation}
      Emoji_Presentation} property. *)

  val is_emoji_modifier : Uchar.t -> bool
  (** [is_emoji_modifier u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Emoji_Modifier}
      Emoji_Modifier} property. *)

  val is_emoji_modifier_base : Uchar.t -> bool
  (** [is_emoji_modifier u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Emoji_Modifier_Base}
      Emoji_Modifier_Base} property. *)

  val is_emoji_component : Uchar.t -> bool
  (** [is_emoji_component u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Emoji_Component}
      Emoji_Component} property. *)

  val is_extended_pictographic : Uchar.t -> bool
  (** [is_extended_pictographic u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Extended_Pictographic}
      Extended_Pictographic} property. *)
end

(** Function and graphics properties. *)
module Func : sig

  (** {1:funcprops Function and graphics properties} *)

  val is_dash : Uchar.t -> bool
  (** [is_dash u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Dash}Dash}
      property. *)

  val is_diacritic : Uchar.t -> bool
  (** [is_diacritic u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Diacritic}Diacritic}
      property. *)

  val is_extender : Uchar.t -> bool
  (** [is_extender u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Extender}Extender}
      property. *)

  val is_grapheme_base : Uchar.t -> bool
  (** [is_grapheme_base u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Grapheme_Base}Grapheme_Base}
      property. *)

  val is_grapheme_extend : Uchar.t -> bool
  (** [is_grapheme_extend u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Grapheme_Extend}Grapheme_Extend}
      property. *)

  val is_math : Uchar.t -> bool
  (** [is_math u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Math}Math}
      property. *)

  val is_quotation_mark : Uchar.t -> bool
  (** [is_quotation_mark u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Quotation_Mark}Quotation_Mark}
      property. *)

  val is_soft_dotted : Uchar.t -> bool
  (** [is_soft_dotted u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Soft_Dotted}Soft_Dotted}
      property. *)

  val is_terminal_punctuation : Uchar.t -> bool
  (** [is_terminal_punctuation u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Terminal_Punctuation}
      Terminal_Punctuation} property. *)

  val is_regional_indicator : Uchar.t -> bool
  (** [is_regional_indicator u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Regional_Indicator}
      Regional_indicator} property. *)

  val is_join_control : Uchar.t -> bool
  (** [is_join_control u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Join_Control}Join_Control}
      property. *)
end

(** General category property. *)
module Gc : sig

  (** {1:gcprop General category property} *)

  type t =
    [ `Cc | `Cf | `Cn | `Co | `Cs | `Ll | `Lm | `Lo | `Lt | `Lu | `Mc
    | `Me | `Mn | `Nd | `Nl | `No | `Pc | `Pd | `Pe | `Pf | `Pi | `Po
    | `Ps | `Sc | `Sk | `Sm | `So | `Zl | `Zp | `Zs ]
  (** The type for general categories. *)

  val compare : t -> t -> int
  (** [compare c c'] is [Stdlib.compare s s']. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf c] prints an unspecified representation of [c] on [ppf]. *)

  val general_category : Uchar.t -> t
  (** [general_category u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#General_Category}
      General_Category} property. *)
end

(** General properties. *)
module Gen : sig

  (** {1:genprops General properties} *)

  val is_default_ignorable : Uchar.t -> bool
  (** [is_default_ignorable u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Default_Ignorable_Code_Point}
       Default_Ignorable_Code_Point} property. *)

  val is_deprecated : Uchar.t -> bool
  (** [is_deprecated u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Deprecated}
       Deprecated} property. *)

  val is_logical_order_exception : Uchar.t -> bool
  (** [is_logical_order_exception u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Logical_Order_Exception}
      Logical_Order_Exception} property. *)

  val is_non_character : Uchar.t -> bool
  (** [is_non_character u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Noncharacter_Code_Point}
      Noncharacter_Code_Point} property. *)

  val is_variation_selector : Uchar.t -> bool
  (** [is_variation_selector u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Variation_Selector}
      Variation_Selector} property. See the
      {{:http://www.unicode.org/faq/vs.html}Variation Sequences FAQ}. *)
end

(** Hangul properties. *)
module Hangul : sig

  (** {1:hangul_syllable_type Hangul syllable type property} *)

  type syllable_type = [ `L | `V | `T | `LV | `LVT | `NA ]
  (** The type for hangul syllable types. *)

  val pp_syllable_type : Format.formatter -> syllable_type -> unit
  (** [pp_syllable_type ppf s] prints an unspecified representation of
      [s] on [ppf]. *)

  val syllable_type : Uchar.t -> syllable_type
  (** [syllable_type u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Hangul_Syllable_Type}
      Hangul_Syllable_type} property. *)
end

(** Identifier properties.

    {3 References}
    {ul
    {- Mark Davis.
    {e {{:http://www.unicode.org/reports/tr31/}UAX #31
       Unicode Identifier and Pattern Syntax}}. (latest version)}} *)
module Id : sig

  (** {1:idprops Identifier properties} *)

  val is_id_start : Uchar.t -> bool
  (** [is_id_start u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#ID_Start}ID_Start}
      property. *)

  val is_id_continue : Uchar.t -> bool
  (** [is_id_continue u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#ID_Continue}ID_Continue}
      property. *)

  val is_xid_start : Uchar.t -> bool
  (** [is_xid_start u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#XID_Start}XID_Start}
      property. *)

  val is_xid_continue : Uchar.t -> bool
  (** [is_xid_continue u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#XID_Continue}XID_Continue}
      property. *)

  (** {1:patprops Pattern syntax properties} *)

  val is_pattern_syntax : Uchar.t -> bool
  (** [is_pattern_syntax u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Pattern_Syntax}Pattern_Syntax}
      property. *)


  val is_pattern_white_space : Uchar.t -> bool
  (** [is_pattern_white_space u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Pattern_White_Space}
      Pattern_White_Space} property. *)
end

(** Name and name alias properties.

    {3 References}
    {ul
    {- {{:http://unicode.org/faq/casemap_charprop.html#nameprop}
    The Unicode names FAQ}.}
    {- The Unicode consortium.
       {{:https://unicode.org/charts/nameslist/index.html}
       The Unicode names charts}}} *)
module Name : sig

  (** {1:nameprop Names} *)

  val name : Uchar.t -> string
  (** [name u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Name}Name} property. *)

  (** {1:namealiasprop Name aliases} *)

  type alias_tag =
    [ `Abbreviation | `Alternate | `Control | `Correction | `Figment ]

  val pp_alias_tag : Format.formatter -> alias_tag -> unit
  (** [pp_alias_tag t] prints an unspecified representation of [t]
      on [ppf]. *)

  val name_alias : Uchar.t -> (alias_tag * string) list
  (** [name_alias u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Name_Alias}Name_Alias}
      property. *)
end

(** Numeric properties. *)
module Num : sig

  (** {1:hexprop Hex digits} *)

  val is_ascii_hex_digit : Uchar.t -> bool
  (** [is_ascii_hex_digit u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#ASCII_Hex_Digit}ASCII_Hex_Digit}
      property. *)

  val is_hex_digit : Uchar.t -> bool
  (** [is_ascii_hex_digit u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Hex_Digit}Hex_Digit}
      property. *)

  (** {1:numtypeprop Numeric type} *)

  type numeric_type = [ `De | `Di | `None | `Nu ]
  (** The type for numeric types. *)

  val pp_numeric_type : Format.formatter -> numeric_type -> unit
  (** [pp_numeric_type ppf n] prints an unspecified representation of
      [n] on [ppf]. *)

  val numeric_type : Uchar.t -> numeric_type
  (** [numeric_type u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Numeric_Type}
      Numeric_Type} property. *)

  (** {1:numvalueprop Numeric value} *)

  type numeric_value = [ `Frac of int * int | `NaN | `Num of int64 ]
  (** The type for numeric values. *)

  val pp_numeric_value : Format.formatter -> numeric_value -> unit
  (** [pp_numeric_value ppf n] prints an unspecified representation of
      [n] on [ppf]. *)

  val numeric_value : Uchar.t -> [ `Frac of int * int | `NaN | `Num of int64 ]
  (** [numeric_type u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Numeric_Value}
      Numeric_Value} property. *)
end

(** Script and script extensions properties.

    {3 References}
    {ul
    {- Mark Davis, Ken Whistler.
    {{:http://www.unicode.org/reports/tr24/}{e Unicode script property}}.
    (latest version)}
    {- {{:http://www.unicode.org/charts/script/index.html}The Unicode script
    charts}.}} *)
module Script : sig

  (** {1:scriptprop Script} *)

  type t = [
  | `Adlm
  | `Aghb
  | `Ahom
  | `Arab
  | `Armi
  | `Armn
  | `Avst
  | `Bali
  | `Bamu
  | `Bass
  | `Batk
  | `Beng
  | `Bhks
  | `Bopo
  | `Brah
  | `Brai
  | `Bugi
  | `Buhd
  | `Cakm
  | `Cans
  | `Cari
  | `Cham
  | `Cher
  | `Chrs
  | `Copt
  | `Cprt
  | `Cyrl
  | `Deva
  | `Diak
  | `Dogr
  | `Dsrt
  | `Dupl
  | `Egyp
  | `Elba
  | `Elym
  | `Ethi
  | `Geor
  | `Glag
  | `Gong
  | `Gonm
  | `Goth
  | `Gran
  | `Grek
  | `Gujr
  | `Guru
  | `Hang
  | `Hani
  | `Hano
  | `Hatr
  | `Hebr
  | `Hira
  | `Hluw
  | `Hmng
  | `Hmnp
  | `Hrkt
  | `Hung
  | `Ital
  | `Java
  | `Kali
  | `Kana
  | `Khar
  | `Khmr
  | `Khoj
  | `Knda
  | `Kthi
  | `Kits
  | `Lana
  | `Laoo
  | `Latn
  | `Lepc
  | `Limb
  | `Lina
  | `Linb
  | `Lisu
  | `Lyci
  | `Lydi
  | `Mahj
  | `Maka
  | `Mand
  | `Mani
  | `Marc
  | `Medf
  | `Mend
  | `Merc
  | `Mero
  | `Mlym
  | `Modi
  | `Mong
  | `Mroo
  | `Mtei
  | `Mult
  | `Mymr
  | `Nand
  | `Narb
  | `Nbat
  | `Newa
  | `Nkoo
  | `Nshu
  | `Ogam
  | `Olck
  | `Orkh
  | `Orya
  | `Osge
  | `Osma
  | `Palm
  | `Pauc
  | `Perm
  | `Phag
  | `Phli
  | `Phlp
  | `Phnx
  | `Plrd
  | `Prti
  | `Qaai
  | `Rjng
  | `Rohg
  | `Runr
  | `Samr
  | `Sarb
  | `Saur
  | `Sgnw
  | `Shaw
  | `Shrd
  | `Sidd
  | `Sind
  | `Sinh
  | `Sogd
  | `Sogo
  | `Sora
  | `Soyo
  | `Sund
  | `Sylo
  | `Syrc
  | `Tagb
  | `Takr
  | `Tale
  | `Talu
  | `Taml
  | `Tang
  | `Tavt
  | `Telu
  | `Tfng
  | `Tglg
  | `Thaa
  | `Thai
  | `Tibt
  | `Tirh
  | `Ugar
  | `Vaii
  | `Wara
  | `Wcho
  | `Xpeo
  | `Xsux
  | `Yezi
  | `Yiii
  | `Zanb
  | `Zinh
  | `Zyyy
  | `Zzzz
  ]
  (** The type for scripts. *)

  val compare : t -> t -> int
  (** [compare s s'] is [Stdlib.compare s s']. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf s] prints an unspecified representation of [s] on [ppf]. *)

  val script : Uchar.t -> t
  (** [script u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Script}Script} property. *)

  val script_extensions : Uchar.t -> t list
  (** [script_extension u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Script_Extensions}
      Script_Extensions} property. The list is never empty. *)
end

(** White space property. *)
module White : sig

  (**  {1:whiteprop White space property} *)

  val is_white_space : Uchar.t -> bool
  (** [is_white_space u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#White_Space}White_Space}
      property. *)
end

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
       Changes_When_Casemapped}.}
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
       {{:http://www.unicode.org/reports/tr44/#Prepended_Concatenation_Mark}Prepended_Concatenation_Mark}}
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

(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

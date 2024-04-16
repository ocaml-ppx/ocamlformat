(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

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
  | `Arabic_Ext_B
  | `Arabic_Ext_C
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
  | `CJK_Ext_H
  | `CJK_Ext_I
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
  | `Cypro_Minoan
  | `Cyrillic
  | `Cyrillic_Ext_A
  | `Cyrillic_Ext_B
  | `Cyrillic_Ext_C
  | `Cyrillic_Ext_D
  | `Cyrillic_Sup
  | `Deseret
  | `Devanagari
  | `Devanagari_Ext
  | `Devanagari_Ext_A
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
  | `Ethiopic_Ext_B
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
  | `Kaktovik_Numerals
  | `Kana_Ext_A
  | `Kana_Ext_B
  | `Kawi
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
  | `Latin_Ext_F
  | `Latin_Ext_G
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
  | `Nag_Mundari
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
  | `Old_Uyghur
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
  | `Tangsa
  | `Tangut
  | `Tangut_Components
  | `Tangut_Sup
  | `Telugu
  | `Thaana
  | `Thai
  | `Tibetan
  | `Tifinagh
  | `Tirhuta
  | `Toto
  | `Transport_And_Map
  | `UCAS
  | `UCAS_Ext
  | `UCAS_Ext_A
  | `Ugaritic
  | `VS
  | `VS_Sup
  | `Vai
  | `Vedic_Ext
  | `Vertical_Forms
  | `Vithkuqi
  | `Wancho
  | `Warang_Citi
  | `Yezidi
  | `Yi_Radicals
  | `Yi_Syllables
  | `Yijing
  | `Zanabazar_Square
  | `Znamenny_Music
]

let pp ppf b = Format.fprintf ppf "%s" begin match b with
  | `ASCII -> "ASCII"
  | `Adlam -> "Adlam"
  | `Aegean_Numbers -> "Aegean_Numbers"
  | `Ahom -> "Ahom"
  | `Alchemical -> "Alchemical"
  | `Alphabetic_PF -> "Alphabetic_PF"
  | `Anatolian_Hieroglyphs -> "Anatolian_Hieroglyphs"
  | `Ancient_Greek_Music -> "Ancient_Greek_Music"
  | `Ancient_Greek_Numbers -> "Ancient_Greek_Numbers"
  | `Ancient_Symbols -> "Ancient_Symbols"
  | `Arabic -> "Arabic"
  | `Arabic_Ext_A -> "Arabic_Ext_A"
  | `Arabic_Ext_B -> "Arabic_Ext_B"
  | `Arabic_Ext_C -> "Arabic_Ext_C"
  | `Arabic_Math -> "Arabic_Math"
  | `Arabic_PF_A -> "Arabic_PF_A"
  | `Arabic_PF_B -> "Arabic_PF_B"
  | `Arabic_Sup -> "Arabic_Sup"
  | `Armenian -> "Armenian"
  | `Arrows -> "Arrows"
  | `Avestan -> "Avestan"
  | `Balinese -> "Balinese"
  | `Bamum -> "Bamum"
  | `Bamum_Sup -> "Bamum_Sup"
  | `Bassa_Vah -> "Bassa_Vah"
  | `Batak -> "Batak"
  | `Bengali -> "Bengali"
  | `Bhaiksuki -> "Bhaiksuki"
  | `Block_Elements -> "Block_Elements"
  | `Bopomofo -> "Bopomofo"
  | `Bopomofo_Ext -> "Bopomofo_Ext"
  | `Box_Drawing -> "Box_Drawing"
  | `Brahmi -> "Brahmi"
  | `Braille -> "Braille"
  | `Buginese -> "Buginese"
  | `Buhid -> "Buhid"
  | `Byzantine_Music -> "Byzantine_Music"
  | `CJK -> "CJK"
  | `CJK_Compat -> "CJK_Compat"
  | `CJK_Compat_Forms -> "CJK_Compat_Forms"
  | `CJK_Compat_Ideographs -> "CJK_Compat_Ideographs"
  | `CJK_Compat_Ideographs_Sup -> "CJK_Compat_Ideographs_Sup"
  | `CJK_Ext_A -> "CJK_Ext_A"
  | `CJK_Ext_B -> "CJK_Ext_B"
  | `CJK_Ext_C -> "CJK_Ext_C"
  | `CJK_Ext_D -> "CJK_Ext_D"
  | `CJK_Ext_E -> "CJK_Ext_E"
  | `CJK_Ext_F -> "CJK_Ext_F"
  | `CJK_Ext_G -> "CJK_Ext_G"
  | `CJK_Ext_H -> "CJK_Ext_H"
  | `CJK_Ext_I -> "CJK_Ext_I"
  | `CJK_Radicals_Sup -> "CJK_Radicals_Sup"
  | `CJK_Strokes -> "CJK_Strokes"
  | `CJK_Symbols -> "CJK_Symbols"
  | `Carian -> "Carian"
  | `Caucasian_Albanian -> "Caucasian_Albanian"
  | `Chakma -> "Chakma"
  | `Cham -> "Cham"
  | `Cherokee -> "Cherokee"
  | `Cherokee_Sup -> "Cherokee_Sup"
  | `Chess_Symbols -> "Chess_Symbols"
  | `Chorasmian -> "Chorasmian"
  | `Compat_Jamo -> "Compat_Jamo"
  | `Control_Pictures -> "Control_Pictures"
  | `Coptic -> "Coptic"
  | `Coptic_Epact_Numbers -> "Coptic_Epact_Numbers"
  | `Counting_Rod -> "Counting_Rod"
  | `Cuneiform -> "Cuneiform"
  | `Cuneiform_Numbers -> "Cuneiform_Numbers"
  | `Currency_Symbols -> "Currency_Symbols"
  | `Cypriot_Syllabary -> "Cypriot_Syllabary"
  | `Cypro_Minoan -> "Cypro_Minoan"
  | `Cyrillic -> "Cyrillic"
  | `Cyrillic_Ext_A -> "Cyrillic_Ext_A"
  | `Cyrillic_Ext_B -> "Cyrillic_Ext_B"
  | `Cyrillic_Ext_C -> "Cyrillic_Ext_C"
  | `Cyrillic_Ext_D -> "Cyrillic_Ext_D"
  | `Cyrillic_Sup -> "Cyrillic_Sup"
  | `Deseret -> "Deseret"
  | `Devanagari -> "Devanagari"
  | `Devanagari_Ext -> "Devanagari_Ext"
  | `Devanagari_Ext_A -> "Devanagari_Ext_A"
  | `Diacriticals -> "Diacriticals"
  | `Diacriticals_Ext -> "Diacriticals_Ext"
  | `Diacriticals_For_Symbols -> "Diacriticals_For_Symbols"
  | `Diacriticals_Sup -> "Diacriticals_Sup"
  | `Dingbats -> "Dingbats"
  | `Dives_Akuru -> "Dives_Akuru"
  | `Dogra -> "Dogra"
  | `Domino -> "Domino"
  | `Duployan -> "Duployan"
  | `Early_Dynastic_Cuneiform -> "Early_Dynastic_Cuneiform"
  | `Egyptian_Hieroglyph_Format_Controls -> "Egyptian_Hieroglyph_Format_Controls"
  | `Egyptian_Hieroglyphs -> "Egyptian_Hieroglyphs"
  | `Elbasan -> "Elbasan"
  | `Elymaic -> "Elymaic"
  | `Emoticons -> "Emoticons"
  | `Enclosed_Alphanum -> "Enclosed_Alphanum"
  | `Enclosed_Alphanum_Sup -> "Enclosed_Alphanum_Sup"
  | `Enclosed_CJK -> "Enclosed_CJK"
  | `Enclosed_Ideographic_Sup -> "Enclosed_Ideographic_Sup"
  | `Ethiopic -> "Ethiopic"
  | `Ethiopic_Ext -> "Ethiopic_Ext"
  | `Ethiopic_Ext_A -> "Ethiopic_Ext_A"
  | `Ethiopic_Ext_B -> "Ethiopic_Ext_B"
  | `Ethiopic_Sup -> "Ethiopic_Sup"
  | `Geometric_Shapes -> "Geometric_Shapes"
  | `Geometric_Shapes_Ext -> "Geometric_Shapes_Ext"
  | `Georgian -> "Georgian"
  | `Georgian_Ext -> "Georgian_Ext"
  | `Georgian_Sup -> "Georgian_Sup"
  | `Glagolitic -> "Glagolitic"
  | `Glagolitic_Sup -> "Glagolitic_Sup"
  | `Gothic -> "Gothic"
  | `Grantha -> "Grantha"
  | `Greek -> "Greek"
  | `Greek_Ext -> "Greek_Ext"
  | `Gujarati -> "Gujarati"
  | `Gunjala_Gondi -> "Gunjala_Gondi"
  | `Gurmukhi -> "Gurmukhi"
  | `Half_And_Full_Forms -> "Half_And_Full_Forms"
  | `Half_Marks -> "Half_Marks"
  | `Hangul -> "Hangul"
  | `Hanifi_Rohingya -> "Hanifi_Rohingya"
  | `Hanunoo -> "Hanunoo"
  | `Hatran -> "Hatran"
  | `Hebrew -> "Hebrew"
  | `High_PU_Surrogates -> "High_PU_Surrogates"
  | `High_Surrogates -> "High_Surrogates"
  | `Hiragana -> "Hiragana"
  | `IDC -> "IDC"
  | `IPA_Ext -> "IPA_Ext"
  | `Ideographic_Symbols -> "Ideographic_Symbols"
  | `Imperial_Aramaic -> "Imperial_Aramaic"
  | `Indic_Number_Forms -> "Indic_Number_Forms"
  | `Indic_Siyaq_Numbers -> "Indic_Siyaq_Numbers"
  | `Inscriptional_Pahlavi -> "Inscriptional_Pahlavi"
  | `Inscriptional_Parthian -> "Inscriptional_Parthian"
  | `Jamo -> "Jamo"
  | `Jamo_Ext_A -> "Jamo_Ext_A"
  | `Jamo_Ext_B -> "Jamo_Ext_B"
  | `Javanese -> "Javanese"
  | `Kaithi -> "Kaithi"
  | `Kaktovik_Numerals -> "Kaktovik_Numerals"
  | `Kana_Ext_A -> "Kana_Ext_A"
  | `Kana_Ext_B -> "Kana_Ext_B"
  | `Kawi -> "Kawi"
  | `Kana_Sup -> "Kana_Sup"
  | `Kanbun -> "Kanbun"
  | `Kangxi -> "Kangxi"
  | `Kannada -> "Kannada"
  | `Katakana -> "Katakana"
  | `Katakana_Ext -> "Katakana_Ext"
  | `Kayah_Li -> "Kayah_Li"
  | `Kharoshthi -> "Kharoshthi"
  | `Khitan_Small_Script -> "Khitan_Small_Script"
  | `Khmer -> "Khmer"
  | `Khmer_Symbols -> "Khmer_Symbols"
  | `Khojki -> "Khojki"
  | `Khudawadi -> "Khudawadi"
  | `Lao -> "Lao"
  | `Latin_1_Sup -> "Latin_1_Sup"
  | `Latin_Ext_A -> "Latin_Ext_A"
  | `Latin_Ext_Additional -> "Latin_Ext_Additional"
  | `Latin_Ext_B -> "Latin_Ext_B"
  | `Latin_Ext_C -> "Latin_Ext_C"
  | `Latin_Ext_D -> "Latin_Ext_D"
  | `Latin_Ext_E -> "Latin_Ext_E"
  | `Latin_Ext_F -> "Latin_Ext_F"
  | `Latin_Ext_G -> "Latin_Ext_G"
  | `Lepcha -> "Lepcha"
  | `Letterlike_Symbols -> "Letterlike_Symbols"
  | `Limbu -> "Limbu"
  | `Linear_A -> "Linear_A"
  | `Linear_B_Ideograms -> "Linear_B_Ideograms"
  | `Linear_B_Syllabary -> "Linear_B_Syllabary"
  | `Lisu -> "Lisu"
  | `Lisu_Sup -> "Lisu_Sup"
  | `Low_Surrogates -> "Low_Surrogates"
  | `Lycian -> "Lycian"
  | `Lydian -> "Lydian"
  | `Mahajani -> "Mahajani"
  | `Mahjong -> "Mahjong"
  | `Makasar -> "Makasar"
  | `Malayalam -> "Malayalam"
  | `Mandaic -> "Mandaic"
  | `Manichaean -> "Manichaean"
  | `Marchen -> "Marchen"
  | `Masaram_Gondi -> "Masaram_Gondi"
  | `Math_Alphanum -> "Math_Alphanum"
  | `Math_Operators -> "Math_Operators"
  | `Mayan_Numerals -> "Mayan_Numerals"
  | `Medefaidrin -> "Medefaidrin"
  | `Meetei_Mayek -> "Meetei_Mayek"
  | `Meetei_Mayek_Ext -> "Meetei_Mayek_Ext"
  | `Mende_Kikakui -> "Mende_Kikakui"
  | `Meroitic_Cursive -> "Meroitic_Cursive"
  | `Meroitic_Hieroglyphs -> "Meroitic_Hieroglyphs"
  | `Miao -> "Miao"
  | `Misc_Arrows -> "Misc_Arrows"
  | `Misc_Math_Symbols_A -> "Misc_Math_Symbols_A"
  | `Misc_Math_Symbols_B -> "Misc_Math_Symbols_B"
  | `Misc_Pictographs -> "Misc_Pictographs"
  | `Misc_Symbols -> "Misc_Symbols"
  | `Misc_Technical -> "Misc_Technical"
  | `Modi -> "Modi"
  | `Modifier_Letters -> "Modifier_Letters"
  | `Modifier_Tone_Letters -> "Modifier_Tone_Letters"
  | `Mongolian -> "Mongolian"
  | `Mongolian_Sup -> "Mongolian_Sup"
  | `Mro -> "Mro"
  | `Multani -> "Multani"
  | `Music -> "Music"
  | `Myanmar -> "Myanmar"
  | `Myanmar_Ext_A -> "Myanmar_Ext_A"
  | `Myanmar_Ext_B -> "Myanmar_Ext_B"
  | `NB -> "NB"
  | `NKo -> "NKo"
  | `Nabataean -> "Nabataean"
  | `Nag_Mundari -> "Nag_Mundari"
  | `Nandinagari -> "Nandinagari"
  | `New_Tai_Lue -> "New_Tai_Lue"
  | `Newa -> "Newa"
  | `Number_Forms -> "Number_Forms"
  | `Nushu -> "Nushu"
  | `Nyiakeng_Puachue_Hmong -> "Nyiakeng_Puachue_Hmong"
  | `OCR -> "OCR"
  | `Ogham -> "Ogham"
  | `Ol_Chiki -> "Ol_Chiki"
  | `Old_Hungarian -> "Old_Hungarian"
  | `Old_Italic -> "Old_Italic"
  | `Old_North_Arabian -> "Old_North_Arabian"
  | `Old_Permic -> "Old_Permic"
  | `Old_Persian -> "Old_Persian"
  | `Old_Sogdian -> "Old_Sogdian"
  | `Old_South_Arabian -> "Old_South_Arabian"
  | `Old_Turkic -> "Old_Turkic"
  | `Old_Uyghur -> "Old_Uyghur"
  | `Oriya -> "Oriya"
  | `Ornamental_Dingbats -> "Ornamental_Dingbats"
  | `Osage -> "Osage"
  | `Osmanya -> "Osmanya"
  | `Ottoman_Siyaq_Numbers -> "Ottoman_Siyaq_Numbers"
  | `PUA -> "PUA"
  | `Pahawh_Hmong -> "Pahawh_Hmong"
  | `Palmyrene -> "Palmyrene"
  | `Pau_Cin_Hau -> "Pau_Cin_Hau"
  | `Phags_Pa -> "Phags_Pa"
  | `Phaistos -> "Phaistos"
  | `Phoenician -> "Phoenician"
  | `Phonetic_Ext -> "Phonetic_Ext"
  | `Phonetic_Ext_Sup -> "Phonetic_Ext_Sup"
  | `Playing_Cards -> "Playing_Cards"
  | `Psalter_Pahlavi -> "Psalter_Pahlavi"
  | `Punctuation -> "Punctuation"
  | `Rejang -> "Rejang"
  | `Rumi -> "Rumi"
  | `Runic -> "Runic"
  | `Samaritan -> "Samaritan"
  | `Saurashtra -> "Saurashtra"
  | `Sharada -> "Sharada"
  | `Shavian -> "Shavian"
  | `Shorthand_Format_Controls -> "Shorthand_Format_Controls"
  | `Siddham -> "Siddham"
  | `Sinhala -> "Sinhala"
  | `Sinhala_Archaic_Numbers -> "Sinhala_Archaic_Numbers"
  | `Small_Forms -> "Small_Forms"
  | `Small_Kana_Ext -> "Small_Kana_Ext"
  | `Sogdian -> "Sogdian"
  | `Sora_Sompeng -> "Sora_Sompeng"
  | `Soyombo -> "Soyombo"
  | `Specials -> "Specials"
  | `Sundanese -> "Sundanese"
  | `Sundanese_Sup -> "Sundanese_Sup"
  | `Sup_Arrows_A -> "Sup_Arrows_A"
  | `Sup_Arrows_B -> "Sup_Arrows_B"
  | `Sup_Arrows_C -> "Sup_Arrows_C"
  | `Sup_Math_Operators -> "Sup_Math_Operators"
  | `Sup_PUA_A -> "Sup_PUA_A"
  | `Sup_PUA_B -> "Sup_PUA_B"
  | `Sup_Punctuation -> "Sup_Punctuation"
  | `Sup_Symbols_And_Pictographs -> "Sup_Symbols_And_Pictographs"
  | `Symbols_For_Legacy_Computing -> "Symbols_For_Legacy_Computing"
  | `Super_And_Sub -> "Super_And_Sub"
  | `Sutton_SignWriting -> "Sutton_SignWriting"
  | `Syloti_Nagri -> "Syloti_Nagri"
  | `Symbols_And_Pictographs_Ext_A -> "Symbols_And_Pictographs_Ext_A"
  | `Syriac -> "Syriac"
  | `Syriac_Sup -> "Syriac_Sup"
  | `Tagalog -> "Tagalog"
  | `Tagbanwa -> "Tagbanwa"
  | `Tags -> "Tags"
  | `Tai_Le -> "Tai_Le"
  | `Tai_Tham -> "Tai_Tham"
  | `Tai_Viet -> "Tai_Viet"
  | `Tai_Xuan_Jing -> "Tai_Xuan_Jing"
  | `Takri -> "Takri"
  | `Tamil -> "Tamil"
  | `Tamil_Sup -> "Tamil_Sup"
  | `Tangsa -> "Tangsa"
  | `Tangut -> "Tangut"
  | `Tangut_Components -> "Tangut_Components"
  | `Tangut_Sup -> "Tangut_Sup"
  | `Telugu -> "Telugu"
  | `Thaana -> "Thaana"
  | `Thai -> "Thai"
  | `Tibetan -> "Tibetan"
  | `Tifinagh -> "Tifinagh"
  | `Tirhuta -> "Tirhuta"
  | `Toto -> "Toto"
  | `Transport_And_Map -> "Transport_And_Map"
  | `UCAS -> "UCAS"
  | `UCAS_Ext -> "UCAS_Ext"
  | `UCAS_Ext_A -> "UCAS_Ext_A"
  | `Ugaritic -> "Ugaritic"
  | `VS -> "VS"
  | `VS_Sup -> "VS_Sup"
  | `Vai -> "Vai"
  | `Vedic_Ext -> "Vedic_Ext"
  | `Vertical_Forms -> "Vertical_Forms"
  | `Vithkuqi -> "Vithkuqi"
  | `Wancho -> "Wancho"
  | `Warang_Citi -> "Warang_Citi"
  | `Yezidi -> "Yezidi"
  | `Yi_Radicals -> "Yi_Radicals"
  | `Yi_Syllables -> "Yi_Syllables"
  | `Yijing -> "Yijing"
  | `Zanabazar_Square -> "Zanabazar_Square"
  | `Znamenny_Music -> "Znamenny_Music"
  end

(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Line break *)

type line =
  [ `AI | `AK | `AL | `AP | `AS | `B2 | `BA | `BB | `BK | `CB | `CJ | `CL
  | `CM | `CP | `CR | `EX | `EB | `EM | `GL | `H2 | `H3 | `HL | `HY | `ID
  | `IN | `IS | `JL | `JT | `JV | `LF | `NL | `NS | `NU | `OP | `PO | `PR
  | `QU | `RI | `SA | `SG | `SP | `SY | `VF | `VI | `WJ | `XX | `ZW | `ZWJ ]

let line_of_byte : line array =
  [| `AI; `AK; `AL; `AP; `AS; `B2; `BA; `BB; `BK; `CB; `CJ; `CL;
     `CM; `CP; `CR; `EX; `EB; `EM; `GL; `H2; `H3; `HL; `HY; `ID;
     `IN; `IS; `JL; `JT; `JV; `LF; `NL; `NS; `NU; `OP; `PO; `PR;
     `QU; `RI; `SA; `SG; `SP; `SY; `VF; `VI; `WJ; `XX; `ZW; `ZWJ |]

let line_max = Array.length line_of_byte - 1

let line_to_byte = function
| `AI -> 0 | `AK -> 1 | `AL -> 2 | `AP -> 3 | `AS -> 4 | `B2 -> 5
| `BA -> 6 | `BB -> 7 | `BK -> 8 | `CB -> 9 | `CJ -> 10 | `CL -> 11
| `CM -> 12 | `CP -> 13 | `CR -> 14 | `EX -> 15 | `EB -> 16 | `EM -> 17
| `GL -> 18 | `H2 -> 19 | `H3 -> 20 | `HL -> 21 | `HY -> 22 | `ID -> 23
| `IN -> 24 | `IS -> 25 | `JL -> 26 | `JT -> 27 | `JV -> 28 | `LF -> 29
| `NL -> 30 | `NS -> 31 | `NU -> 32 | `OP -> 33 | `PO -> 34 | `PR -> 35
| `QU -> 36 | `RI -> 37 | `SA -> 38 | `SG -> 39 | `SP -> 40 | `SY -> 41
| `VF -> 42 | `VI -> 43 | `WJ -> 44 | `XX -> 45 | `ZW -> 46 | `ZWJ -> 47

let pp_line ppf v = Format.fprintf ppf "%s" begin match v with
  | `AI -> "AI" | `AK -> "AK" | `AL -> "AL" | `AP -> "AP" | `AS -> "AS"
  | `B2 -> "B2" | `BA -> "BA" | `BB -> "BB" | `BK -> "BK" | `CB -> "CB"
  | `CJ -> "CJ" | `CL -> "CL" | `CM -> "CM" | `CP -> "CP" | `CR -> "CR"
  | `EX -> "EX" | `EB -> "EB" | `EM -> "EM" | `GL -> "GL" | `H2 -> "H2"
  | `H3 -> "H3" | `HL -> "HL" | `HY -> "HY" | `ID -> "ID" | `IN -> "IN"
  | `IS -> "IS" | `JL -> "JL" | `JT -> "JT" | `JV -> "JV" | `LF -> "LF"
  | `NL -> "NL" | `NS -> "NS" | `NU -> "NU" | `OP -> "OP" | `PO -> "PO"
  | `PR -> "PR" | `QU -> "QU" | `RI -> "RI" | `SA -> "SA" | `SG -> "SG"
  | `SP -> "SP" | `SY -> "SY" | `VF -> "VF" | `VI -> "VI" | `WJ -> "WJ"
  | `XX -> "XX" | `ZW -> "ZW" | `ZWJ -> "ZWJ"
  end

(* Grapheme cluster break *)

type grapheme_cluster =
  [ `CN | `CR | `EX | `EB | `EBG | `EM | `GAZ | `L | `LF | `LV | `LVT
  | `PP | `RI | `SM | `T | `V | `XX | `ZWJ ]

let grapheme_cluster_of_byte : grapheme_cluster array =
    [| `CN; `CR; `EX; `EB; `EBG; `EM; `GAZ; `L; `LF; `LV; `LVT; `PP; `RI;
       `SM; `T; `V; `XX; `ZWJ |]

let grapheme_cluster_max = Array.length grapheme_cluster_of_byte - 1

let grapheme_cluster_to_byte = function
| `CN -> 0 | `CR -> 1 | `EX -> 2 | `EB -> 3 | `EBG -> 4 | `EM -> 5
| `GAZ -> 6 | `L -> 7 | `LF -> 8 | `LV -> 9 | `LVT -> 10 | `PP -> 11
| `RI -> 12 | `SM -> 13 | `T -> 14 | `V -> 15 | `XX -> 16 | `ZWJ -> 17

let pp_grapheme_cluster ppf v = Format.fprintf ppf "%s" begin match v with
  | `CN -> "CN" | `CR -> "CR" | `EX -> "EX" | `EB -> "EB" | `EBG -> "EBG"
  | `EM -> "EM" | `GAZ -> "GAZ" | `L -> "L" | `LF -> "LF" | `LV -> "LV"
  | `LVT -> "LVT" | `PP -> "PP" | `RI -> "RI" | `SM -> "SM" | `T -> "T"
  | `V -> "V" | `XX -> "XX" | `ZWJ -> "ZWJ"
  end

(* Word break *)

type word =
  [ `CR | `DQ | `EX | `EB | `EBG | `EM | `Extend | `FO | `GAZ | `HL | `KA
  | `LE | `LF | `MB | `ML | `MN | `NL | `NU | `RI | `SQ | `WSegSpace | `XX
  | `ZWJ ]

let word_of_byte : word array =
  [| `CR; `DQ; `EX; `EB; `EBG; `EM; `Extend; `FO; `GAZ; `HL; `KA; `LE; `LF;
     `MB; `ML; `MN; `NL; `NU; `RI; `SQ; `WSegSpace; `XX; `ZWJ |]

let word_max = Array.length word_of_byte - 1

let word_to_byte = function
| `CR -> 0 | `DQ -> 1 | `EX -> 2 | `EB -> 3 | `EBG -> 4 | `EM -> 5
| `Extend -> 6 | `FO -> 7 | `GAZ -> 8 | `HL -> 9 | `KA -> 10 | `LE -> 11
| `LF -> 12 | `MB -> 13 | `ML -> 14 | `MN -> 15 | `NL -> 16 | `NU -> 17
| `RI -> 18 | `SQ -> 19 | `WSegSpace -> 20 | `XX -> 21 | `ZWJ -> 22

let pp_word ppf v = Format.fprintf ppf "%s" begin match v with
  | `CR -> "CR" | `DQ -> "DQ" | `EX -> "EX" | `EB -> "EB" | `EBG -> "EBG"
  | `EM -> "EM"  | `Extend -> "Extend" | `FO -> "FO" | `GAZ -> "GAZ"
  | `HL -> "HL" | `KA -> "KA" | `LE -> "LE" | `LF -> "LF" | `MB -> "MB"
  | `ML -> "ML" | `MN -> "MN" | `NL -> "NL" | `NU -> "NU" | `RI -> "RI"
  | `SQ -> "SQ" | `WSegSpace -> "WSegSpace" | `XX -> "XX" | `ZWJ -> "ZWJ"
  end

(* Sentence break *)

type sentence =
  [ `AT | `CL | `CR | `EX | `FO | `LE | `LF | `LO | `NU | `SC | `SE | `SP
  | `ST | `UP | `XX ]

let sentence_of_byte : sentence array =
  [| `AT; `CL; `CR; `EX; `FO; `LE; `LF; `LO; `NU; `SC; `SE; `SP; `ST; `UP; `XX|]

let sentence_max = Array.length sentence_of_byte - 1

let sentence_to_byte = function
| `AT -> 0 | `CL -> 1 | `CR -> 2 | `EX -> 3 | `FO -> 4 | `LE -> 5 | `LF -> 6
| `LO -> 7 | `NU -> 8 | `SC -> 9 | `SE -> 10 | `SP -> 11 | `ST -> 12
| `UP -> 13 | `XX -> 14

let pp_sentence ppf v = Format.fprintf ppf "%s" begin match v with
  | `AT -> "AT" | `CL -> "CL" | `CR -> "CR" | `EX -> "EX" | `FO -> "FO"
  | `LE -> "LE" | `LF -> "LF" | `LO -> "LO" | `NU -> "NU" | `SC -> "SC"
  | `SE -> "SE" | `SP -> "SP" | `ST -> "ST" | `UP -> "UP" | `XX -> "XX"
  end

(* Indic conjunct break *)

type indic_conjunct_break =
  [ `Consonant | `Extend | `Linker | `None ]

let indic_conjunct_break_of_byte : indic_conjunct_break array =
  [| `Consonant; `Extend; `Linker; `None |]

let indic_conjunct_break_max = Array.length indic_conjunct_break_of_byte - 1

let indic_conjunct_break_to_byte = function
| `Consonant -> 0 | `Extend -> 1 | `Linker -> 2 | `None -> 3

let pp_indic_conjunct_break ppf v = Format.fprintf ppf "%s" begin match v with
  | `Consonant -> "Consonant" | `Extend -> "Extend" | `Linker -> "Linker"
  | `None -> "None"
  end

(* East Asian width *)

type east_asian_width = [ `A | `F | `H | `N | `Na | `W ]
let pp_east_asian_width ppf v = Format.pp_print_string ppf begin match v with
| `A -> "A" | `F -> "F" | `H -> "H" | `N -> "N" | `Na -> "Na" | `W -> "W"
end

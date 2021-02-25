(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

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

let pp ppf s = Format.fprintf ppf "%s" begin match s with
  | `Adlm -> "Adlm"
  | `Aghb -> "Aghb"
  | `Ahom -> "Ahom"
  | `Arab -> "Arab"
  | `Armi -> "Armi"
  | `Armn -> "Armn"
  | `Avst -> "Avst"
  | `Bali -> "Bali"
  | `Bamu -> "Bamu"
  | `Bass -> "Bass"
  | `Batk -> "Batk"
  | `Beng -> "Beng"
  | `Bhks -> "Bhks"
  | `Bopo -> "Bopo"
  | `Brah -> "Brah"
  | `Brai -> "Brai"
  | `Bugi -> "Bugi"
  | `Buhd -> "Buhd"
  | `Cakm -> "Cakm"
  | `Cans -> "Cans"
  | `Cari -> "Cari"
  | `Cham -> "Cham"
  | `Cher -> "Cher"
  | `Chrs -> "Chrs"
  | `Copt -> "Copt"
  | `Cprt -> "Cprt"
  | `Cyrl -> "Cyrl"
  | `Deva -> "Deva"
  | `Diak -> "Diak"
  | `Dogr -> "Dogr"
  | `Dsrt -> "Dsrt"
  | `Dupl -> "Dupl"
  | `Egyp -> "Egyp"
  | `Elba -> "Elba"
  | `Elym -> "Elym"
  | `Ethi -> "Ethi"
  | `Geor -> "Geor"
  | `Glag -> "Glag"
  | `Gong -> "Gong"
  | `Gonm -> "Gonm"
  | `Goth -> "Goth"
  | `Gran -> "Gran"
  | `Grek -> "Grek"
  | `Gujr -> "Gujr"
  | `Guru -> "Guru"
  | `Hang -> "Hang"
  | `Hani -> "Hani"
  | `Hano -> "Hano"
  | `Hatr -> "Hatr"
  | `Hebr -> "Hebr"
  | `Hira -> "Hira"
  | `Hluw -> "Hluw"
  | `Hmng -> "Hmng"
  | `Hmnp -> "Hmnp"
  | `Hrkt -> "Hrkt"
  | `Hung -> "Hung"
  | `Ital -> "Ital"
  | `Java -> "Java"
  | `Kali -> "Kali"
  | `Kana -> "Kana"
  | `Khar -> "Khar"
  | `Khmr -> "Khmr"
  | `Khoj -> "Khoj"
  | `Knda -> "Knda"
  | `Kthi -> "Kthi"
  | `Kits -> "Kits"
  | `Lana -> "Lana"
  | `Laoo -> "Laoo"
  | `Latn -> "Latn"
  | `Lepc -> "Lepc"
  | `Limb -> "Limb"
  | `Lina -> "Lina"
  | `Linb -> "Linb"
  | `Lisu -> "Lisu"
  | `Lyci -> "Lyci"
  | `Lydi -> "Lydi"
  | `Mahj -> "Mahj"
  | `Maka -> "Maka"
  | `Mand -> "Mand"
  | `Mani -> "Mani"
  | `Marc -> "Marc"
  | `Medf -> "Medf"
  | `Mend -> "Mend"
  | `Merc -> "Merc"
  | `Mero -> "Mero"
  | `Mlym -> "Mlym"
  | `Modi -> "Modi"
  | `Mong -> "Mong"
  | `Mroo -> "Mroo"
  | `Mtei -> "Mtei"
  | `Mult -> "Mult"
  | `Mymr -> "Mymr"
  | `Nand -> "Nand"
  | `Narb -> "Narb"
  | `Nbat -> "Nbat"
  | `Newa -> "Newa"
  | `Nkoo -> "Nkoo"
  | `Nshu -> "Nshu"
  | `Ogam -> "Ogam"
  | `Olck -> "Olck"
  | `Orkh -> "Orkh"
  | `Orya -> "Orya"
  | `Osge -> "Osge"
  | `Osma -> "Osma"
  | `Palm -> "Palm"
  | `Pauc -> "Pauc"
  | `Perm -> "Perm"
  | `Phag -> "Phag"
  | `Phli -> "Phli"
  | `Phlp -> "Phlp"
  | `Phnx -> "Phnx"
  | `Plrd -> "Plrd"
  | `Prti -> "Prti"
  | `Qaai -> "Qaai"
  | `Rjng -> "Rjng"
  | `Rohg -> "Rohg"
  | `Runr -> "Runr"
  | `Samr -> "Samr"
  | `Sarb -> "Sarb"
  | `Saur -> "Saur"
  | `Sgnw -> "Sgnw"
  | `Shaw -> "Shaw"
  | `Shrd -> "Shrd"
  | `Sidd -> "Sidd"
  | `Sind -> "Sind"
  | `Sinh -> "Sinh"
  | `Sogd -> "Sogd"
  | `Sogo -> "Sogo"
  | `Sora -> "Sora"
  | `Soyo -> "Soyo"
  | `Sund -> "Sund"
  | `Sylo -> "Sylo"
  | `Syrc -> "Syrc"
  | `Tagb -> "Tagb"
  | `Takr -> "Takr"
  | `Tale -> "Tale"
  | `Talu -> "Talu"
  | `Taml -> "Taml"
  | `Tang -> "Tang"
  | `Tavt -> "Tavt"
  | `Telu -> "Telu"
  | `Tfng -> "Tfng"
  | `Tglg -> "Tglg"
  | `Thaa -> "Thaa"
  | `Thai -> "Thai"
  | `Tibt -> "Tibt"
  | `Tirh -> "Tirh"
  | `Ugar -> "Ugar"
  | `Vaii -> "Vaii"
  | `Wara -> "Wara"
  | `Wcho -> "Wcho"
  | `Xpeo -> "Xpeo"
  | `Xsux -> "Xsux"
  | `Yezi -> "Yezi"
  | `Yiii -> "Yiii"
  | `Zanb -> "Zanb"
  | `Zinh -> "Zinh"
  | `Zyyy -> "Zyyy"
  | `Zzzz -> "Zzzz"
  end

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

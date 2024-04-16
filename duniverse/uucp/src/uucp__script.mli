(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uucp programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Script and script extensions properties.

    {b References.}
    {ul
    {- Mark Davis, Ken Whistler.
    {{:http://www.unicode.org/reports/tr24/}{e Unicode script property}}.
    (latest version)}
    {- {{:http://www.unicode.org/charts/script/index.html}The Unicode script
    charts}.}} *)


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
  | `Cpmn
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
  | `Kawi
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
  | `Nagm
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
  | `Ougr
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
  | `Tnsa
  | `Toto
  | `Ugar
  | `Vaii
  | `Vith
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

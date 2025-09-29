type (_[@foo] : value) t

type ('a[@foo] : value) t

type ('a[@foo] : value, _[@foo] : value) t

type (!'a[@foo] : value, !_[@foo] : value) t

type _[@foo] t

type 'a[@foo] t

(* test comment preservation *)

type (* 01 *) ( (* 02 *) _ (* 03 *) [@foo] (* 04 *) : (* 05 *) value (* 06 *) ) (* 07 *) t (* 08 *)

type (* 09 *) ( (* 10 *) 'a (* 11 *) [@foo] (* 12 *) : (* 13 *) value (* 14 *) ) (* 15 *) t (* 16 *)

type (* 17 *) ( (* 18 *) 'a (* 19 *) [@foo] (* 20 *) : (* 21 *) value (* 22 *) , (* 23 *) _ (* 24 *) [@foo] (* 25 *) : (* 26 *) value (* 27 *) ) (* 28 *) t (* 29 *)

type (* 30 *) _ (* 31 *) [@foo] (* 32 *) t (* 33 *)

type (* 34 *) 'a (* 35 *) [@foo] (* 36 *) t (* 37 *)

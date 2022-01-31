let x = #3l

let x = #3L

let x = #3n

let x = -#3l

let x = #3.0

let x = -#3.0

let x = #3.0 + #4.0

let x = #3.0 - #4.0

let x = (#3.0 [@attr])

let x = (#3.0 + #4.0) [@attr]

let x = f #3.0 #4.0 #5.0 x y #0.

type t = float#

type t = float# * float#

type t = float# t2

type t = float #t2

type t = (int, float#) either

type t = (float#, int) either

type t = (float#, float#) either

type ('a : float64) t = 'a

type ('b, 'a : float64) t

type ('b : float64, 'a : immediate) t

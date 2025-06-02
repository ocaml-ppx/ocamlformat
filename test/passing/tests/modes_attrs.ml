(* let bindings *)

let[@attr] x @ mode1 mode2 = y
let[@attr] x : typ @ mode1 mode2 = y

(* expressions *)

let x = (expr [@attr] : typ @ mode1 mode2)
let x = (expr : (typ[@attr]) @ mode1 mode2)
let x = ((expr : typ @ mode1 mode2) [@attr])

(* lhs/rhs of arrows *)

type t = (lhs[@attr]) @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6
type t = lhs @ m1 m2 -> (mhs[@attr]) @ m3 m4 -> rhs @ m5 m6
type t = lhs @ m1 m2 -> mhs @ m3 m4 -> (rhs[@attr]) @ m5 m6
type t = lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6 [@@attr]

let x : ((lhs[@attr]) @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6) @ m7 m8 = y
let x : (lhs @ m1 m2 -> (mhs[@attr]) @ m3 m4 -> rhs @ m5 m6) @ m7 m8 = y
let x : (lhs @ m1 m2 -> mhs @ m3 m4 -> (rhs[@attr]) @ m5 m6) @ m7 m8 = y
let x = (expr [@attr] : (lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6) @ m7 m8)
let x = (expr : ((lhs[@attr]) @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6) @ m7 m8)
let x = (expr : (lhs @ m1 m2 -> (mhs[@attr]) @ m3 m4 -> rhs @ m5 m6) @ m7 m8)
let x = (expr : (lhs @ m1 m2 -> mhs @ m3 m4 -> (rhs[@attr]) @ m5 m6) @ m7 m8)
let x = (expr : (lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6) @ m7 m8) [@@attr]

(* modalities on record fields *)

type t = { x : (t[@attr]) @@ mode1 mode2 }
type t = { x : t @@ mode1 mode2 }
type t = { x : t @@ mode2 mode1 }
type t = { x : t @@ mode1 mode2 [@attr] }
type t = { mutable x : (t[@attr]) @@ mode1 mode2 }
type t = { mutable x : t @@ mode1 mode2 [@attr] }

(* modalities on constructor arguments *)

type t =
  | A of (t1[@attr]) @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8
  | A of t1 @@ m1 m2 * (t2[@attr]) @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8
  | A of t1 @@ m1 m2 * t2 @@ m3 m4 * ((t3[@attr]) @ m5 -> t4 @ m6) @@ m7 m8
  | A of t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> (t4[@attr]) @ m6) @@ m7 m8
  | A of t1 @@ m1 m2 * t2 @@ m3 m4 * ((t3 @ m5 -> t4 @ m6)[@attr]) @@ m7 m8
  | A of t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8 [@attr]

type t =
  | A : (t1[@attr]) @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8 -> t
  | A : t1 @@ m1 m2 * (t2[@attr]) @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8 -> t
  | A : t1 @@ m1 m2 * t2 @@ m3 m4 * ((t3[@attr]) @ m5 -> t4 @ m6) @@ m7 m8 -> t
  | A : t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> (t4[@attr]) @ m6) @@ m7 m8 -> t
  | A : t1 @@ m1 m2 * t2 @@ m3 m4 * ((t3 @ m5 -> t4 @ m6)[@attr]) @@ m7 m8 -> t
  | A : t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8 -> t [@attr]

(* value descriptions *)

module type S = sig
  val x : (t1[@attr]) @ m1 m2 -> t2 @ m3 m4 @@ m5 m6
  val x : t1 @ m1 m2 -> (t2[@attr]) @ m3 m4 @@ m5 m6
  val x : t1 @ m1 m2 -> t2 @ m3 m4 @@ m5 m6 [@@attr]
end

(* let-bound functions *)

let[@attr] (f @ mode1) (arg1 @ mode2) (arg2 @ mode3) : typ = x
let (f @ mode1) ((arg1 [@attr]) @ mode2) (arg2 @ mode3) : typ = x
let (f @ mode1) (arg1 @ mode2) ((arg2 [@attr]) @ mode3) : typ = x
let (f @ mode1) (arg1 @ mode2) (arg2 @ mode3) : (typ[@attr]) = x
let (f @ mode1) (arg1 @ mode2) (arg2 @ mode3) : typ = x [@@attr]

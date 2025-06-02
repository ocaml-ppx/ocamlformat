(* Check that comments are not dropped or moved in unusual ways.
   A few commented out tests where comments move have explanations, and are
   tested in [modes_cmts_move.ml]. *)

(* let bindings *)

let (* cmt *) x @ mode1 mode2 = y
let x (* cmt *) @ mode1 mode2 = y
let x @ (* cmt *) mode1 mode2 = y
let x @ mode1 (* cmt *) mode2 = y
let x @ mode1 mode2 (* cmt *) = y
let x @ mode1 mode2 = (* cmt *) y
let (* cmt *) x : typ @ mode1 mode2 = y
let x (* cmt *) : typ @ mode1 mode2 = y
let x : (* cmt *) typ @ mode1 mode2 = y
let x : typ (* cmt *) @ mode1 mode2 = y
let x : typ @ (* cmt *) mode1 mode2 = y
let x : typ @ mode1 (* cmt *) mode2 = y
let x : typ @ mode1 mode2 (* cmt *) = y
let x : typ @ mode1 mode2 = (* cmt *) y

(* expressions *)

let x = ((* cmt *) expr : typ @ mode1 mode2)
let x = (expr (* cmt *) : typ @ mode1 mode2)
let x = (expr : (* cmt *) typ @ mode1 mode2)
let x = (expr : typ (* cmt *) @ mode1 mode2)
let x = (expr : typ @ (* cmt *) mode1 mode2)
let x = (expr : typ @ mode1 (* cmt *) mode2)
let x = (expr : typ @ mode1 mode2 (* cmt *))

(* lhs/rhs of arrows *)

type t = (* cmt *) lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6
type t = lhs (* cmt *) @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6
type t = lhs @ (* cmt *) m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6
type t = lhs @ m1 (* cmt *) m2 -> mhs @ m3 m4 -> rhs @ m5 m6
type t = lhs @ m1 m2 (* cmt *) -> mhs @ m3 m4 -> rhs @ m5 m6
type t = lhs @ m1 m2 -> (* cmt *) mhs @ m3 m4 -> rhs @ m5 m6
type t = lhs @ m1 m2 -> mhs (* cmt *) @ m3 m4 -> rhs @ m5 m6
type t = lhs @ m1 m2 -> mhs @ (* cmt *) m3 m4 -> rhs @ m5 m6
type t = lhs @ m1 m2 -> mhs @ m3 (* cmt *) m4 -> rhs @ m5 m6
type t = lhs @ m1 m2 -> mhs @ m3 m4 (* cmt *) -> rhs @ m5 m6
type t = lhs @ m1 m2 -> mhs @ m3 m4 -> (* cmt *) rhs @ m5 m6
type t = lhs @ m1 m2 -> mhs @ m3 m4 -> rhs (* cmt *) @ m5 m6
type t = lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ (* cmt *) m5 m6
type t = lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 (* cmt *) m6
type t = lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6 (* cmt *)

let x : ((* cmt *) lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6) @ m7 m8 = y
let x : (lhs (* cmt *) @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6) @ m7 m8 = y
let x : (lhs @ (* cmt *) m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6) @ m7 m8 = y
let x : (lhs @ m1 (* cmt *) m2 -> mhs @ m3 m4 -> rhs @ m5 m6) @ m7 m8 = y
let x : (lhs @ m1 m2 (* cmt *) -> mhs @ m3 m4 -> rhs @ m5 m6) @ m7 m8 = y
let x : (lhs @ m1 m2 -> (* cmt *) mhs @ m3 m4 -> rhs @ m5 m6) @ m7 m8 = y
let x : (lhs @ m1 m2 -> mhs (* cmt *) @ m3 m4 -> rhs @ m5 m6) @ m7 m8 = y
let x : (lhs @ m1 m2 -> mhs @ (* cmt *) m3 m4 -> rhs @ m5 m6) @ m7 m8 = y
let x : (lhs @ m1 m2 -> mhs @ m3 (* cmt *) m4 -> rhs @ m5 m6) @ m7 m8 = y
let x : (lhs @ m1 m2 -> mhs @ m3 m4 (* cmt *) -> rhs @ m5 m6) @ m7 m8 = y
let x : (lhs @ m1 m2 -> mhs @ m3 m4 -> (* cmt *) rhs @ m5 m6) @ m7 m8 = y
let x : (lhs @ m1 m2 -> mhs @ m3 m4 -> rhs (* cmt *) @ m5 m6) @ m7 m8 = y
let x : (lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ (* cmt *) m5 m6) @ m7 m8 = y
let x : (lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 (* cmt *) m6) @ m7 m8 = y
let x : (lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6 (* cmt *)) @ m7 m8 = y
let x : (lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6) @ (* cmt *) m7 m8 = y
let x : (lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6) @ m7 (* cmt *) m8 = y
let x : (lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6) @ m7 m8 (* cmt *) = y
let x = (expr (* cmt *) : (lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6) @ m7 m8)
let x = (expr : ((* cmt *) lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6) @ m7 m8)
let x = (expr : (lhs (* cmt *) @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6) @ m7 m8)
let x = (expr : (lhs @ (* cmt *) m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6) @ m7 m8)
let x = (expr : (lhs @ m1 (* cmt *) m2 -> mhs @ m3 m4 -> rhs @ m5 m6) @ m7 m8)
let x = (expr : (lhs @ m1 m2 (* cmt *) -> mhs @ m3 m4 -> rhs @ m5 m6) @ m7 m8)
let x = (expr : (lhs @ m1 m2 -> (* cmt *) mhs @ m3 m4 -> rhs @ m5 m6) @ m7 m8)
let x = (expr : (lhs @ m1 m2 -> mhs (* cmt *) @ m3 m4 -> rhs @ m5 m6) @ m7 m8)
let x = (expr : (lhs @ m1 m2 -> mhs @ (* cmt *) m3 m4 -> rhs @ m5 m6) @ m7 m8)
let x = (expr : (lhs @ m1 m2 -> mhs @ m3 (* cmt *) m4 -> rhs @ m5 m6) @ m7 m8)
let x = (expr : (lhs @ m1 m2 -> mhs @ m3 m4 (* cmt *) -> rhs @ m5 m6) @ m7 m8)
let x = (expr : (lhs @ m1 m2 -> mhs @ m3 m4 -> (* cmt *) rhs @ m5 m6) @ m7 m8)
let x = (expr : (lhs @ m1 m2 -> mhs @ m3 m4 -> rhs (* cmt *) @ m5 m6) @ m7 m8)
let x = (expr : (lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ (* cmt *) m5 m6) @ m7 m8)
let x = (expr : (lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 (* cmt *) m6) @ m7 m8)
let x = (expr : (lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6 (* cmt *)) @ m7 m8)
let x = (expr : (lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6) @ (* cmt *) m7 m8)
let x = (expr : (lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6) @ m7 (* cmt *) m8)
let x = (expr : (lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6) @ m7 m8 (* cmt *))
let x = (expr : (lhs @ m1 m2 -> mhs @ m3 m4 -> rhs @ m5 m6) @ m7 m8) (* cmt *)

(* modalities on record fields *)

type t = { x (* cmt *) : t @@ mode1 mode2; y : t @@ mode1 mode2 }
type t = { x : (* cmt *) t @@ mode1 mode2; y : t @@ mode1 mode2 }
type t = { x : t (* cmt *) @@ mode1 mode2; y : t @@ mode1 mode2 }
type t = { x : t @@ (* cmt *) mode1 mode2; y : t @@ mode1 mode2 }
type t = { x : t @@ mode1 (* cmt *) mode2; y : t @@ mode1 mode2 }
type t = { x : t @@ mode1 mode2 (* cmt *); y : t @@ mode1 mode2 }
type t = { x : t @@ mode1 mode2; (* cmt *) y : t @@ mode1 mode2 }
type t = { x : t @@ mode1 mode2; y (* cmt *) : t @@ mode1 mode2 }
type t = { x : t @@ mode1 mode2; y : (* cmt *) t @@ mode1 mode2 }
type t = { x : t @@ mode1 mode2; y : t (* cmt *) @@ mode1 mode2 }
type t = { x : t @@ mode1 mode2; y : t @@ (* cmt *) mode1 mode2 }
type t = { x : t @@ mode1 mode2; y : t @@ mode1 (* cmt *) mode2 }
type t = { x : t @@ mode1 mode2; y : t @@ mode1 mode2 (* cmt *) }
type t = { mutable x (* cmt *) : t @@ mode1 mode2; y : t @@ mode1 mode2 }
type t = { mutable x : (* cmt *) t @@ mode1 mode2; y : t @@ mode1 mode2 }
type t = { mutable x : t (* cmt *) @@ mode1 mode2; y : t @@ mode1 mode2 }
type t = { mutable x : t @@ (* cmt *) mode1 mode2; y : t @@ mode1 mode2 }
type t = { mutable x : t @@ mode1 (* cmt *) mode2; y : t @@ mode1 mode2 }
type t = { mutable x : t @@ mode1 mode2 (* cmt *); y : t @@ mode1 mode2 }
type t = { mutable x : t @@ mode1 mode2; (* cmt *) y : t @@ mode1 mode2 }
type t = { mutable x : t @@ mode1 mode2; y (* cmt *) : t @@ mode1 mode2 }
type t = { mutable x : t @@ mode1 mode2; y : (* cmt *) t @@ mode1 mode2 }
type t = { mutable x : t @@ mode1 mode2; y : t (* cmt *) @@ mode1 mode2 }
type t = { mutable x : t @@ mode1 mode2; y : t @@ (* cmt *) mode1 mode2 }
type t = { mutable x : t @@ mode1 mode2; y : t @@ mode1 (* cmt *) mode2 }
type t = { mutable x : t @@ mode1 mode2; y : t @@ mode1 mode2 (* cmt *) }

(* modalities on constructor arguments *)

type t =
  | A of (* cmt *) t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8
  | A of t1 (* cmt *) @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8
  | A of t1 @@ (* cmt *) m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8
  | A of t1 @@ m1 (* cmt *) m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8
  | A of t1 @@ m1 m2 (* cmt *) * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8
  | A of t1 @@ m1 m2 * (* cmt *) t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8
  | A of t1 @@ m1 m2 * t2 (* cmt *) @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8
  | A of t1 @@ m1 m2 * t2 @@ (* cmt *) m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8
  | A of t1 @@ m1 m2 * t2 @@ m3 (* cmt *) m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8
  | A of t1 @@ m1 m2 * t2 @@ m3 m4 (* cmt *) * (t3 @ m5 -> t4 @ m6) @@ m7 m8
  | A of t1 @@ m1 m2 * t2 @@ m3 m4 * (* cmt *) (t3 @ m5 -> t4 @ m6) @@ m7 m8
  | A of t1 @@ m1 m2 * t2 @@ m3 m4 * ((* cmt *) t3 @ m5 -> t4 @ m6) @@ m7 m8
  | A of t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 (* cmt *) @ m5 -> t4 @ m6) @@ m7 m8
  | A of t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ (* cmt *) m5 -> t4 @ m6) @@ m7 m8
  | A of t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 (* cmt *) -> t4 @ m6) @@ m7 m8
  | A of t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> (* cmt *) t4 @ m6) @@ m7 m8
  | A of t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 (* cmt *) @ m6) @@ m7 m8
  | A of t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ (* cmt *) m6) @@ m7 m8
  | A of t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6 (* cmt *)) @@ m7 m8
  (* | A of t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) (* cmt *) @@ m7 m8 *)
  | A of t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ (* cmt *) m7 m8
  | A of t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 (* cmt *) m8
  | A of t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8 (* cmt *)

type t =
  | A : (* cmt *) t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8 -> t
  | A : t1 (* cmt *) @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8 -> t
  | A : t1 @@ (* cmt *) m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8 -> t
  | A : t1 @@ m1 (* cmt *) m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8 -> t
  | A : t1 @@ m1 m2 (* cmt *) * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8 -> t
  | A : t1 @@ m1 m2 * (* cmt *) t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8 -> t
  | A : t1 @@ m1 m2 * t2 (* cmt *) @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8 -> t
  | A : t1 @@ m1 m2 * t2 @@ (* cmt *) m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8 -> t
  | A : t1 @@ m1 m2 * t2 @@ m3 (* cmt *) m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8 -> t
  | A : t1 @@ m1 m2 * t2 @@ m3 m4 (* cmt *) * (t3 @ m5 -> t4 @ m6) @@ m7 m8 -> t
  | A : t1 @@ m1 m2 * t2 @@ m3 m4 * (* cmt *) (t3 @ m5 -> t4 @ m6) @@ m7 m8 -> t
  | A : t1 @@ m1 m2 * t2 @@ m3 m4 * ((* cmt *) t3 @ m5 -> t4 @ m6) @@ m7 m8 -> t
  | A : t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 (* cmt *) @ m5 -> t4 @ m6) @@ m7 m8 -> t
  | A : t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ (* cmt *) m5 -> t4 @ m6) @@ m7 m8 -> t
  | A : t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 (* cmt *) -> t4 @ m6) @@ m7 m8 -> t
  | A : t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> (* cmt *) t4 @ m6) @@ m7 m8 -> t
  | A : t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 (* cmt *) @ m6) @@ m7 m8 -> t
  | A : t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ (* cmt *) m6) @@ m7 m8 -> t
  | A : t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6 (* cmt *)) @@ m7 m8 -> t
  (* Comment moves between [@@] and [m7]:
     | A : t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) (* cmt *) @@ m7 m8 -> t *)
  | A : t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ (* cmt *) m7 m8 -> t
  | A : t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 (* cmt *) m8 -> t
  | A : t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) @@ m7 m8 (* cmt *) -> t

(* value descriptions *)

module type S = sig
  val x : (* cmt *) t1 @ m1 m2 -> t2 @ m3 m4 @@ m5 m6
  val x : t1 (* cmt *) @ m1 m2 -> t2 @ m3 m4 @@ m5 m6
  val x : t1 @ (* cmt *) m1 m2 -> t2 @ m3 m4 @@ m5 m6
  val x : t1 @ m1 (* cmt *) m2 -> t2 @ m3 m4 @@ m5 m6
  val x : t1 @ m1 m2 (* cmt *) -> t2 @ m3 m4 @@ m5 m6
  val x : t1 @ m1 m2 -> (* cmt *) t2 @ m3 m4 @@ m5 m6
  val x : t1 @ m1 m2 -> t2 (* cmt *) @ m3 m4 @@ m5 m6
  val x : t1 @ m1 m2 -> t2 @ (* cmt *) m3 m4 @@ m5 m6
  val x : t1 @ m1 m2 -> t2 @ m3 (* cmt *) m4 @@ m5 m6
  val x : t1 @ m1 m2 -> t2 @ m3 m4 (* cmt *) @@ m5 m6
  val x : t1 @ m1 m2 -> t2 @ m3 m4 @@ (* cmt *) m5 m6
  val x : t1 @ m1 m2 -> t2 @ m3 m4 @@ m5 (* cmt *) m6
  val x : t1 @ m1 m2 -> t2 @ m3 m4 @@ m5 m6 (* cmt *)
end

(* let-bound functions *)

(* Comment moves to between [(] and [f]:
   let (* cmt *) (f @ mode1) (arg1 @ mode2) (arg2 @ mode3) : typ = x *)

let ((* cmt *) f @ mode1) (arg1 @ mode2) (arg2 @ mode3) : typ = x
let (f (* cmt *) @ mode1) (arg1 @ mode2) (arg2 @ mode3) : typ = x
let (f @ (* cmt *) mode1) (arg1 @ mode2) (arg2 @ mode3) : typ = x
let (f @ mode1 (* cmt *)) (arg1 @ mode2) (arg2 @ mode3) : typ = x
let (f @ mode1) (* cmt *) (arg1 @ mode2) (arg2 @ mode3) : typ = x
let (f @ mode1) ((* cmt *) arg1 @ mode2) (arg2 @ mode3) : typ = x
let (f @ mode1) (arg1 (* cmt *) @ mode2) (arg2 @ mode3) : typ = x
let (f @ mode1) (arg1 @ (* cmt *) mode2) (arg2 @ mode3) : typ = x
let (f @ mode1) (arg1 @ mode2 (* cmt *)) (arg2 @ mode3) : typ = x
let (f @ mode1) (arg1 @ mode2) (* cmt *) (arg2 @ mode3) : typ = x
let (f @ mode1) (arg1 @ mode2) ((* cmt *) arg2 @ mode3) : typ = x
let (f @ mode1) (arg1 @ mode2) (arg2 (* cmt *) @ mode3) : typ = x
let (f @ mode1) (arg1 @ mode2) (arg2 @ (* cmt *) mode3) : typ = x
let (f @ mode1) (arg1 @ mode2) (arg2 @ mode3 (* cmt *)) : typ = x

(* Comment moves to after [=], but not because of modes:
   let (f @ mode1) (arg1 @ mode2) (arg2 @ mode3) (* cmt *) : typ = x *)

let (f @ mode1) (arg1 @ mode2) (arg2 @ mode3) : (* cmt *) typ = x

(* examples taken from:
   ocaml/testsuite/tests/typing-modules/include_functor.ml *)

(* In structure *)
module type S = sig
  type t

  val x : t
end

module F1 (X : S) = struct
  let y = X.x
end

module M1 = struct
  type t = int

  let x = 5

  include functor F1
end

(* In signature *)
module type T = sig
  type s

  val f : s -> bool
end

module type F5 = functor (X : S) -> T with type s = X.t

module type M5_sig = sig
  type t

  val x : t

  include functor F5
end

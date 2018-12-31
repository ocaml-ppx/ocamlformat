[@@@ocamlformat "module-item-spacing=compact"]

val z : this one is pretty looooooooooooooooooooooooooooooooooong
val z : so is this oooooooooooooooooooooooooooooooooooooooooooone
val f : k -> k -> k -> k -> k k -> k k -> k k
(** [f o o o o o o] is a great function. *)

val z : this one is pretty looooooooooooooooooooooooooooooooooong
val z : so is this oooooooooooooooooooooooooooooooooooooooooooone
val g : unit

val f : aaaaaaaaaaaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb -> cccccccccccccccccccccccc -> dddddddddddd

(** [x] is a great value. *)
val x : k
val z : k
val y : k
(** [y] is a great value. *)

val z : this one is pretty looooooooooooooooooooooooooooooooooong
val z : so is this oooooooooooooooooooooooooooooooooooooooooooone

module A = AA
module B = BB
open AA
module C = CC

module type M = sig
  val a : z
  val b : zz
  val c : zzz
end

val x : k
val y : k
val x : k
val y : k
type c = {a : int; b : toto; c : char * char * char; d : [`Foo | `Bar]}
val z : this one is pretty looooooooooooooooooooooooooooooooooong
val z : so is this oooooooooooooooooooooooooooooooooooooooooooone

type k = A | B | K of int * char * string | E

val x : k
val z :
  this
    one
    is short
val y : k
val w : this one is toooooooooooooooooooooooooo looooooooooooooooooooooooog but is originally a one_liner
val k : z


module type N = sig
  val x : k

  val z :
    soooooooooo
      is
      this
      oooooooooooooooooooooooooooooooooooooooooooone

  val y : k
  val z : soooooooooo iis this oooooooooooooooooooooooooooooooooooooooooooone
  val y : k
  module A = AA
  include A
  module B = BB
  open B
end

;;
[@@@ocamlformat "module-item-spacing=preserve"]

val cmos_rtc_seconds : foo
val cmos_rtc_seconds_alarm : foo
val cmos_rtc_minutes : foo

val x : foo

val log_other : foo
val log_cpu : foo
val log_fpu : foo

val cr0_pe : foo
val cr0_mp : foo
val cr0_em : foo

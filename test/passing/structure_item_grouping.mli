[@@@ocamlformat "structure-item-grouping=compact"]

val z : this one is pretty looooooooooooooooooooooooooooooooooong
val z : so is this oooooooooooooooooooooooooooooooooooooooooooone
val f : k -> k -> k -> k -> k k -> k k -> k k
val z : this one is pretty looooooooooooooooooooooooooooooooooong
val z : so is this oooooooooooooooooooooooooooooooooooooooooooone
val g : unit

val f : aaaaaaaaaaaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb -> cccccccccccccccccccccccc -> dddddddddddd
val x : k
val y : k
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

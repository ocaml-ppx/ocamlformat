[@@@ocamlformat "module-item-spacing=compact"]

let z = this one is pretty looooooooooooooooooooooooooooooooooong
and z = so is this oooooooooooooooooooooooooooooooooooooooooooone
let f x = x + 1
let z = this one is pretty looooooooooooooooooooooooooooooooooong
let z = so is this oooooooooooooooooooooooooooooooooooooooooooone
let g = ()

let f = function
  | `a | `b | `c -> foo
  | `xxxxxxxxxxxxxxxxxx ->
      yyyyyyyyyyyyyyyyyyyyyyyy
        zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
        zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
        zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz

let x = 1
and y = 2
let z = this one is pretty looooooooooooooooooooooooooooooooooong
let z = so is this oooooooooooooooooooooooooooooooooooooooooooone

module A = AA
module B = BB
open AA
module C = CC

module M =
  X
    (Y)
    (struct
      let x = k
    end)

let x = 1
let y = 2
let x = 1
and y = 2
and c = {a : int; b : toto; c : char * char * char; d : [`Foo | `Bar]}
and z = this one is pretty looooooooooooooooooooooooooooooooooong
and z = so is this oooooooooooooooooooooooooooooooooooooooooooone

type k = A | B | K of int * char * string | E

let x = 1
let z =
  this
    one
    (is short)
let y = 2
let w = this one is toooooooooooooooooooooooooo (looooooooooooooooooooooooog but is (originally a one-liner))
let k = z


module N = struct
  let x = 1

  let z =
    soooooooooo
      is
      this
      oooooooooooooooooooooooooooooooooooooooooooone

  let y = 2
  let z = soooooooooo iis this oooooooooooooooooooooooooooooooooooooooooooone
  let y = 2
  module A = AA
  include A
  module B = BB
  open B
end

let x = x

(** comment *)
and y = y

let x = x

(** floating comment *)

and y = y

let x = x
and y = something veeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeery veeeeeeeeeeeeeeeeeeeeeeeeeeeery long

let y = something veeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeery veeeeeeeeeeeeeeeeeeeeeeeeeeeery long
and x = x

let a = a
and a = a
and a = a

[@@ocamlformat "module-item-spacing=sparse"]

and a = a
and a = a
and a = a

[@@ocamlformat "module-item-spacing=compact"]

and a = a
and a = a
and a = a

let x = 1

(* floating *)

let y = 2

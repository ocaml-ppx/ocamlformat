type%foo t = < .. >

type t =
  [%foooooooooo
    fooooooooooooooooooooooooooo foooooooooooooooooooooooooooooooooo
      foooooooooooooooooooooooooooooooooo foooooooooooooooooooooooooooo
      foooooooooooooooooooooooooooo]
[@@foooooooooo
  fooooooooooooooooooooooooooo foooooooooooooooooooooooooooooooooo
    foooooooooooooooooooooooooooooooooo foooooooooooooooooooooooooooo
    foooooooooooooooooooooooooooo]

[%%foooooooooo:
fooooooooooooooooooooooooooo foooooooooooooooooooooooooooooooooo
foooooooooooooooooooooooooooooooooo
foooooooooooooooooooooooooooo
foooooooooooooooooooooooooooo]

[@@@foooooooooo
fooooooooooooooooooooooooooo foooooooooooooooooooooooooooooooooo
  foooooooooooooooooooooooooooooooooo foooooooooooooooooooooooooooo
  foooooooooooooooooooooooooooo]

[%%ext
val foooooooooooooooooooooo : fooooooooooo

val fooooooooooooooooooooooooooo : fooooo]

exception%ext E

[%%ext exception E]

include%ext M

[%%ext include M]

module type%ext T = M

[%%ext module type T = M]

module%ext T : M

[%%ext: module T : M]

module%ext rec T : M
and Z : Q

[%%ext:
module rec T : M
and Z : Q]

module%ext T := M

[%%ext: module T := M]

open%ext M

open! %ext M

[%%ext open M]

[%%ext open! M]

type%foo t += T

[%%foo: type t += T]

val%foo x : t

[%%foo: val x : t]

external%foo x : t = ""

[%%foo: external x : t = ""]

class%foo x : t

[%%foo: class x : t]

class type%foo x = x

[%%foo: class type x = x]

type%ext t := x

[%%ext: type t := x]

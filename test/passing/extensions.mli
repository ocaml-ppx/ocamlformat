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

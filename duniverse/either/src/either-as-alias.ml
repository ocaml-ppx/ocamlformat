type ('a, 'b) t = ('a, 'b) Stdlib.Either.t = Left of 'a | Right of 'b

include (Stdlib.Either : Either_intf.S with type ('a, 'b) t := ('a, 'b) t)

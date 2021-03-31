type t = A : t

type t = A : t * 'b -> t

type (_, _, _, _, _) gadt =
  | SomeLongName :
      ('a, 'b, long_name * long_name2, 't, 'u) gadt
      * ('b, 'c, 'v, 'u, 'k) gadt2
      -> ('a, 'c, long_name * 'k, 't, 'v) gadt
  | AnEvenLongerName :
      ('a, 'b, long_name * long_name2, 't, 'u) gadt
      * ('b, 'c, 'v, 'u, 'k) gadt2
      -> ('a, 'c, long_name * 'k, 't, 'v) gadt

type _ t = ..

type _ t += A : int | B : int -> int

type t = A : (int -> int) -> int

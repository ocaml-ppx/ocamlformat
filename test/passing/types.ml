type uu = A of int | B of (< leq: 'a > as 'a)

type uu = A of int | B of (< leq: 'a > as 'a) * 'a

type uu = A of (int as 'a) | B of 'a * (< leq: 'a > as 'a)

type uu += A of (int as 'a)

type uu += B of 'a * (< leq: 'a > as 'a)

let _ = ignore Async_unix.Fd.(([stdin (); stdout (); stderr ()] : t list))

type t = { a: int; b: int }

type t = [`A | `B]

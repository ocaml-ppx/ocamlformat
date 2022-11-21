module type S = sig
  val x : unit -> unit
end

let get = failwith "TODO"

let foo () =
  let module X = (val get : S) in
  X.x ()

module type S = sig end

type t = (module S)

type 'a monoid_a = (module Monoid with type t = 'a)

type 'a monoid_a = (module Monoid with type F.t = 'a)

let sumi (type a) ((module A) : a monoid_a) (n : a) = A.mappend n A.mempty

module type BAR = sig
  module rec A : (FOO with type t = < b: B.t >)
  and B : FOO
end

module type M =
  module type of M
    with module A := A
    (*test*)
     and module A = A
    (*test*)
     and module A = A
    with module A = A
    (*test*)
    with module A = A

module U :
  S
    with type ttttttttt = int
     and type uuuuuuuu = int
     and type vvvvvvvvvvv = int = struct end

module U :
  S
    with type ttttttttt = int
     and type uuuuuuu = int
    with type vvvvvvvvv = int = struct end

module U :
  S
    with type Command.t =
      [ `Halt
      | `Unknown
      | `Error of string
      | `Config of (string * string) list
      | `Format of string ]
     and type Command.t =
      [ `Halt
      | `Unknown
      | `Error of string
      | `Config of (string * string) list
      | `Format of string ] = struct end

module U = (val S : S with type t = int and type u = int)

module U = (val S : S with type t = int and type u = int)

module type S = sig
  (* floating *)

  exception E
end

module type S' = functor
  (A : A)
  (B : sig
     type t
   end)
  (Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   : sig
     type t
   end)
  -> S with type t = B.t

module M : sig
  include (* foo *) module type of K

  include module type of
      Fooooooooooooooooooooooooooo (Foooooooooo.Foo) (Fooooooooooooo)
        (Fooooooooooooo)

  include (* fooooooooo *) module type of
      Fooooooooooooooooooooooooooo (Foooooooooo.Foo) (Fooooooooooooo)
        (Fooooooooooooo)
end = struct end

let foo (type foooo fooo_ooooo)
    (module Fooo : Fooooo_foooooooooo.Foooo_intf.Bar
      with type foooo = foooo
       and type Fooo_fooooooooo_fooooo.t =
         ( xxxxx
         , wwwwwwwwww
         , xxxxxxxxxxxxxxxxxxxx
         , xxxxxxxxxxxxxxxxx
         , xxxxxxxxxxxxxxxxxxxxxx
         , yyyyyyyyyyyyyyyyyyyyyy )
         Fooooo_ooooooo_oooooo.Foooo_fooooooooo_fooooo.t )
    (Fooo.Fooo.T (foo, bar)) xxxx =
  ()

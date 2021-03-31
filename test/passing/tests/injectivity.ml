type !'a t = private 'a ref

type +!'a t = private 'a

type -!'a t = private 'a -> unit

type +!'a t = private 'a

type -!'a t = private 'a -> unit

type +!'a t = private 'a

type -!'a t = private 'a -> unit

type +!'a t = private 'a

type -!'a t = private 'a -> unit

module M : sig
  type +!'a t
end = struct
  type 'a t = 'a list
end

module N : sig
  type +'a t
end = struct
  type 'a t = 'a list
end

type !'a t = 'a list

type !'a u = int

module M : sig
  type !'a t = private < m: int ; .. >
end = struct
  type 'a t = < m: int >
end

type 'a t = 'b constraint 'a = < b: 'b >

type !'b u = < b: 'b > t

type !_ t = X

type (_, _) eq = Refl : ('a, 'a) eq

type !'a t = private 'b constraint 'a = < b: 'b >

type !'a t = private 'b constraint 'a = < b: 'b ; c: 'c >

module M : sig
  type !'a t constraint 'a = < b: 'b ; c: 'c >
end = struct
  type nonrec 'a t = 'a t
end

type !'a u = int constraint 'a = 'b t

module F (X : sig
  type 'a t
end) =
struct
  type !'a u = 'b constraint 'a = < b: 'b > constraint 'b = _ X.t
end

module F (X : sig
  type 'a t
end) =
struct
  type !'a u = 'b X.t constraint 'a = < b: 'b X.t >
end

module F (X : sig
  type 'a t
end) =
struct
  type !'a u = 'b constraint 'a = < b: (_ X.t as 'b) >
end

module rec R : sig
  type !'a t = [`A of 'a S.t]
end =
  R

and S : sig
  type !'a t = 'a R.t
end =
  S

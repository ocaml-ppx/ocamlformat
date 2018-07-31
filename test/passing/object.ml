let _ =
  object
    (* some comment *)
    inherit M.t as p [@@attr]

    (* some comment *)
    method! x = 2 [@@attr]

    method virtual x : t

    method virtual private x : t

    method! private x = 3

    method! private x : t = 4

    method! private x : type a b c. r = 5

    method! private x : type a. r = 6

    val virtual x : t

    val virtual mutable x : t

    val virtual mutable x : t

    val! mutable x = 7

    val! mutable x : t = 8

    constraint t = 'a t

    [%%ext salut, "hello"]

    [@@@attr]

    initializer f x ; 9

    method x =
      let f = {<a; b = e>} in
      x <- expr

    method x : type a b c. (a, b) t -> c =
      let f = {<a; b = e>} in
      x <- expr

    method x : (a, b) t -> c =
      let f =
        {< a
        ;  b = something very
                 loooooooooooooooooooooooooooooooooooooooooooooooong
        >}
      in
      x <-
        something very
          looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong
  end

let _ = f a#b (a#c x y)

let _ = f a##.b (a##c x y)

type t = (int, int) #q

let _ = object%js end

let _ = object%js (super) end

let _ = object%js (super: 'a) end

let _ = f (object end)

let _ = f (object%js end)

class t ~a =
  object
    inherit f a

    method x a b = a + b
  end

class type mapper = [%test]

module type A = sig
  class mapper :
    int
    -> x:int
    -> ?y:int
    -> object
         method xxxxxxxxxxxxxxxxxxxxxxxxxxx : int
       end

  class tttttttttttt :
    aaaaaaaaaaaaaaaaaa:int
    -> bbbbbbbbbbbbbbbbbbbbb:float
    -> cccccccccccccccccccc

  class c :
    object
      inherit ['a a] d

      constraint 'a = int

      [%%ext something]

      [@@@attr something]

      val virtual mutable a : int

      method virtual private b : int -> int -> int
    end
end

class type mapper =
  let open Modl1 in
  object
    method expression : Javascript.expression -> Javascript.expression

    method expression_o :
      Javascript.expression option -> Javascript.expression option

    method switch_case :
         Javascript.expression
      -> Javascript.expression
      -> a
      -> b
      -> ccccccccccc
      -> d
      -> e
  end

class tttttttttttttttttttttttttt ~aaaaaaaaaaaaaaaaaaaaaaaaaaaa
  bbbbbbbbbbbbbbbbbbbbb =
  object
    inherit f a

    method x a b = a + b
  end

class tttttttttttttttttttttttttt x y =
  let open Mod in
  let x = 2 in
  let f x =
    object
      inherit f a

      method x a b = a + b
    end
  in
  f 0

class tttttttttttttttttttttttttt x y =
  let open Mod in
  let x = 2 in
  (fun x ->
    object
      inherit f a

      method x a b = a + b
    end )
    0

class c =
  object
    (** about a *)
    method a : type a b c. d -> e -> f = g

    (** floatting *)

    (** about a *)
    method a : 'a. d -> e -> f = g
  end

(** about a *)
class a = object end

(** floatting *)

and b = object end
 (** about b *)

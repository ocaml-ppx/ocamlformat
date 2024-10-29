val foo :
  ('k
   : immediate64 mod mode) 'cmp.
     (module S
        with type Id_and_repr.t = 'k
         and type Id_and_repr.comparator_witness = 'cmp )
  -> 'k Jane_symbol.Map.t
  -> ('k, Sockaddr.t, 'cmp) Map.t

type ttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt : value mod mode 

type  t_value  :  value mod mode 

type t_imm
     : immediate mod mode 

type t_imm64 :
       immediate64

type t_float64 : float64 mod mode 

type t_any : any mod mode 

type t_void : void mod mode 

(***************************************)
(* Test 1: annotation on type variable *)

let x : int as ('a : value mod mode) = 5

let x : int as ('a : immediate mod mode) = 5

let x : int as ('a : any mod mode) = 5

let x : int as ('a : float64 mod mode) = 5

let x : (int as ('a : immediate mod mode)) list as ('b : value mod mode) = [3; 4; 5]

let x : int list as ('a : immediate mod mode) = [3; 4; 5]

(****************************************)
(* Test 2: Annotation on type parameter *)

type ('a : immediate mod mode) t2_imm

type (_ : immediate mod mode) t2_imm'

type t1 = int t2_imm

type t2 = bool t2_imm

type ('a : float64 mod mode) t2_float64

type (_ : float64 mod mode) t2_float64'

type t3 = float# t2_float64

module M1 : sig
  type ('a : immediate mod mode) t
end = struct
  type (_ : immediate mod mode) t
end

module M2 : sig
  type (_ : immediate mod mode) t
end = struct
  type ('a : immediate mod mode) t
end

type t = string t2_imm

let f : 'a t2_imm -> 'a t2_imm = fun x -> x

let f : ('a : immediate mod mode) t2_imm -> ('a : value mod mode) t2_imm = fun x -> x

let f : ('a : value mod mode) t2_imm -> ('a : value mod mode) t2_imm = fun x -> x

let f : ('a : immediate mod mode). 'a t2_imm -> 'a t2_imm = fun x -> x

let f : ('a : value mod mode). 'a t2_imm -> 'a t2_imm = fun x -> x

type 'a t = 'a t2_imm

type ('a : value mod mode) t = 'a t2_imm

type ('a : immediate mod mode) t = 'a t2_imm

let f : (_ : value mod mode) t2_imm -> unit = fun _ -> ()

let g : (_ : immediate mod mode) t2_imm -> unit = fun _ -> ()

let f : (_ : immediate mod mode) -> unit = fun _ -> ()

let g : (_ : value mod mode) -> unit = fun _ -> ()

let f : (_ : immediate mod mode) -> (_ : value mod mode) = fun _ -> assert false

let g : (_ : value mod mode) -> (_ : immediate mod mode) = fun _ -> assert false

type ('a : any mod mode , 'b : any mod mode , 'c : any mod mode) t4

type 'a t5 = ('a : float64 mod mode , int, bool) t4

let f : ('a, _ : value mod mode , bool) t4 -> int = fun _ -> 42;;

type ('a, 'b, 'c) t6 = ('a, 'b, 'c : bits32 mod mode) t4;;

(********************************************)
(* Test 3: Annotation on types in functions *)

let f : ('a : any mod mode) -> 'a = fun x -> x

let f : ('a : any mod mode). 'a -> 'a = fun x -> x

let f : ('a : float64 mod mode). 'a -> 'a = fun x -> x

(********************************************)
(* Test 4: Annotation on record field types *)

type r = {field: ('a : immediate mod mode). 'a -> 'a}

let f {field} = field 5

type rf = {fieldf: ('a : float64 mod mode). 'a -> 'a}

let f {fieldf} = fieldf (Stdlib__Float_u.of_float 3.14)

let f {field} = field "hello"

let r = {field= (fun x -> x)}

let r = {field= Fun.id}

let r = {field= (fun (type a : immediate mod mode) (x : a) -> x)}

let r = {field= (fun (type a : value mod mode) (x : a) -> x)}

type r_value = {field: 'a. 'a -> 'a}

let r = {field= (fun (type a : immediate mod mode) (x : a) -> x)}

(* CR layouts v1.5: that's a pretty awful error message *)

type ('a : immediate mod mode) t_imm

type s = {f: ('a : value mod mode). 'a -> 'a u}

and 'a u = 'a t_imm

(* CR layouts v1.5: the location on that message is wrong. But it's hard to
   improve, because it comes from re-checking typedtree, where we don't have
   locations any more. I conjecture the same location problem exists when
   constraints aren't satisfied. *)

(********************)
(* Test 5: newtypes *)

let f (type a : value mod mode) (x : a) = x

let f (type a : immediate mod mode) (x : a) = x

let f (type a : float64 mod mode) (x : a) = x

let f (type a : any mod mode) (x : a) = x

(****************************************)
(* Test 6: abstract universal variables *)

let f : type (a : value mod mode). a -> a = fun x -> x

let f : type (a : immediate mod mode). a -> a = fun x -> x

let f : type (a : float64 mod mode). a -> a = fun x -> x

let f : type (a : any mod mode). a -> a = fun x -> x

(**************************************************)
(* Test 7: Defaulting universal variable to value *)

module type S = sig
  val f : 'a. 'a t2_imm -> 'a t2_imm
end

let f : 'a. 'a t2_imm -> 'a t2_imm = fun x -> x

(********************************************)
(* Test 8: Annotation on universal variable *)

module type S = sig
  val f : ('a : value mod mode). 'a t2_imm -> 'a t2_imm
end

module type S = sig
  val f : 'a t2_imm -> 'a t2_imm

  val g : ('a : immediate mod mode). 'a t2_imm -> 'a t2_imm
end

module type S = sig
  val f : 'a t2_float64 -> 'a t2_float64

  val g : ('a : float64 mod mode). 'a t2_float64 -> 'a t2_float64
end

(************************************************************)
(* Test 9: Annotation on universal in polymorphic parameter *)

let f (x : ('a : immediate mod mode). 'a -> 'a) = x "string"

(**************************************)
(* Test 10: Parsing & pretty-printing *)

let f (type a : immediate mod mode) (x : a) = x

let f (type a : immediate mod mode) (x : a) = x

let f (type a : value mod mode) (x : a) = x

let o =
  object
    method m : type (a : immediate mod mode). a -> a = fun x -> x
  end

let f : type (a : immediate mod mode). a -> a = fun x -> x

let f x =
  let local_ g (type a : immediate mod mode) (x : a) = x in
  g x [@nontail]

let f x y (type a : immediate mod mode) (z : a) = z

let f x y (type a : immediate mod mode) (z : a) = z

external f : ('a : immediate mod mode). 'a -> 'a = "%identity"

type (_ : any mod mode) t2_any

exception E : ('a : immediate mod mode) ('b : any mod mode). 'b t2_any * 'a list -> exn

let f (x : ('a : immediate mod mode). 'a -> 'a) = (x 3, x true)

type _ a = Mk : [> ] * ('a : immediate mod mode) -> int a

module type S = sig
  type _ a = Mk : [> ] * ('a : immediate mod mode) -> int a

  val f_imm : ('a : immediate mod mode) ('b : value mod mode). 'a -> 'a

  val f_val : ('a : value mod mode). 'a -> 'a

  type (_ : value) g = MkG : ('a : immediate mod mode). 'a g

  type t = int as (_ : immediate mod mode)
end

let f_imm : ('a : immediate mod mode). 'a -> 'a = fun x -> x

let f_val : ('a : value mod mode). 'a -> 'a = fun x -> f_imm x

type (_ : value mod mode) g = MkG : ('a : immediate mod mode). 'a g

type t = int as (_ : immediate mod mode)

type t = (('a : value mod mode), ('b : value mod mode)) t2

type ('a, 'b) t = ('a : value mod mode) * ('b : value mod mode)

class c : object
  method m : ('a : immediate mod mode). 'a -> 'a

  val f : ('a : immediate mod mode) -> 'a
end =
  object
    method m : type (a : immediate mod mode). a -> a = fun x -> x

    val f = fun (x : ('a : immediate mod mode)) -> x
  end

type _ g = MkG : ('a : immediate mod mode) ('b : void mod mode). 'a -> 'b g

type ('a : void mod mode) t3 = ..

type _ t3 += MkG : ('a : immediate mod mode) 'b. 'a -> 'b t3

let f_gadt : ('a : value mod mode). 'a -> 'a g -> 'a = fun x MkG -> f_imm x

(* comments *)
val foo :
  ((* comment 1 *) 'k (* comment 2 *) : (* comment 3 *) immediate64
  (* comment 4 *)) (* comment 5 *)
  'cmp.
     (module S
        with type Id_and_repr.t = 'k
         and type Id_and_repr.comparator_witness = 'cmp )
  -> 'k Jane_symbol.Map.t
  -> ('k, Sockaddr.t, 'cmp) Map.t

type a =
  b (* comment 0 *)
  as
  ((* comment 1 *)
  'k
  (* comment 2 *)
  :
  (* comment 3 *)
  immediate64
  (* comment 4 *)
  mod
  (* comment 5 *)
  mode
  (* comment 6 *)
)
(* comment 7 *)

let f (type a : immediate mod mode) x = x

let f
    (type (a : immediate mod mode) b c d e f g h i j k l m n o p q r s t u v w x y z)
    x =
  x

let f (type (a : immediate mod mode) b) x = x

let f (type a (b : immediate mod mode)) x = x

let f (type (a : immediate mod mode) (b : immediate mod mode)) x = x

module type S = sig
  val init_with_immediates :
    ('a : immediate mod mode) ('b : immediate mod mode).
    int -> f:local_ (int -> local_ 'a) -> local_ 'a t
end

(**************************************)
(* Test 11: Arbitrary strings as layout names *)

type t_asdf : asdf mod mode 

let x : int as ('a : some_layout mod mode) = 5

let f : ('a : alayout mod mode). 'a t -> 'a t = fun x -> x

let _ : _ =
  [%str
    let%lpoly rec fold (type (a : poly mod mode) acc) (xs : a list) ~(init : acc) ~f =
      match xs with [] -> init | x :: xs -> fold xs ~init:(f init x) ~f
    [@@layout (poly : value bits64), (acc : value bits64)]]

(**********************************************)
(* Test 12: annotated quantification in gadts *)

type t = T : ('a : value mod mode) 'b ('c : float64 mod mode) 'd . 'a * 'b * 'c * 'd -> t

type t = T : ('a : value mod mode) 'b ('c : float64 mod mode) 'd . { x : 'a * 'b * 'c * 'd } -> t

type t =
  | T : (* 1 *) ('a : value mod mode) 'b (* 2 *) ('c : (* 3 *) float64 mod mode) 'd . (* 4 *) 'a * 'b * 'c * 'd -> t

type t =
  | T : (* 1 *) ('a : value mod mode) 'b (* 2 *) ('c : (* 3 *) float64 (* 4 *) mod mode) 'd . (* 5 *) { x : 'a * 'b * 'c * 'd } -> t


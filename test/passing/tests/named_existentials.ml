let ok1 = function Dyn (type a) ((w, x) : a ty * a) -> ignore (x : a)

let ok2 = function Dyn (type a) ((w, x) : _ * a) -> ignore (x : a)

type u = C : 'a * ('a -> 'b list) -> u

let f = function C (type a b) ((x, f) : _ * (a -> b list)) -> ignore (x : a)

(* with GADT unification *)
type _ expr =
  | Int : int -> int expr
  | Add : (int -> int -> int) expr
  | App : ('a -> 'b) expr * 'a expr -> 'b expr

let rec eval : type t. t expr -> t = function
  | Int n -> n
  | Add -> ( + )
  | App (type a) ((f, x) : _ * a expr) -> eval f (eval x : a)

(* Also allow annotations on multiary constructors *)
type ('a, 'b) pair = Pair of 'a * 'b

let f = function Pair ((x, y) : int * _) -> x + y

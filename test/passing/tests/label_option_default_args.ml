let f x = e

let (* 0 *) f (* 1 *) x (* 2 *) = (* 3 *) e

let f ~x = e

let (* 0 *) f (* 1 *) ~x (* 2 *) = (* 3 *) e

let f ~(x : t) = e

let (* 0 *) f (* 1 *) ~((* 2 *) x (* 3 *) : (* 4 *) t (* 5 *)) (* 6 *) = (* 7 *) e

let f ~l:x = e

let (* 0 *) f (* 1 *) ~l: (* 2 *) x (* 3 *) = (* 4 *) e

let f ~l:{f; g} = e

let (* 0 *) f (* 1 *) ~l: (* 2 *) { (* 3 *) f (* 4 *); (* 5 *) g (* 6 *)} (* 7 *) = e

let f ~x:({f; g} as x) = e

let (* 0 *) f (* 1 *) ~x:((* 2 *) {f; g} (* 3 *) as (* 4 *) x (* 5 *)) (* 6 *) = e

let f ?x = e

let (* 0 *) f (* 1 *) ?(* 2 *)x (* 3 *) = e

let f ?(x : t) = e

let (* 0 *) f (* 1 *) ?((* 2 *) x (* 3 *) : (* 4 *) t (* 5 *)) (* 6 *) = e

let f ?l:x = e

let (* 0 *) f (* 1 *) ?l:(* 2 *) x (* 3 *) = e

let f ?l:(C x) = e

let (* 0 *) f (* 1 *) ?l: (* 2 *) ((* 3 *) C (* 4 *) x (* 5 *)) (* 6 *) = e

let f ?(x= d) = e

let (* 0 *) f (* 1 *) ?((* 2 *) x (* 3 *) = (* 4 *) d (* 5 *)) (* 6 *) = e

let f ?(x : t = d) = e

let (* 0 *) f (* 1 *) ?((* 2 *) x (* 3 *) : (* 4 *) t (* 5 *) = (* 6 *) d (* 7 *)) (* 8 *) = e

let f ?(x= (d : t)) = e

let (* 0 *) f (* 1 *) ?((* 2 *) x (* 3 *) = (* 4 *) ((* 5 *) d (* 6 *) : (* 7 *) t (* 8 *)) (* 9 *)) (* 10 *) = e

let f ?l:(x = d) = e

let f ?l:(x = (d : t)) = e

let f ?l:(x : t = d) = e

let (* 0 *) f (* 1 *) ?l: (* 2 *) ((* 3 *) x (* 4 *) : (* 5 *) t (* 6 *) = (* 7 *) d (* 8 *)) (* 9 *) = e

let f ?l:(C x = d) = e

let (* 0 *) f (* 1 *) ?l: (* 2 *) ((* 3 *) C (* 4 *) x (* 5 *) = (* 6 *) d (* 7 *)) (* 8 *) = e

(* Regression tests for https://github.com/ocaml-ppx/ocamlformat/issues/1260
   (optional argument rebound to non-variable without necessary parens). *)

(* Safe without parens *)
let f ?any:_ = ()

let f ?var:a = ()

(* Requires parens *)
let f ?alias:(_ as b) = ()

let f ?constant:(0) = ()

let f ?interval:('a' .. 'z') = ()

let f ?tuple:((1, 2)) = ()

let f ?construct1:(A) ?construct2:(()) ?construct3:(Some ()) = ()

let f ?variant:(`A ()) = ()

let f ?record:({a; b}) = ()

let f ?array:([| 1; 2; 3 |]) = ()

let f ?or_:(Some () | None) = ()

let f ?constraint_:(() : unit) = ()

let f ?type_:(#tconst) = ()

let f ?lazy_:(lazy ()) = ()

let f ?extension:([%ext]) = ()

let f ?open_:(Int.(zero)) = ()

(* Requires two pairs of parens *)
let f ?unpack:((module P)) = ()

(* May need extra parens to handle attributes *)
let f ?any:(_ [@attr]) = ()

let f ?constant:(0 [@attr]) = ()

let f ?open_:(Int.(zero) [@attr]) = ()

let f ?or_:((Some () | None) [@attr]) = ()

let f ?unpack:((module P) [@attr]) = ()

let f ?tuple:((1, 2) [@attr]) = ()

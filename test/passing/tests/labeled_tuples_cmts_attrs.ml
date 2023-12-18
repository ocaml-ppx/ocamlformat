(* Tests making sure comments and attributes are handled reasonably by labeled tuple
   printing.  This test has examples where the comments stay in exactly the same place -
   see [labeled_tuples_cmts_attrs_move.ml] for examples where we allow ourselves to
   slightly shift a comment or add a pun.  *)

(* Attrs around expressions *)
let y = ~z, ~z:(z [@attr])
let y = ~z, (z [@attr])
let y = ~z, ~z [@@attr]
let y = ~z:((42 [@attr]) : int), 42

(* Comments around expressions *)
let _ = (* baz *) ~z:42, ~y
let _ = ~z:(* baz *) 42, ~y
let _ = ~z:42 (* baz *), ~y
let _ = ~z:42, (* baz *) ~y
let _ = ~z:42, ~y (* baz *)
let _ = (* baz *) ~z, ~(y : int)
let _ = ~z (* baz *), ~(y : int)
let _ = ~z, ~(* baz *) (y : int)
let _ = ~z, ~((* baz *) y : int)
let _ = ~z, ~(y : (* baz *) int)
let _ = ~z, ~(y : int (* baz *))
let _ = ~z, ~(y : int) (* baz *)

(* Attrs around types *)
type t = z:(int[@attr]) * y:bool
type t = (z:int * y:bool[@attr])
type t = z:int * y:(bool[@attr])
type t = z:int * y:bool [@@attr]

(* Comments around types *)
type t = (* baz *) z:int * y:bool
type t = z (* baz *):int * y:bool
type t = z:(* baz *) int * y:bool
type t = z:int (* baz *) * y:bool
type t = z:int * (* baz *) y:bool
type t = z:int * y (* baz *):bool
type t = z:int * y:(* baz *) bool
type t = z:int * y:bool (* baz *)

(* Attrs around patterns *)
let ~z:(z [@attr]), ~y = ()
let ~z, ~y:(42 [@attr]) = ()
let ((~z, ~y:42) [@attr]) = ()

(* Comments around patterns *)
let (* baz *) ~z, ~y = ()
let ~z (* baz *), ~y = ()
let ~z, (* baz *) ~y = ()
let ~z, ~y (* baz *) = ()
let (* baz *) ~z:42, ~(y : int) = ()
let ~z:(* baz *) 42, ~(y : int) = ()
let ~z:42 (* baz *), ~(y : int) = ()
let ~z:42, ~((* baz *) y : int) = ()
let ~z:42, ~(y : (* baz *) int) = ()
let ~z:42, ~(y : int (* baz *)) = ()
let ~z:42, ~(y : int) (* baz *) = ()

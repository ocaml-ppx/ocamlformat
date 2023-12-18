(* Tests making sure comments and attributes are handled reasonably by labeled tuple
   printing.  This test has where we allow ourselves to slightly move a comment or add a
   pun or parens that aren't strictly necessary.  These examples are all analagous to
   similar cases with other forms (e.g., labeled arguments).  See
   [labeled_tuples_cmts_attrs.ml] for examples that stay exactly the same after
   formatting. *)

(* Attrs around expressions *)
let y = ~z, z [@attr]
let y = ~z, ~z:z [@@attr]
let y = ~z:(42[@attr]:int), 42

(* Comments around expressions *)
let _ = ~(* baz *)z, ~(y : int)
let _ = ~z, (* baz *)~(y : int)
let _ = ~z, ~(y(* baz *) : int)

(* Attrs around types *)
type t = z:int * y:bool[@attr]

(* Comments around patterns *)
let ~(* baz *)z, ~y = ()
let ~z, ~(* baz *)y = ()
let ~z:42, (* baz *)~(y : int) = ()
let ~z:42, ~(* baz *)(y : int) = ()
let ~z:42, ~(y(* baz *) : int) = ()

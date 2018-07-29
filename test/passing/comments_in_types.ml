type t1 =
  S.s                           (* comment attached to t1 *)
type t2 =
  s                             (* comment attached to t2 *)
type t3 =
    S                           (* comment attached to t3 *)
type t4 =
  | S                                   (* comment attached to t4 *)



(* comment attached to the list *)
let _ =
  [ x
  ; y
  ]


type t5 = int              (*  comment attached to t5 *)

type t6 = X of int (*  comment attached to X *)  | Y of float   (* comment attached to Y *)
	    | Q of int    (* comment attached to X *) | P of string (* comment attached to P *)
	    | W
(* comment attached to N *)
	    | N of char             (* comment attached to t6 *)

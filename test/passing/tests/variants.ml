type t =
  [ (* xx *) `(* yy *) A (* zz *)
  | (* xx *) `B (* zz *)
  | `(* yy *) C (* zz *) ]

let (* xx *) `(* yy *) A (* zz *) = x

let (* xx *) `B (* zz *) = x

let `(* yy *) C (* zz *) = x

let _ = (* xx *) `(* yy *) A (* zz *)

let _ = (* xx *) `B (* zz *)

let _ = `(* yy *) C (* zz *)

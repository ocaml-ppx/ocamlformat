include Cmdliner

(** existential package of a Term and a setter for a ref to receive the
    parsed value *)
type arg = Arg : 'a Term.t * ('a -> unit) -> arg

(** convert a list of arg packages to a term for the tuple of all the arg
    terms, and apply it to a function that sets all the receiver refs *)
let tuple args =
  let pair (Arg (trm_x, set_x)) (Arg (trm_y, set_y)) =
    let trm_xy = Term.(const (fun a b -> (a, b)) $ trm_x $ trm_y) in
    let set_xy (a, b) = set_x a ; set_y b in
    Arg (trm_xy, set_xy)
  in
  let init = Arg (Term.const (), fun () -> ()) in
  let (Arg (trm, set)) = Base.List.fold_right ~f:pair args ~init in
  Term.app (Term.const set) trm

let args : arg list ref = ref []

let mk ~default arg =
  let var = ref default in
  let set x = var := x in
  args := Arg (arg, set) :: !args ;
  var

let parse info validate =
  Term.eval (Term.(ret (const validate $ tuple !args)), info)

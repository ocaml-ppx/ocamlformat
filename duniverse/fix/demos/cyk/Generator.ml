open Sigs

module Make (G : CNF) = struct

  open G

  (* Pick a random element out of a list. *)

  let weight n = function
    | E | T _ ->
	1 + n/12
    | NN _ ->
	1

  let rec measure n = function
    | [] ->
	0
    | prod :: prods ->
	weight n prod + measure n prods

  let rec extract n k = function
    | [] ->
	assert false
    | prod :: prods ->
	if k < weight n prod then
	  prod
	else
	  extract n (k - weight n prod) prods

  let pick n prods =
    extract n (Random.int (measure n prods)) prods

  (* Generate an expansion of [nt]. *)

  let rec generate nt n =
    match pick n (productions nt) with
    | E ->
        []
    | T t ->
        [ t ]
    | NN (nt1, nt2) ->
	  generate nt1 (n+1) @ generate nt2 (n+1)

  let generate nt n =
    Array.of_list (generate nt n)

end

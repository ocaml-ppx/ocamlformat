let _ =
  match x with
  | A -> begin match B with A -> fooooooooooooo end
  | A -> begin match B with A -> fooooooooooooo | B -> fooooooooooooo end
  | A -> begin
    match B with
    | A -> fooooooooooooo
    | B -> fooooooooooooo
    | C -> fooooooooooooo
    | D -> fooooooooooooo
  end
[@@ocamlformat "break-cases=fit"]

let _ =
  match x with
  | A -> begin match B with A -> fooooooooooooo end
  | A -> begin match B with A -> fooooooooooooo | B -> fooooooooooooo end
  | A -> begin
    match B with
    | A -> fooooooooooooo
    | B -> fooooooooooooo
    | C -> fooooooooooooo
    | D -> fooooooooooooo
  end
[@@ocamlformat "break-cases=nested"]

let _ =
  match x with
  | A -> begin match B with A -> fooooooooooooo end
  | A -> begin match B with A -> fooooooooooooo | B -> fooooooooooooo end
  | A -> begin
    match B with
    | A -> fooooooooooooo
    | B -> fooooooooooooo
    | C -> fooooooooooooo
    | D -> fooooooooooooo
  end
[@@ocamlformat "break-cases=toplevel"]

let _ =
  match x with
  | A -> begin match B with A -> fooooooooooooo end
  | A -> begin match B with A -> fooooooooooooo | B -> fooooooooooooo end
  | A -> begin
    match B with
    | A -> fooooooooooooo
    | B -> fooooooooooooo
    | C -> fooooooooooooo
    | D -> fooooooooooooo
  end
[@@ocamlformat "break-cases=fit-or-vertical"]

let _ =
  match x with
  | A -> begin match B with A -> fooooooooooooo end
  | A -> begin match B with A -> fooooooooooooo | B -> fooooooooooooo end
  | A -> begin
    match B with
    | A -> fooooooooooooo
    | B -> fooooooooooooo
    | C -> fooooooooooooo
    | D -> fooooooooooooo
  end
[@@ocamlformat "break-cases=all"]

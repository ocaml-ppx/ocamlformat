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

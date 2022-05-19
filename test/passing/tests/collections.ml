let _ =
  [
    a;
    b (* foooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo *)
  ]

let
  [ fooooooooooooooooooooooooooooooo;
    fooooooooooooooooooooooooooooooo;
    fooooooooooooooooooooooooooooooo;
  (* before end of the list *) ] =
  [ fooooooooooooooooooooooooooooooo;
    fooooooooooooooooooooooooooooooo;
    fooooooooooooooooooooooooooooooo;
  (* after all elements *)
  (* after all elements as well *) ]

let
  [| fooooooooooooooooooooooooooooooo;
    fooooooooooooooooooooooooooooooo;
    fooooooooooooooooooooooooooooooo;
  (* before end of the array *) |] =
  [| fooooooooooooooooooooooooooooooo;
    fooooooooooooooooooooooooooooooo;
    fooooooooooooooooooooooooooooooo;
  (* after all elements *)
  (* after all elements as well *) |]

let { fooooooooooooooooooooooooooooooo;
    fooooooooooooooooooooooooooooooo;
    fooooooooooooooooooooooooooooooo;
    _
    (* xxx *) } =
  { fooooooooooooooooooooooooooooooo= x;
    fooooooooooooooooooooooooooooooo= y;
    fooooooooooooooooooooooooooooooo= z;
  (* after all fields *) }

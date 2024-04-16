let () =
  Lwt_main.run
    begin
      let a = "a" in
      let b = "b" in
      let c = "c" in
      Lwt.return "test"
    end

let () =
  Lwt_main.run
    ( let a = "a" in
      let b = "b" in
      let c = "c" in
      Lwt.return "test" )

let () =
  List.iter begin fun v ->
    (* do a lot of things *)
    let a = "a" in
    let b = "b" in
    let c = "c" in
    ()
  end values

let () =
  List.iter
    (fun v ->
      (* do a lot of things *)
      let a = "a" in
      let b = "b" in
      let c = "c" in
      ())
    values

let () =
  foooooooo
    begin
      fooooooooooooo ;
      foooooooo foooooooooooo ;
      fooooooooooo foooooooooo ;
      foooooooooooooooo
    end

let () =
  foooooooo
    ( fooooooooooooo ;
      foooooooo foooooooooooo ;
      fooooooooooo foooooooooo ;
      foooooooooooooooo )

let () =
  foooooooo
    begin
      if foooooooooooooooooooooooooooo then
        if foooooooooooooooooooooooooooo then
          foooooooooooooooooo
        else foooooooooooooooooooooooooo
      else
        if foooooooooooooooooooooooooooooooo then
          foooooooooooooooooo
            begin
              if foooooooooooooooooooooooooooo then
                if foooooooooooooooooooooooooooo then foooooooooooooooooooooooo
                else foooooooooooooooooooooooooo
              else
              if foooooooooooooooooooooooooooooooo then
                fooooooooooooooooooooooooooooooooooo
              else
              if foooooooooooooooooo then foooooooooooooooooooooooooooooooooo
              else fooooooooooooooooooooo
            end
        else
          if foooooooooooooooooo then foooooooooooooooooooooooooooooooooo
          else fooooooooooooooooooooo
    end

let () =
  foooooooo
    ( if foooooooooooooooooooooooooooo then
        if foooooooooooooooooooooooooooo then foooooooooooooooooooooooo
        else
          foooooooooooooooooooooooooooo
            ( if foooooooooooooooooooooooooooo then
                if foooooooooooooooooooooooooooo then foooooooooooooooooooooooo
                else foooooooooooooooooooooooooo
              else
              if foooooooooooooooooooooooooooooooo then
                fooooooooooooooooooooooooooooooooooo
              else
              if foooooooooooooooooo then foooooooooooooooooooooooooooooooooo
              else fooooooooooooooooooooo )
      else
        if foooooooooooooooooooooooooooooooo then
          fooooooooooooooooooooooooooooooooooo
        else
          if foooooooooooooooooo then foooooooooooooooooooooooooooooooooo
          else fooooooooooooooooooooo )

let _ = a |> let a = b in c
let _ = ( let a = b in c ) |> d
let _ = a := let a = b in c
let _ = ( let a = b in c ) := d
let _ = a + let a = b in c
let _ = ( let a = b in c ) + d
let _ = f ( let a = b in c )
let _ = ( let a = b in c ) d
let _ = a#f ( let a = b in c )
let _ = ( let a = b in c ) #f
let _ = A ( let a = b in c )
let _ = `A ( let a = b in c )
let _ = { x= ( let a = b in c ) }
let _ = { ( let a = b in c ) with a= b }
let _ = {< x = let a = b in c >}
let _ = x <- ( let a = b in c )
let _ = ( let a = b in c ) .x
let _ = ( let a = b in c ).x <- d
let _ = ( ( let a = b in c ) , d )
let _ = ( let a = b in c :> t )
let _ = let a = b in c :: d
let _ = a :: ( let a = b in c )
let _ = [ ( let a = b in c ) ]
let _ = [| ( let a = b in c ) |]

let () =
  if a then begin b
    (* asd *)
  end

[@@@ocamlformat "if-then-else=compact"]

let _ =
  if x
  then begin
    foo.fooooo <- Fooo.foo fooo foo.fooooo;
    Fooo fooo
  end
  else if y then begin f 0; f 2 end
  else begin
    foo.fooooo <- Fooo.foo fooo foo.fooooo;
    Fooo fooo
  end

let () =
  if a then begin b
    (* asd *)
  end

[@@@ocamlformat "if-then-else=fit-or-vertical"]

let _ =
  if x
  then begin
    foo.fooooo <- Fooo.foo fooo foo.fooooo;
    Fooo fooo
  end
  else if y then begin f 0; f 2 end
  else begin
    foo.fooooo <- Fooo.foo fooo foo.fooooo;
    Fooo fooo
  end

let () =
  if a then begin b
    (* asd *)
  end

[@@@ocamlformat "if-then-else=keyword-first"]

let _ =
  if x
  then begin
    foo.fooooo <- Fooo.foo fooo foo.fooooo;
    Fooo fooo
  end
  else if y then begin f 0; f 2 end
  else begin
    foo.fooooo <- Fooo.foo fooo foo.fooooo;
    Fooo fooo
  end

let () =
  if a then begin b
    (* asd *)
  end

[@@@ocamlformat "if-then-else=k-r"]

let _ =
  if x
  then begin
    foo.fooooo <- Fooo.foo fooo foo.fooooo;
    Fooo fooo
  end
  else if y then begin f 0; f 2 end
  else begin
    foo.fooooo <- Fooo.foo fooo foo.fooooo;
    Fooo fooo
  end

let _ =
  match x with
  | A -> begin
    match B with
    | A -> fooooooooooooo
  end
  | A -> begin
    match B with
    | A -> fooooooooooooo
    | B -> fooooooooooooo
  end
  | A -> begin
    match B with
    | A -> fooooooooooooo
    | B -> fooooooooooooo
    | C -> fooooooooooooo
    | D -> fooooooooooooo
  end

let () =
  begin
    add_test @@
    let test_name = "Test 1" in
    test_name >:: fun _ ->
      assert_equal "a" "a"
  end;
  begin
    add_test @@
    let test_name = "Test 2" in
    test_name >:: fun _ ->
      assert_equal "b" "b"
  end

let _ = begin end
let _ = begin (* foo *) end
let _ = begin%ext end
let _ = begin%ext (* foo *) end
let _ = begin x y end
let _ = begin (* foo *) x y end
let _ = begin%ext x y end
let _ = begin%ext (* foo *) x y end

let _ =
  begin[@landmark "parse_constant_dividends"]
    market_data_items := ()
  end

let () =
  if a then begin b
    (* asd *)
  end

let x =
  let get_path_and_distance pv1 pv2 =
    if is_loop pv1 pv2 then Some ([],0) else
      match Tbl.find dist_tbl (pv1, pv2) with
      | None ->
        (* FIXME: temporary hack to avoid Jane Street's annoying warnings. *)
        begin[@warning "-3"] try
          let path', dist = Dijkstra.shortest_path pgraph pv1 pv2 in
          let path = unwrap_path path' in
          Tbl.set dist_tbl ~key:(pv1, pv2) ~data:(path, dist);
          Some (path, dist)
        with Not_found | Not_found_s _ ->
          None
        end
      | pd -> pd
  in
  ()

let _ =
  if something_changed then begin[@attr]
    loop
  end

let _ =
  match x with
  | _ ->
      (* xxx *)
      begin y end

let _ =
  match x with
  | _ ->
      begin[@foo] y end

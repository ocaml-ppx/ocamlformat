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

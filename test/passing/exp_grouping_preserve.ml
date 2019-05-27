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

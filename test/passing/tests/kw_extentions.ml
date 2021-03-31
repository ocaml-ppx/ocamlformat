let _ =
  let%lwt foo = Lwt.return 1 in
  Lwt.return_unit

let _ =
  let%lwt foo = Lwt.return 1 in
  let%lwt bar = Lwt.return 1 in
  let%lwt baz = Lwt.return 1 in
  Lwt.return_unit

let () =
  if%ext true then () else () ;
  if%ext true then () else if true then () else () ;
  let%ext x = () in
  for%ext i = 1 to 10 do
    ()
  done ;
  while%ext false do
    ()
  done ;
  match%ext x with _ -> ()

let () =
  let%ext x = () in
  try%ext x with _ -> ()

let () =
  if%ext true then () else () ;
  if%ext true then () else if true then () else () ;
  if%ext true then () else ()

let () =
  (match%ext x with _ -> ()) ;
  match%ext x with _ -> ()

let () = () ; () ;%ext () ; () ;%ext ()

let _ =
  let%ext () = () and () = () in
  ()

let () =
  f (fun () -> ()) ;%ext
  f ()

let () =
  f (fun () -> ()) ;%ext
  g (fun () -> ()) ;
  h (fun () -> ()) ;%ext
  i () ;
  j () ;%ext
  f ()

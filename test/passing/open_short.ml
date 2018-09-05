let () =
  ( Term.(
      term_result
        (const Phases.phase1 $ arch $ hub_id $ build_dir $ logs_dir
       $ setup_logs))
  , Term.info "phase1" ~doc ~sdocs:Manpage.s_common_options ~exits ~man )

let () =
  Arg.(
    let doc = "Output all." in
    value & flag & info ["all"] ~doc)
  $ Arg.(
      let doc = "Commit to git." in
      value & flag & info ["commit"; "c"] ~doc)

let () = X.(f y i)

let () = X.(i)

let () =
  let open! K in
  x y z

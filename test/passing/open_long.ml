let () =
  ( (let open Term in
    term_result
      (const Phases.phase1 $ arch $ hub_id $ build_dir $ logs_dir
     $ setup_logs))
  , Term.info "phase1" ~doc ~sdocs:Manpage.s_common_options ~exits ~man )

let () =
  (let open Arg in
  let doc = "Output all." in
  value & flag & info ["all"] ~doc)
  $
  let open Arg in
  let doc = "Commit to git." in
  value & flag & info ["commit"; "c"] ~doc

let () =
  let open X in
  f y i

let () =
  let open X in
  i

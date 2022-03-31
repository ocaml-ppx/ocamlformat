module StringMap = Map.Make (String)

type kind = Structure | Signature | Use_file

type setup = {
  mutable has_ref : bool;
  mutable base_file : string option;
  kind : kind;
  dir : string;
}

let register_file dir kind tests fname =
  match String.split_on_char '.' fname with
  | test_name :: (("ml" | "mli" | "mlt") as ext) :: rest -> (
      let fname = dir ^ "/" ^ fname in
      let src_test_name = test_name ^ "." ^ ext in
      let setup =
        match StringMap.find src_test_name !tests with
        | setup -> setup
        | exception Not_found ->
            let s = { has_ref = false; base_file = None; kind; dir } in
            tests := StringMap.add src_test_name s !tests;
            s
      in
      match rest with
      | [] -> ()
      | [ "ref" ] -> setup.has_ref <- true
      | _ -> invalid_arg fname)
  (* ignore dune file, .foo.whatever.swp, etc *)
  | _ -> ()

let emit_test test_name setup =
  let out_name = test_name ^ ".stdout" in
  let ref_name =
    setup.dir ^ "/" ^ if setup.has_ref then test_name ^ ".ref" else test_name
  in
  let kind =
    match setup.kind with
    | Structure -> "-structure"
    | Signature -> "-signature"
    | Use_file -> "-use-file"
  in
  Printf.printf
    {|
(rule (action (with-stdout-to %s (run ./gen/driver.exe %s %%{dep:%s}))))
(rule (alias runtest) (action (diff %s %s)))
|}
    out_name kind
    (setup.dir ^ "/" ^ test_name)
    ref_name out_name

let read_dir map kind =
  let dir =
    match kind with
    | Structure -> "structure"
    | Signature -> "signature"
    | Use_file -> "use_file"
  in
  Sys.readdir ("./" ^ dir) |> Array.iter (register_file dir kind map)

let () =
  let map = ref StringMap.empty in
  read_dir map Structure;
  read_dir map Signature;
  read_dir map Use_file;
  StringMap.iter emit_test !map

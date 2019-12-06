module StringMap = Map.Make (String)

type setup =
  { mutable has_ref: bool
  ; mutable has_opts: bool
  ; mutable has_ocp: bool
  ; mutable base_file: string option
  ; mutable extra_deps: string list
  ; mutable should_fail: bool }

let read_lines fn =
  Stdio.In_channel.with_file fn ~f:Stdio.In_channel.input_lines

let add_test ?base_file map src_test_name =
  let s =
    { has_ref= false
    ; has_opts= false
    ; has_ocp= false
    ; base_file
    ; extra_deps= []
    ; should_fail= false }
  in
  map := StringMap.add src_test_name s !map ;
  s

let register_file tests fname =
  match String.split_on_char '.' fname with
  | test_name :: (("ml" | "mli" | "mlt") as ext) :: rest -> (
      let src_test_name = test_name ^ "." ^ ext in
      let setup =
        match StringMap.find src_test_name !tests with
        | setup -> setup
        | exception Not_found -> (
          (* foo_file-some_variant.ml should derive from foo_file.ml *)
          match String.index_opt test_name '-' with
          | None -> add_test tests src_test_name
          | Some i ->
              let base_file = String.sub test_name 0 i ^ "." ^ ext in
              add_test ~base_file tests src_test_name )
      in
      match rest with
      | [] -> ()
      | ["output"] | ["ocp"; "output"] -> ()
      | ["opts"] -> setup.has_opts <- true
      | ["ref"] -> setup.has_ref <- true
      | ["ocp"] -> setup.has_ocp <- true
      | ["deps"] -> setup.extra_deps <- read_lines fname
      | ["should-fail"] -> setup.should_fail <- true
      | _ -> invalid_arg fname )
  | _ -> ()

(* ignore dune file, .foo.whatever.swp, etc *)

let emit_test test_name setup =
  let open Printf in
  let opts =
    if setup.has_opts then sprintf " %%{read-lines:%s.opts}" test_name
    else ""
  in
  let ref_name = if setup.has_ref then test_name ^ ".ref" else test_name in
  let base_test_name =
    match setup.base_file with Some n -> n | None -> test_name
  in
  let extra_deps = String.concat " " setup.extra_deps in
  let cmd_prefix = if setup.should_fail then "! " else "" in
  Printf.printf
    {|
(rule
 (targets %s.output)
 (deps .ocamlformat %s)
 (action
   (with-outputs-to %%{targets}
     (system "%s%%{bin:ocamlformat}%s %%{dep:%s}"))))

(alias
 (name runtest)
 (action (diff %s %s.output)))
|}
    test_name extra_deps cmd_prefix opts base_test_name ref_name test_name ;
  if setup.has_ocp then
    Printf.printf
      {|
(rule
 (targets %s.ocp.output)
 (deps .ocamlformat %s)
 (action
   (with-outputs-to %%{targets}
     (system "%s%%{bin:ocp-indent} %%{dep:%s}"))))

(alias
 (name runtest)
 (action (diff %s.ocp %s.ocp.output)))
|}
      test_name extra_deps cmd_prefix ref_name test_name test_name

let () =
  let map = ref StringMap.empty in
  Sys.readdir "." |> Array.iter (register_file map) ;
  StringMap.iter emit_test !map

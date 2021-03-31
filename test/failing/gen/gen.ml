module StringMap = Map.Make (String)

type setup =
  { mutable has_ref: bool
  ; mutable has_opts: bool
  ; mutable has_ocp: bool
  ; mutable base_file: string option
  ; mutable extra_deps: string list
  ; mutable minimum_ocaml_version: string option }

let read_lines file =
  let ic = Stdlib.open_in file in
  let rec aux acc =
    match Stdlib.input_line ic with
    | exception End_of_file -> Stdlib.close_in ic ; List.rev acc
    | line -> aux (line :: acc)
  in
  aux []

let read_file file =
  let ic = Stdlib.open_in file in
  let buf = Buffer.create 32 in
  let rec aux buf =
    match Stdlib.input_line ic with
    | exception End_of_file ->
        Stdlib.close_in ic ;
        let ret = Buffer.contents buf |> String.trim in
        Buffer.clear buf ; ret
    | line ->
        Buffer.add_string buf line ;
        aux buf
  in
  aux buf

let add_test ?base_file map src_test_name =
  let s =
    { has_ref= false
    ; has_opts= false
    ; has_ocp= false
    ; base_file
    ; extra_deps= []
    ; minimum_ocaml_version= None }
  in
  map := StringMap.add src_test_name s !map ;
  s

let register_file tests fname =
  match String.split_on_char '.' fname with
  | test_name :: (("ml" | "mli" | "mlt" | "eliom" | "eliomi") as ext) :: rest
    -> (
      let fname = "tests/" ^ fname in
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
      | ["broken-ref"] -> setup.has_ref <- true
      | ["ocp"] -> setup.has_ocp <- true
      | ["deps"] -> setup.extra_deps <- read_lines fname
      | ["minimum-ocaml-version"] ->
          setup.minimum_ocaml_version <- Some (read_file fname)
      | _ -> invalid_arg fname )
  | _ -> ()

(* ignore dune file, .foo.whatever.swp, etc *)

let cmd args =
  let cmd_string = String.concat " " args in
  Printf.sprintf {|(with-accepted-exit-codes 1
       (run %s))|} cmd_string

let emit_test test_name setup =
  let opts =
    if setup.has_opts then
      read_lines (Printf.sprintf "tests/%s.opts" test_name)
    else []
  in
  let ref_name =
    "tests/" ^ if setup.has_ref then test_name ^ ".broken-ref" else test_name
  in
  let base_test_name =
    "tests/" ^ match setup.base_file with Some n -> n | None -> test_name
  in
  let extra_deps = String.concat " " setup.extra_deps in
  let enabled_if_line =
    match setup.minimum_ocaml_version with
    | None -> ""
    | Some v -> Printf.sprintf "\n (enabled_if (>= %%{ocaml_version} %s))" v
  in
  Printf.printf
    {|
(rule
 (deps tests/.ocamlformat %s)%s
 (package ocamlformat)
 (action
   (with-outputs-to %s.output
     %s)))

(rule
 (alias runtest)%s
 (package ocamlformat)
 (action (diff %s %s.output)))
|}
    extra_deps enabled_if_line test_name
    (cmd
       ( ["%{bin:ocamlformat}"] @ opts
       @ [Printf.sprintf "%%{dep:%s}" base_test_name] ) )
    enabled_if_line ref_name test_name ;
  if setup.has_ocp then
    Printf.printf
      {|
(rule
 (deps tests/.ocp-indent %s)%s
 (package ocamlformat)
 (action
   (with-outputs-to %s.ocp.output
     %s)))

(rule
 (alias runtest)%s
 (package ocamlformat)
 (action (diff tests/%s.ocp %s.ocp.output)))
|}
      extra_deps enabled_if_line test_name
      (cmd ["%{bin:ocp-indent}"; Printf.sprintf "%%{dep:%s}" ref_name])
      enabled_if_line test_name test_name

let () =
  let map = ref StringMap.empty in
  Sys.readdir "./tests" |> Array.iter (register_file map) ;
  StringMap.iter emit_test !map

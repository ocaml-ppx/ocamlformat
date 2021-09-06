module StringMap = Map.Make (String)

type setup =
  { mutable has_ref: bool
  ; mutable has_opts: bool
  ; mutable has_ocp: bool
  ; mutable base_file: string option
  ; mutable extra_deps: string list
  ; mutable should_fail: bool
  ; mutable enabled_if: string option }

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
    ; should_fail= false
    ; enabled_if= None }
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
      | ["ref"] -> setup.has_ref <- true
      | ["ocp"] -> setup.has_ocp <- true
      | ["deps"] -> setup.extra_deps <- read_lines fname
      | ["should-fail"] -> setup.should_fail <- true
      | ["enabled-if"] -> setup.enabled_if <- Some (read_file fname)
      | ["err"] -> ()
      | _ -> invalid_arg fname )
  | _ -> ()

(* ignore dune file, .foo.whatever.swp, etc *)

let cmd should_fail args =
  let cmd_string = String.concat " " args in
  if should_fail then
    Printf.sprintf
      {|(with-accepted-exit-codes 1
       (run %s))|}
      cmd_string
  else Printf.sprintf {|(run %s)|} cmd_string

let emit_test test_name setup =
  let opts =
    "--margin-check"
    ::
    ( if setup.has_opts then
      read_lines (Printf.sprintf "tests/%s.opts" test_name)
    else [] )
  in
  let ref_name =
    "tests/" ^ if setup.has_ref then test_name ^ ".ref" else test_name
  in
  let err_name = "tests/" ^ test_name ^ ".err" in
  let base_test_name =
    "tests/" ^ match setup.base_file with Some n -> n | None -> test_name
  in
  let extra_deps = String.concat " " setup.extra_deps in
  let enabled_if_line =
    match setup.enabled_if with
    | None -> ""
    | Some clause -> Printf.sprintf "\n (enabled_if %s)" clause
  in
  Printf.printf
    {|
(rule
 (deps tests/.ocamlformat %s)%s
 (package ocamlformat)
 (action
  (with-stdout-to %s.stdout
   (with-stderr-to %s.stderr
     %s))))

(rule
 (alias runtest)%s
 (package ocamlformat)
 (action (diff %s %s.stdout)))

(rule
 (alias runtest)%s
 (package ocamlformat)
 (action (diff %s %s.stderr)))
|}
    extra_deps enabled_if_line test_name test_name
    (cmd setup.should_fail
       ( ["%{bin:ocamlformat}"] @ opts
       @ [Printf.sprintf "%%{dep:%s}" base_test_name] ) )
    enabled_if_line ref_name test_name enabled_if_line err_name test_name ;
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
      (cmd setup.should_fail
         ["%{bin:ocp-indent}"; Printf.sprintf "%%{dep:%s}" ref_name] )
      enabled_if_line test_name test_name

let () =
  let map = ref StringMap.empty in
  Sys.readdir "./tests" |> Array.iter (register_file map) ;
  StringMap.iter emit_test !map

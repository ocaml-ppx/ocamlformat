module StringMap = Map.Make (String)

let spf = Printf.sprintf

let dep fname = spf "%%{dep:%s}" fname

type setup =
  { mutable has_ref: bool
  ; mutable has_js_ref: bool
  ; mutable has_why_no_js: bool
  ; mutable has_opts: bool
  ; mutable has_js_opts: bool
  ; mutable has_ocp: bool
  ; mutable ocp_opts: string list
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
    ; has_js_ref= false
    ; has_why_no_js= false
    ; has_opts= false
    ; has_js_opts= false
    ; has_ocp= false
    ; ocp_opts= []
    ; base_file
    ; extra_deps= []
    ; should_fail= false
    ; enabled_if= None }
  in
  map := StringMap.add src_test_name s !map ;
  s

let register_file tests fname =
  match String.split_on_char '.' fname with
  | test_name
    :: (("ml" | "mli" | "mlt" | "mld" | "eliom" | "eliomi") as ext)
    :: rest -> (
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
      | ["js-opts"] -> setup.has_js_opts <- true
      | ["ref"] -> setup.has_ref <- true
      | ["js-ref"] -> setup.has_js_ref <- true
      | ["why-no-js"] -> setup.has_why_no_js <- true
      | ["ocp"] -> setup.has_ocp <- true
      | ["ocp-opts"] -> setup.ocp_opts <- read_lines fname
      | ["deps"] -> setup.extra_deps <- read_lines fname
      | ["should-fail"] -> setup.should_fail <- true
      | ["enabled-if"] -> setup.enabled_if <- Some (read_file fname)
      | [("err" | "js-err")] -> ()
      | _ -> invalid_arg fname )
  | _ -> ()

(* ignore dune file, .foo.whatever.swp, etc *)

let cmd should_fail args =
  let cmd_string = String.concat " " args in
  if should_fail then
    spf {|(with-accepted-exit-codes 1
       (run %s))|} cmd_string
  else spf {|(run %s)|} cmd_string

let one_styling_test ~extra_deps ~enabled_if_line ~test_name ~base_test_name
    ~should_fail ~opts ~output_name ~extra_suffix =
  Printf.sprintf
    {|
(rule
 (deps tests/.ocamlformat %s)%s
 (package ocamlformat)
 (action
  (with-stdout-to %s.%sstdout
   (with-stderr-to %s.%sstderr
     %s))))

(rule
 (alias runtest)%s
 (package ocamlformat)
 (action (diff %s %s.%sstdout)))

(rule
 (alias runtest)%s
 (package ocamlformat)
 (action (diff tests/%s.%serr %s.%sstderr)))
|}
    extra_deps enabled_if_line test_name extra_suffix test_name extra_suffix
    (cmd should_fail (["%{bin:ocamlformat}"] @ opts @ [dep base_test_name]))
    enabled_if_line output_name test_name extra_suffix enabled_if_line
    test_name extra_suffix test_name extra_suffix

let one_js_coverage_test ~test_name ~has_js_ref ~has_why_no_js =
  let (err_msg : _ format option) =
    match (has_js_ref, has_why_no_js) with
    | true, true -> Some "%s has both a [.js-ref] and a [.why-no-js]!"
    | false, false -> Some "%s has neither a [.js-ref], nor a [.why-no-js]!"
    | true, false | false, true -> None
  in
  match err_msg with
  | Some err_msg ->
      let err_msg = Printf.sprintf err_msg test_name in
      Printf.sprintf
        {|
(rule
 (alias runtest)
 (enabled_if (<> %%{os_type} Win32))
 (package ocamlformat)
 (action (system "echo '%s'; exit 1")))
|}
        err_msg
  | None -> ""

let emit_test test_name setup =
  let opts =
    "--margin-check"
    ::
    ( if setup.has_opts then read_lines (spf "tests/%s.opts" test_name)
      else [] )
  in
  let js_opts =
    "--profile=janestreet" :: "--enable-outside-detected-project"
    :: "--disable-conf-files"
    ::
    ( if setup.has_js_opts then read_lines (spf "tests/%s.js-opts" test_name)
      else [] )
  in
  let ref_name =
    "tests/" ^ if setup.has_ref then test_name ^ ".ref" else test_name
  in
  let js_ref_name = "tests/" ^ test_name ^ ".js-ref" in
  let base_test_name =
    "tests/" ^ match setup.base_file with Some n -> n | None -> test_name
  in
  let extra_deps = String.concat " " setup.extra_deps in
  let enabled_if_line =
    match setup.enabled_if with
    | None -> ""
    | Some clause -> spf "\n (enabled_if %s)" clause
  in
  let output_fname = test_name ^ ".stdout" in
  one_styling_test ~extra_deps ~enabled_if_line ~test_name ~base_test_name
    ~should_fail:setup.should_fail ~opts ~output_name:ref_name
    ~extra_suffix:""
  |> print_string ;
  if setup.has_js_ref then
    one_styling_test ~extra_deps ~enabled_if_line ~test_name ~base_test_name
      ~should_fail:setup.should_fail ~opts:js_opts ~output_name:js_ref_name
      ~extra_suffix:"js-"
    |> print_string ;
  one_js_coverage_test ~test_name ~has_js_ref:setup.has_js_ref
    ~has_why_no_js:setup.has_why_no_js
  |> print_string ;
  if setup.has_ocp then
    let ocp_cmd =
      "%{bin:ocp-indent}" :: (setup.ocp_opts @ [dep output_fname])
    in
    let ocp_out_file = test_name ^ ".ocp.output" in
    Printf.printf
      {|
(rule
 (deps tests/.ocp-indent %s)%s
 (package ocamlformat)
 (action
   (with-outputs-to %s
     %s)))

(rule
 (alias runtest)%s
 (package ocamlformat)
 (action (diff tests/%s.ocp %s)))
|}
      extra_deps enabled_if_line ocp_out_file
      (cmd setup.should_fail ocp_cmd)
      enabled_if_line test_name ocp_out_file

let () =
  let map = ref StringMap.empty in
  Sys.readdir "./tests" |> Array.iter (register_file map) ;
  StringMap.iter emit_test !map

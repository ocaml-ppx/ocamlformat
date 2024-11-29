(** Generates Dune rules for running the tests. The [tests/] directory contains
    the source files for the tests. The output of the tests is promoted to the
    corresponding [refs.*/] directory for each profiles. *)

let input_dir = "../tests/"

module StringMap = Map.Make (String)

let spf = Printf.sprintf

let dep fname = spf "%%{dep:%s}" fname

type setup =
  { mutable has_opts: bool
  ; mutable base_file: string option
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
    {has_opts= false; base_file; should_fail= false; enabled_if= None}
  in
  map := StringMap.add src_test_name s !map ;
  s

let register_file tests fname =
  match String.split_on_char '.' fname with
  | test_name
    :: (("ml" | "mli" | "mlt" | "mld" | "eliom" | "eliomi") as ext)
    :: rest -> (
      let fname = input_dir ^ fname in
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
      | ["output"] -> ()
      | ["opts"] -> setup.has_opts <- true
      | ["should-fail"] -> setup.should_fail <- true
      | ["enabled-if"] -> setup.enabled_if <- Some (read_file fname)
      | ["err"] -> ()
      | _ -> invalid_arg fname )
  | _ -> ()

(* ignore dune file, .foo.whatever.swp, etc *)

let cmd should_fail args =
  let cmd_string = String.concat " " args in
  if should_fail then
    spf {|(with-accepted-exit-codes 1
       (run %s))|} cmd_string
  else spf {|(run %s)|} cmd_string

let emit_test test_name setup =
  let opts =
    (* Pass a relative file name to [--name] so that the right config is
       picked in [refs.*/.ocamlformat], this is not the input path. *)
    "--name" :: test_name :: "--margin-check"
    ::
    ( if setup.has_opts then read_lines (spf "%s/%s.opts" input_dir test_name)
      else [] )
  in
  let ref_file ext = test_name ^ ext in
  let base_test_name =
    input_dir ^ match setup.base_file with Some n -> n | None -> test_name
  in
  let enabled_if_line =
    match setup.enabled_if with
    | None -> ""
    | Some clause -> spf "\n (enabled_if %s)" clause
  in
  let output_fname = ref_file ".stdout" in
  Printf.printf
    {|
(rule
 (deps .ocamlformat)%s
 (package ocamlformat)
 (action
  (with-stdout-to %s
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
    enabled_if_line output_fname test_name
    (cmd setup.should_fail
       (["%{bin:ocamlformat}"] @ opts @ [dep base_test_name]) )
    enabled_if_line (ref_file ".ref") test_name enabled_if_line
    (ref_file ".err") test_name

let () =
  let map = ref StringMap.empty in
  Sys.readdir input_dir |> Array.iter (register_file map) ;
  StringMap.iter emit_test !map

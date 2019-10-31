module StringMap = Map.Make (String)

type entry = {
  has_ref : bool;
  has_opts : bool;
  should_fail : bool
}

let empty_entry = {
  has_ref = false;
  has_opts = false;
  should_fail = false;
}

let register_file entries fname =
  let update_or_add key ~f =
    StringMap.update key (function
        | Some entry -> Some (f entry)
        | None -> Some (f empty_entry)) entries
  in
  match String.split_on_char '.' fname with
  | [ test_name; "ref" ] ->
      update_or_add test_name ~f:(fun e -> { e with has_ref = true })
  | [ test_name; "opts" ] ->
      update_or_add test_name ~f:(fun e -> { e with has_opts = true })
  | [ test_name; "should-fail" ] ->
      update_or_add test_name ~f:(fun e -> { e with should_fail = true })
  | _ ->
      entries

let check_test test_name entry ok =
  let e ok b =
    if b then Format.kfprintf (fun _ -> false) Format.err_formatter
    else Format.ikfprintf (fun _ -> ok) Format.err_formatter
  in
  let ok = e ok (not entry.has_ref) "@{<error>Error@}: Missing file %s.ref\n" test_name in
  let ok = e ok (not entry.has_opts) "@{<error>Error@}: Missing file %s.opts\n" test_name in
  ok

let emit_test test_name entry =
  let cmd_prefix = if entry.should_fail then "! " else "" in
  Printf.printf {|
(rule
 (targets %s.output)
 (action
  (with-outputs-to %%{targets}
   (system "%s%%{bin:ocamlformat} %%{read-lines:%s.opts}"))))

(alias
 (name runtest)
 (action (diff %s.ref %s.output)))
|}
    test_name cmd_prefix test_name
    test_name test_name

let () =
  let files = Sys.readdir "." in
  let tests = Array.fold_left register_file StringMap.empty files in
  if not (StringMap.fold check_test tests true) then
    exit 1;
  StringMap.iter emit_test tests

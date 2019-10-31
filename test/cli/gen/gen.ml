module StringMap = Map.Make (String)

type entry = {
  should_fail : bool
}

let empty_entry = {
  should_fail = false;
}

let register_file entries fname =
  let update_or_add key ~f =
    StringMap.update key (function
        | Some entry -> Some (f entry)
        | None -> Some (f empty_entry)) entries
  in
  match String.split_on_char '.' fname with
  | [ test_name; "opts" ] ->
      update_or_add test_name ~f:(fun e -> e)
  | [ test_name; "should-fail" ] ->
      update_or_add test_name ~f:(fun _e -> { should_fail = true })
  | _ ->
      entries

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
  Sys.readdir "."
  |> Array.fold_left register_file StringMap.empty
  |> StringMap.iter emit_test

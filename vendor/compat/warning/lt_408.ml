open Base

let with_warning_filter ~filter ~f =
  let warning_printer = !Location.warning_printer in
  (Location.warning_printer :=
     fun loc fmt warn ->
       if filter loc warn then warning_printer loc fmt warn else ()) ;
  let reset () = Location.warning_printer := warning_printer in
  try
    let x = f () in
    reset () ; x
  with e -> reset () ; raise e

let print_to_string f =
  let buf = Buffer.create 0 in
  let ppf = Caml.Format.formatter_of_buffer buf in
  f ppf;
  Caml.Format.pp_print_flush ppf ();
  Buffer.contents buf

let expand_names_on_line s =
  let open Option in
  let parsed_line =
    String.index s ':' >>= fun colon_pos ->
    let before_colon = String.subo s ~len:(colon_pos) in
    String.chop_prefix before_colon ~prefix:"Warning " >>= fun n_str ->
    Caml.int_of_string_opt n_str >>= fun n ->
    Warning_name.warning_name n >>| fun name ->
    let rest = String.subo s ~pos:colon_pos in
    (n, name, rest)
  in
  match parsed_line with
  | None -> s
  | Some (n, name, rest) ->
      Printf.sprintf "Warning %d [%s]%s" n name rest

let expand_names s =
  String.split_lines s
  |> List.map ~f:expand_names_on_line
  |> List.map ~f:(fun s -> s ^ "\n")
  |> String.concat ~sep:""

let print_warning l w =
  print_to_string (fun ppf -> !Location.warning_printer l ppf w)
  |> expand_names
  |> Caml.Format.eprintf "%s"

let is_unexpected_docstring = function
  | Warnings.Bad_docstring _ -> true
  | _ -> false

let read_file file =
  let ic = Stdlib.open_in file in
  let buf = Buffer.create 32 in
  let rec aux buf =
    match Stdlib.input_line ic with
    | exception End_of_file ->
        Stdlib.close_in ic;
        let ret = Buffer.contents buf |> String.trim in
        Buffer.clear buf;
        ret
    | line ->
        Buffer.add_string buf line;
        aux buf
  in
  aux buf

module Pp = struct
  module Printast = Parser_extended.Printast

  let structure = Printast.implementation
  let signature = Printast.interface

  let use_file fs lx =
    Format.pp_print_list
      ~pp_sep:(fun fs () -> Format.fprintf fs "@\n")
      (fun fs x -> Format.fprintf fs "%a" Printast.top_phrase x)
      fs lx
end

let () =
  let test_name = Sys.argv.(2) in
  let contents = read_file test_name in
  let lx = Lexing.from_string contents in
  match Sys.argv.(1) with
  | "-structure" ->
      let x = Parser_recovery.structure lx in
      Format.fprintf Format.std_formatter "%a%!" Pp.structure x
  | "-signature" ->
      let x = Parser_recovery.signature lx in
      Format.fprintf Format.std_formatter "%a%!" Pp.signature x
  | "-use-file" ->
      let x = Parser_recovery.use_file lx in
      Format.fprintf Format.std_formatter "%a%!" Pp.use_file x
  | arg -> Format.fprintf Format.std_formatter "Invalid arg: %s%!" arg

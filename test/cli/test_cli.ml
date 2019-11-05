let pp_banner ppf s =
  let line = String.make (String.length s) '=' in
  Format.fprintf ppf "%s\n%s\n" s line

let pp_status ppf = function
  | Unix.WEXITED n -> Format.fprintf ppf "[%d]" n
  | WSIGNALED _ -> Format.fprintf ppf "WSIGNALED _"
  | WSTOPPED _ -> Format.fprintf ppf "WSTOPPED _"

let main ocamlformat =
  let test ?(stdin = "") ~name ~args () =
    let cmd = Printf.sprintf "%s %s 2>&1" ocamlformat args in
    let proc = Unix.open_process cmd in
    let proc_out, proc_in = proc in
    Stdio.Out_channel.output_string proc_in stdin ;
    Stdio.Out_channel.close proc_in ;
    let out = Stdio.In_channel.input_all proc_out in
    let stat = Unix.close_process proc in
    Format.printf "%aargs: %s\n%s%a\n\n" pp_banner name args out pp_status stat
  in
  let impl_file = Stdio.In_channel.read_all "sample/b.ml" in
  let intf_file = Stdio.In_channel.read_all "sample/a.mli" in
  let syntax_error_file = Stdio.In_channel.read_all "sample/syntax_error.ml" in
  test ~name:"err_default_several_file" ~args:"sample/a.ml sample/b.ml" () ;
  test ~name:"err_inplace_and_check" ~args:"--inplace --check sample/a.ml" () ;
  test ~name:"err_inplace_and_output"
    ~args:"--inplace --output o.ml sample/a.ml" () ;
  test ~name:"err_no_arg" ~args:"" () ;
  test ~name:"err_output_and_check" ~args:"--output x.ml --check sample/a.ml" () ;
  test ~name:"err_output_several_files"
    ~args:"--output x.ml sample/a.ml sample/b.ml" () ;
  test ~name:"err_stdin_and_file" ~args:"sample/a.ml -" () ;
  test ~name:"err_stdin_and_inplace" ~args:"--inplace -" () ;
  test ~name:"err_stdin_no_kind" ~args:"-" () ;
  test ~name:"stdin_and_impl" ~stdin:impl_file ~args:"--impl -" () ;
  test ~name:"stdin_and_intf" ~stdin:intf_file ~args:"--intf -" () ;
  test ~name:"stdin_and_name" ~stdin:impl_file ~args:"--name a.ml -" () ;
  test ~name:"name_unknown_ext" ~args:"--name b.cpp sample/b.ml" () ;
  test ~name:"err_stdin_name_unknown_ext" ~stdin:impl_file
    ~args:"--name b.cpp -" () ;
  test ~name:"err_several_files_and_kind" ~args:"--impl --check sample/a.mli sample/b.ml" () ;
  test ~name:"err_several_files_and_name" ~args:"--name foo.ml --check sample/a.mli sample/b.ml" () ;
  test ~name:"err_several_files_and_kind_inplace" ~args:"--impl --check sample/a.mli sample/b.ml" () ;
  test ~name:"err_several_files_and_name_inplace" ~args:"--name foo.ml --check sample/a.mli sample/b.ml" () ;
  test ~name:"fmterr_file_and_name" ~args:"--name foo.ml sample/syntax_error.ml" () ;
  test ~name:"fmterr_stdin_and_name" ~stdin:syntax_error_file ~args:"--name foo.ml -" () ;
  test ~name:"fmterr_file_bad_kind" ~args:"--impl sample/a.mli" () ;
  test ~name:"fmterr_stdin_bad_kind" ~stdin:intf_file ~args:"--impl -" () ;
  test ~name:"fmterr_file_and_name_bad_kind" ~args:"--name foo.ml sample/a.mli" () ;
  test ~name:"fmterr_stdin_and_name_bad_kind" ~stdin:intf_file ~args:"--name foo.ml -" () ;
  ()

let () =
  match Sys.argv with
  | [|_; ocamlformat|] -> main ocamlformat
  | _ -> assert false

open! Base

type t = Stdlib.out_channel

let equal (t1 : t) t2 = phys_equal t1 t2
let seek = Stdlib.LargeFile.seek_out
let pos = Stdlib.LargeFile.pos_out
let length = Stdlib.LargeFile.out_channel_length
let stdout = Stdlib.stdout
let stderr = Stdlib.stderr

let sexp_of_t t =
  if phys_equal t stderr
  then Sexp.Atom "<stderr>"
  else if phys_equal t stdout
  then Sexp.Atom "<stdout>"
  else Sexp.Atom "<Out_channel.t>"
;;

type 'a with_create_args =
  ?binary:bool -> ?append:bool -> ?fail_if_exists:bool -> ?perm:int -> 'a

let create
      ?(binary = true)
      ?(append = false)
      ?(fail_if_exists = false)
      ?(perm = 0o666)
      file
  =
  let flags = [ Open_wronly; Open_creat ] in
  let flags = (if binary then Open_binary else Open_text) :: flags in
  let flags = (if append then Open_append else Open_trunc) :: flags in
  let flags = if fail_if_exists then Open_excl :: flags else flags in
  Stdlib.open_out_gen flags perm file
;;

let set_binary_mode = Stdlib.set_binary_mode_out
let flush = Stdlib.flush
let close = Stdlib.close_out
let close_no_err = Stdlib.close_out_noerr
let output t ~buf ~pos ~len = Stdlib.output t buf pos len
let output_substring t ~buf ~pos ~len = Stdlib.output_substring t buf pos len
let output_string = Stdlib.output_string
let output_bytes = Stdlib.output_bytes
let output_char = Stdlib.output_char
let output_byte = Stdlib.output_byte
let output_binary_int = Stdlib.output_binary_int
let output_buffer = Stdlib.Buffer.output_buffer
let output_value = Stdlib.output_value
let newline t = output_string t "\n"

let output_lines t lines =
  List.iter lines ~f:(fun line ->
    output_string t line;
    newline t)
;;

let printf = Stdlib.Printf.printf
let eprintf = Stdlib.Printf.eprintf
let fprintf = Stdlib.Printf.fprintf
let kfprintf = Stdlib.Printf.kfprintf
let print_string = Stdlib.print_string
let print_endline = Stdlib.print_endline
let prerr_endline = Stdlib.prerr_endline

let print_s ?mach sexp =
  print_endline
    (match mach with
     | Some () -> Sexp.to_string_mach sexp
     | None -> Sexp.to_string_hum sexp)
;;

let eprint_s ?mach sexp =
  prerr_endline
    (match mach with
     | Some () -> Sexp.to_string_mach sexp
     | None -> Sexp.to_string_hum sexp)
;;

let with_file ?binary ?append ?fail_if_exists ?perm file ~f =
  Exn.protectx (create ?binary ?append ?fail_if_exists ?perm file) ~f ~finally:close
;;

let write_lines file lines = with_file file ~f:(fun t -> output_lines t lines)
let write_all file ~data = with_file file ~f:(fun t -> output_string t data)

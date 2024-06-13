let handle_arg arg =
  print_endline arg ;
  let b = Buffer.create 128 in
  let in_chan = open_in arg in
  let lexbuf = Lexing.from_channel in_chan in
  Lexer.disabled (Buffer.add_string b) lexbuf ;
  close_in in_chan ;
  let out_chan = open_out arg in
  Buffer.output_buffer out_chan b ;
  close_out out_chan

let () =
  let argv = Sys.argv in
  for i = 1 to Array.length argv - 1 do
    handle_arg argv.(i)
  done

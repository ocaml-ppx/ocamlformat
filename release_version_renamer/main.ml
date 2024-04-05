let handle_arg arg =
  let b = Buffer.create 128 in
  In_channel.with_open_text arg (fun input ->
      let lexbuf = Lexing.from_channel input in
      Lexer.disabled (Buffer.add_string b) lexbuf ) ;
  Out_channel.with_open_text arg (fun out -> Buffer.output_buffer out b)

let () =
  let argv = Sys.argv in
  for i = 1 to Array.length argv - 1 do
    handle_arg argv.(i)
  done

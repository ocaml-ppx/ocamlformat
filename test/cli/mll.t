  $ echo profile=default > .ocamlformat

Format a .mll file from stdin using --mll (default: ocaml-block mode):

  $ cat << 'EOF' | ocamlformat --mll -
  > {
  >   let   x   =   1
  > }
  > let blank = [' '   '\t']
  > rule main = parse
  >   | blank+
  >     { () }
  >   | eof
  >     { () }
  > EOF
  {
  let x = 1
  }
  let blank = [' '   '\t']
  rule main = parse
    | blank+
      { () }
    | eof
      { () }

Format a .mll file inferred from --name:

  $ cat << 'EOF' | ocamlformat --name test.mll -
  > {
  >   open   Lexing
  > }
  > let  newline =  '\n' | "\r\n"
  > rule   token   =   parse
  >   | newline
  >     { new_line    lexbuf;   token    lexbuf }
  >   | eof
  >     { EOF }
  > EOF
  {
  open Lexing
  }
  let  newline =  '\n' | "\r\n"
  rule   token   =   parse
    | newline
      {
        new_line lexbuf;
        token lexbuf
      }
    | eof
      { EOF }

Default mode (ocaml-block) preserves comments:

  $ cat << 'EOF' | ocamlformat --mll -
  > (* a header comment *)
  > {
  >   let   x   =   1
  > }
  > (* a definition comment *)
  > let blank = [' '   '\t']
  > (* a rule comment *)
  > rule main = parse
  >   (* a case comment *)
  >   | blank+
  >     { () }
  >   | eof
  >     { () }
  > EOF
  (* a header comment *)
  {
  let x = 1
  }
  (* a definition comment *)
  let blank = [' '   '\t']
  (* a rule comment *)
  rule main = parse
    (* a case comment *)
    | blank+
      { () }
    | eof
      { () }

Full reformat mode (--reformat-mll=full):

  $ cat << 'EOF' | ocamlformat --mll --reformat-mll=full -
  > {
  >   let   x   =   1
  > }
  > let blank = [' '   '\t']
  > rule main = parse
  >   | blank+
  >     { () }
  >   | eof
  >     { () }
  > EOF
  {
  let x = 1
  }
  
  let blank = [' ' '\t']
  
  rule main = parse
         | blank+ { () }
         | eof { () }

Full reformat mode preserves comments:

  $ cat << 'EOF' | ocamlformat --mll --reformat-mll=full -
  > (* a header comment *)
  > {
  >   let   x   =   1
  > }
  > (* a definition comment *)
  > let blank = [' '   '\t']
  > (* a rule comment *)
  > rule main = parse
  >   (* a case comment *)
  >   | blank+
  >     { () }
  >   | eof
  >     { () }
  > EOF
  (* a header comment *)
  {
  let x = 1
  }
  
  (* a definition comment *)
  let blank = [' ' '\t']
  
  (* a rule comment *)
  rule main = parse
         (* a case comment *)
         | blank+ { () }
         | eof { () }

No-format mode (--reformat-mll=no) returns input unchanged:

  $ cat << 'EOF' | ocamlformat --mll --reformat-mll=no -
  > {
  >   let   x   =   1
  > }
  > let blank = [' '   '\t']
  > rule main = parse
  >   | blank+
  >     { () }
  >   | eof
  >     { () }
  > EOF
  {
    let   x   =   1
  }
  let blank = [' '   '\t']
  rule main = parse
    | blank+
      { () }
    | eof
      { () }

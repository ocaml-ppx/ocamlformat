open Ocamlformat_lib

let rec all_tokens acc lexbuf =
  let tok = Lexer.token_with_comments lexbuf in
  let acc = lexbuf.lex_start_p.pos_cnum :: acc in
  if tok = Migrate_ast.Parser.EOF then acc else all_tokens acc lexbuf

(** Return the offset at which every tokens start. *)
let token_offsets source =
  let lexbuf = Lexing.from_string source in
  Lexer.init () ;
  Lexer.skip_hash_bang lexbuf ;
  List.rev (all_tokens [] lexbuf)

let insert_at_every_offsets insert toffs source ~f =
  let ilen = String.length insert and slen = String.length source in
  let buff = Bytes.create (ilen + slen) in
  Bytes.blit_string source 0 buff ilen slen ;
  let rec loop head_i = function
    | hd :: tl ->
        (* Move previous token from after [insert] to before (overriding it). *)
        Bytes.blit buff (head_i + ilen) buff head_i (hd - head_i) ;
        (* Blit again [insert]. *)
        Bytes.blit_string insert 0 buff hd (String.length insert) ;
        f (Bytes.unsafe_to_string buff) ;
        loop hd tl
    | [] -> ()
  in
  loop 0 toffs

let run file =
  let conf = Conf.conventional_profile
  and opts =
    Conf.{debug= false; margin_check= false; format_invalid_files= false}
  in
  let source = Stdio.In_channel.read_all file in
  let toffs = token_offsets source in
  insert_at_every_offsets "(* toto *)" toffs source ~f:(fun source ->
      match
        Translation_unit.parse_and_format Migrate_ast.Traverse.Structure
          ~input_name:file ~source conf opts
      with
      | Ok formatted -> Printf.printf "%s\n" formatted
      | Error err ->
          Translation_unit.print_error ~debug:false ~quiet:true
            ~input_name:file err )

open Cmdliner

let cmd =
  let a_file = Arg.(required & pos 0 (some file) None & info []) in
  let doc =
    "Repeatedly test ocamlformat with a comment before every tokens."
  in
  Term.(const run $ a_file, info ~doc "test_comments")

let () = Term.exit (Term.eval cmd)

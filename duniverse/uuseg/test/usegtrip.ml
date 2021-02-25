(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let str = Printf.sprintf
let pp = Format.fprintf
let pp_pos ppf d = pp ppf "%d.%d:(%d,%06X) "
    (Uutf.decoder_line d) (Uutf.decoder_col d) (Uutf.decoder_count d)
    (Uutf.decoder_byte_count d)

let pp_malformed ppf bs =
  let l = String.length bs in
  pp ppf "@[malformed bytes @[(";
  if l > 0 then pp ppf "%02X" (Char.code (bs.[0]));
  for i = 1 to l - 1 do pp ppf "@ %02X" (Char.code (bs.[i])) done;
  pp ppf ")@]@]"

let exec = Filename.basename Sys.executable_name
let log f = Format.eprintf ("%s: " ^^ f ^^ "@?") exec

let input_malformed = ref false
let log_malformed inf d bs =
  input_malformed := true;
  log "%s:%a: %a@." inf pp_pos d pp_malformed bs

let u_rep = `Uchar Uutf.u_rep

(* Output *)

let uchar_ascii delim ppf =
  let last_was_u = ref false in
  function
  | `Uchar u ->
      if !last_was_u then (Format.pp_print_char ppf ' ');
      last_was_u := true; pp ppf "U+%04X" (Uchar.to_int u)
  | `Boundary ->
      last_was_u := false; pp ppf "%s" delim
  | `End -> ()

let uchar_encoder enc delim =
  let enc = match enc with
  | `ISO_8859_1 | `US_ASCII -> `UTF_8
  | #Uutf.encoding as enc -> enc
  in
  let delim =
    let add acc _ = function
    | `Uchar _ as u -> u :: acc
    | `Malformed bs ->
        log "delimiter: %a" pp_malformed bs; u_rep :: acc
    in
    List.rev (Uutf.String.fold_utf_8 add [] delim)
  in
  let e = Uutf.encoder enc (`Channel stdout) in
  function
  | `Uchar _ | `End as v -> ignore (Uutf.encode e v)
  | `Boundary -> List.iter (fun u -> ignore (Uutf.encode e u)) delim

let out_fun delim ascii oe =
  if ascii then uchar_ascii delim Format.std_formatter else
  uchar_encoder oe delim

(* Trip *)

let segment boundary inf d first_dec out =
  let segmenter = Uuseg.create boundary in
  let rec add v = match Uuseg.add segmenter v with
  | `Uchar _ | `Boundary as v -> out v; add `Await
  | `Await | `End -> ()
  in
  let rec loop d = function
  | `Uchar _ as v -> add v; loop d (Uutf.decode d)
  | `End as v -> add v; out `End
  | `Malformed bs -> log_malformed inf d bs; add u_rep; loop d (Uutf.decode d)
  | `Await -> assert false
  in
  if Uutf.decoder_removed_bom d then add (`Uchar Uutf.u_bom);
  loop d first_dec

let trip seg inf enc delim ascii =
  try
    let ic = if inf = "-" then stdin else open_in inf in
    let d = Uutf.decoder ?encoding:enc (`Channel ic) in
    let first_dec = Uutf.decode d in            (* guess encoding if needed. *)
    let out = out_fun delim ascii (Uutf.decoder_encoding d) in
    segment seg inf d first_dec out;
    if inf <> "-" then close_in ic;
    flush stdout
  with Sys_error e -> log "%s@." e; exit 1

(* Version *)

let unicode_version () = Format.printf "%s@." Uuseg.unicode_version

(* Cmd *)

let do_cmd cmd seg inf enc delim ascii = match cmd with
| `Unicode_version -> unicode_version ()
| `Trip -> trip seg inf enc delim ascii

(* Cmdline interface *)

open Cmdliner

let cmd =
  let doc = "Output supported Unicode version." in
  let unicode_version = `Unicode_version, Arg.info ["unicode-version"] ~doc in
  Arg.(value & vflag `Trip [unicode_version])

let seg_docs = "SEGMENTATION"
let seg =
  let docs = seg_docs in
  let doc = "Line break opportunities boundaries." in
  let line = `Line_break, Arg.info ["l"; "line"] ~doc ~docs in
  let doc = "Grapheme cluster boundaries." in
  let gc = `Grapheme_cluster, Arg.info ["g"; "grapheme-cluster"] ~doc ~docs in
  let doc = "Word boundaries (default)." in
  let w = `Word, Arg.info ["w"; "word"] ~doc ~docs in
  let doc = "Sentence boundaries." in
  let s = `Sentence, Arg.info ["s"; "sentence"] ~doc ~docs in
  Arg.(value & vflag `Word [line; gc; w; s])

let file =
  let doc = "The input file. Reads from stdin if unspecified." in
  Arg.(value & pos 0 string "-" & info [] ~doc ~docv:"FILE")

let enc =
  let enc = [ "UTF-8", `UTF_8; "UTF-16", `UTF_16; "UTF-16LE", `UTF_16LE;
              "UTF-16BE", `UTF_16BE; "ASCII", `US_ASCII; "latin1", `ISO_8859_1 ]
  in
  let doc = str "Input encoding, must %s. If unspecified the encoding is \
                 guessed. The output encoding is the same as the input \
                 encoding except for ASCII and latin1 where UTF-8 is output."
                (Arg.doc_alts_enum enc)
  in
  Arg.(value & opt (some (enum enc)) None & info [ "e"; "encoding" ] ~doc)

let ascii =
  let doc = "Output the input text as space (U+0020) separated Unicode
             scalar values written in the US-ASCII charset."
  in
  Arg.(value & flag & info ["a"; "ascii"] ~doc)

let delim =
  let doc = "The UTF-8 encoded delimiter used to denote boundaries." in
  Arg.(value & opt string "|" & Arg.info [ "d"; "delimiter" ] ~doc ~docv:"SEP")

let cmd =
  let doc = "segment Unicode text" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) inputs Unicode text from stdin and rewrites it
        to stdout with segment boundaries as determined according
        the locale independent specifications of UAX 29 and UAX 14.
        Boundaries are represented by the UTF-8 encoded delimiter string
        specified with the option $(b,-d) (defaults to `|').";
    `P "Invalid byte sequences in the input are reported on stderr and
        replaced by the Unicode replacement character (U+FFFD) in the output.";
    `S seg_docs;
    `S "OPTIONS";
    `S "EXIT STATUS";
    `P "$(tname) exits with one of the following values:";
    `I ("0", "no error occured");
    `I ("1", "a command line parsing error occured");
    `I ("2", "the input text was malformed");
    `S "BUGS";
    `P "This program is distributed with the Uuseg OCaml library.
        See http://erratique.ch/software/uuseg for contact
        information."; ]
  in
  Term.(pure do_cmd $ cmd $ seg $ file $ enc $ delim $ ascii),
  Term.info "usegtrip" ~version:"%%VERSION%%" ~doc ~man

let () = match Term.eval cmd with
| `Error _ -> exit 1
| _ -> if !input_malformed then exit 2 else exit 0

(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

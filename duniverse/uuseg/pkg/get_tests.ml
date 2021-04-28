#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let tests_file kind = strf "test/%sBreakTest.txt" kind
let tests_uri kind = (* FIXME this should source from %%UNICODE_VERSION%% *)
  strf "http://www.unicode.org/Public/UCD/latest/ucd/auxiliary/%sBreakTest.txt" kind

let get_tests kind =
  let tests_file = tests_file kind in
  let tests_uri = tests_uri kind in
  OS.File.exists tests_file >>= function
  | true -> Topkg.Ok ()
  | false ->
      let curl = Cmd.(v "curl" % "-f" % "-#" % "-S" % tests_uri) in
      Log.app (fun m -> m "Downloading %s" tests_uri);
      OS.Cmd.(run_out curl |> to_file tests_file)

let main () =
  begin
    Topkg.Private.disable_main (); (* We only want the nicer OS API. *)
    get_tests "Line"
    >>= fun () -> get_tests "Grapheme"
    >>= fun () -> get_tests "Word"
    >>= fun () -> get_tests "Sentence"
    >>= fun () -> Ok 0
  end
  |> Log.on_error_msg ~use:(fun () -> 1)

let () = exit (main ())

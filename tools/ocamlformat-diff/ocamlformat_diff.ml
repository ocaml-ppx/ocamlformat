(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

open Rresult

(** [run_ocamlformat cmd ~ctx ~file_in] runs the ocamlformat command [cmd]
    on the input file [file_in] with the configuration context of file [ctx]
    and returns the path to the output in file. *)
let run_ocamlformat cmd ~ctx ~file_in =
  Bos.OS.File.tmp "%s"
  >>= fun path_out ->
  Bos.Cmd.(
    cmd % Fpath.to_string file_in % "--name" % Fpath.to_string ctx
    % "--disable-outside-detected-project" % "-o" % Fpath.to_string path_out)
  |> Bos.OS.Cmd.run_status
  |> function
     | Ok (`Exited 0) -> Ok path_out
     | Ok _ ->
         R.error_msg
           ("ocamlformat on " ^ Fpath.to_string file_in ^ " failed!")
     | Error e -> Error e

(** [diff_in_ctx ~ctx ~old_file ~new_file] runs the diff of files [old_file]
    and [new_file] in the context of file [ctx], so that they both inherit
    the same configuration. *)
let diff_in_ctx ~ctx ~old_file ~new_file ~diff =
  Bos.OS.Cmd.resolve (Bos.Cmd.v "ocamlformat")
  >>= fun fmt ->
  run_ocamlformat fmt ~ctx ~file_in:old_file
  >>= fun tmp_1 ->
  run_ocamlformat fmt ~ctx ~file_in:new_file
  >>= fun tmp_2 ->
  Bos.OS.Cmd.must_exist (Bos.Cmd.v diff)
  >>= fun diff ->
  Bos.Cmd.(diff % Fpath.to_string tmp_1 % Fpath.to_string tmp_2)
  |> Bos.OS.Cmd.run_out |> Bos.OS.Cmd.out_string

let short_hash x = String.sub x 0 6

let diff_prelude path _ old_hex old_mode _ new_hex _ =
  let new_hex =
    match
      Bos.OS.Cmd.resolve (Bos.Cmd.v "git")
      >>= fun git ->
      Bos.Cmd.(git % "hash-object" % path)
      |> Bos.OS.Cmd.run_out |> Bos.OS.Cmd.out_string
    with
    | Ok (x, _) -> x
    | Error _ -> new_hex
    (* should never fail *)
  in
  print_endline (Format.sprintf "diff --git a/%s b/%s" path path) ;
  print_endline
    (Format.sprintf "index %s..%s %s" (short_hash old_hex)
       (short_hash new_hex) old_mode) ;
  print_endline (Format.sprintf "--- a/%s" path) ;
  print_endline (Format.sprintf "+++ b/%s" path)

let run path old_file old_hex old_mode new_file new_hex new_mode diff =
  let diff =
    Bos.OS.File.must_exist (Fpath.v old_file)
    >>= fun old_file ->
    Bos.OS.File.must_exist (Fpath.v new_file)
    >>= fun new_file ->
    let tmp = Bos.OS.Dir.default_tmp () in
    let ctx = if Fpath.is_prefix tmp new_file then old_file else new_file in
    diff_in_ctx ~ctx ~old_file ~new_file ~diff
  in
  match diff with
  | Ok (str, _) ->
      if String.equal str "" then `Ok ()
      else (
        diff_prelude path old_file old_hex old_mode new_file new_hex
          new_mode ;
        print_endline str ;
        `Ok () )
  | _ -> `Ok ()

open Cmdliner

let diff =
  let doc =
    "Diff command used on formatted files. Default value is `diff`."
  in
  Arg.(value & opt string "diff" & info ["diff"] ~doc)

let path =
  Arg.(
    required
    & pos ~rev:true 6 (some string) None
    & info [] ~docv:"path" ~doc:"")

let old_file =
  Arg.(
    required
    & pos ~rev:true 5 (some string) None
    & info [] ~docv:"old_file" ~doc:"")

let old_hex =
  Arg.(
    required
    & pos ~rev:true 4 (some string) None
    & info [] ~docv:"old_hex" ~doc:"")

let old_mode =
  Arg.(
    required
    & pos ~rev:true 3 (some string) None
    & info [] ~docv:"old_mode" ~doc:"")

let new_file =
  Arg.(
    required
    & pos ~rev:true 2 (some string) None
    & info [] ~docv:"new_file" ~doc:"")

let new_hex =
  Arg.(
    required
    & pos ~rev:true 1 (some string) None
    & info [] ~docv:"new_hex" ~doc:"")

let new_mode =
  Arg.(
    required
    & pos ~rev:true 0 (some string) None
    & info [] ~docv:"new_mode" ~doc:"")

let cmd =
  let doc = "OCamlFormat diff" in
  let exits = Term.default_exits in
  ( Term.(
      ret
        ( const run $ path $ old_file $ old_hex $ old_mode $ new_file
        $ new_hex $ new_mode $ diff ))
  , Term.info "ocamlformat-diff" ~doc ~exits )

let () = Term.(exit @@ eval cmd)

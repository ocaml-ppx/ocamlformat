(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2019-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

let to_output_file output_file data =
  match output_file with
  | None -> Out_channel.output_string Out_channel.stdout data
  | Some output_file -> Out_channel.write_all output_file ~data

let print_error conf (error : Translation_unit.error) =
  ( match error with
  | Unstable {input_name; prev; next} ->
      if Conf.debug then (
        let ext = Filename.extension input_name in
        let input_name =
          Filename.chop_extension (Filename.basename input_name)
        in
        let p =
          Filename.temp_file input_name (Printf.sprintf ".prev%s" ext)
        in
        Out_channel.write_all p ~data:prev ;
        let n =
          Filename.temp_file input_name (Printf.sprintf ".next%s" ext)
        in
        Out_channel.write_all n ~data:next ;
        ignore (Unix.system (Printf.sprintf "diff %S %S 1>&2" p n)) ;
        Unix.unlink p ;
        Unix.unlink n )
  | _ -> () ) ;
  Translation_unit.print_error conf Caml.Format.err_formatter error

let with_file input_name output_file suf ext f =
  let dir =
    match output_file with
    | Some filename -> Filename.dirname filename
    | None -> Filename.get_temp_dir_name ()
  in
  let base = Filename.remove_extension (Filename.basename input_name) in
  let tmp = Filename.concat dir (base ^ suf ^ ext) in
  Out_channel.with_file tmp ~f ;
  tmp

let dump_ast ~input_name ?output_file ~suffix fmt =
  if Conf.debug then
    let ext = ".ast" in
    let (_filename : string) =
      with_file input_name output_file suffix ext (fun oc ->
          fmt (Caml.Format.formatter_of_out_channel oc) )
    in
    ()

let dump_formatted ~input_name ?output_file ~suffix fmted =
  let ext = Filename.extension input_name in
  if Conf.debug then
    let file =
      with_file input_name output_file suffix ext (fun oc ->
          Out_channel.output_string oc fmted )
    in
    Some file
  else None

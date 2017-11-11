(**********************************************************************)
(*                                                                    *)
(*                            OCamlFormat                             *)
(*                                                                    *)
(*  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  *)
(*                                                                    *)
(*  This source code is licensed under the MIT license found in the   *)
(*  LICENSE file in the root directory of this source tree.           *)
(*                                                                    *)
(**********************************************************************)

(** Configuration options *)

(** Extension of Cmdliner supporting lighter-weight option definition *)
module Cmdliner : sig
  include module type of Cmdliner

  val mk : default:'a -> 'a Term.t -> 'a ref
  (** [mk ~default term] is a ref which, after [parse] is called, contains
      the value of the command line option specified by [term]. *)

  val parse : Term.info -> (unit -> unit Term.ret) -> unit
  (** [parse info validate] parses the command line according to the options
      declared by calls to [mk], using manual and version [info], and calling
      [validate] to check usage constraints not expressible in the [Term]
      language. *)
end = struct
  include Cmdliner

  (** existential package of a Term and a setter for a ref to receive the
      parsed value *)
  type arg = Arg: 'a Term.t * ('a -> unit) -> arg

  (** convert a list of arg packages to a term for the tuple of all the arg
      terms, and apply it to a function that sets all the receiver refs *)
  let tuple args =
    let pair (Arg (trm_x, set_x)) (Arg (trm_y, set_y)) =
      let trm_xy = Term.(const (fun a b -> (a, b)) $ trm_x $ trm_y) in
      let set_xy (a, b) = set_x a ; set_y b in
      Arg (trm_xy, set_xy)
    in
    let init = Arg (Term.const (), fun () -> ()) in
    let Arg (trm, set) = List.fold_right ~f:pair args ~init in
    Term.app (Term.const set) trm


  let args : arg list ref = ref []

  let mk ~default arg =
    let var = ref default in
    let set x = var := x in
    args := Arg (arg, set) :: !args ;
    var


  let parse info validate =
    match Term.eval (Term.(ret (const validate $ tuple !args)), info) with
    | `Ok () -> ()
    | `Error _ -> Caml.exit 1
    | `Help | `Version -> Caml.exit 0
end

open Cmdliner

let info =
  let doc = "A tool to format OCaml code." in
  let man =
    [`S "DESCRIPTION"; `P "$(tname) automatically formats OCaml code."]
  in
  Term.info "ocamlformat" ~version:Version.version ~doc ~man


let debug =
  let doc = "Generate debugging output." in
  let env = Arg.env_var "OCAMLFORMAT_DEBUG" in
  let default = false in
  mk ~default Arg.(value & flag & info ["g"; "debug"] ~doc ~env)


let kind : [`Impl | `Intf] ref =
  let doc =
    "Parse file with unrecognized extension as an implementation."
  in
  let impl = (`Impl, Arg.info ["impl"] ~doc) in
  let doc = "Parse file with unrecognized extension as an interface." in
  let intf = (`Intf, Arg.info ["intf"] ~doc) in
  let default = `Impl in
  mk ~default Arg.(value & vflag default [impl; intf])


let margin =
  let docv = "COLS" in
  let doc =
    "Format code to fit within $(docv) columns. Can be set in a config file with a `margin COLS` line."
  in
  let env = Arg.env_var "OCAMLFORMAT_MARGIN" in
  let default = 80 in
  mk ~default
    Arg.(value & opt int default & info ["m"; "margin"] ~doc ~docv ~env)


let max_iters =
  let docv = "N" in
  let doc =
    "Fail if output of formatting does not stabilize within $(docv) iterations. Can be set in a config file with a `max-iters N` line."
  in
  let env = Arg.env_var "OCAMLFORMAT_MAX_ITERS" in
  let default = 10 in
  mk ~default
    Arg.(value & opt int default & info ["n"; "max-iters"] ~doc ~docv ~env)


let inplace =
  let doc = "Format in-place, overwriting input file(s)." in
  let default = false in
  mk ~default Arg.(value & flag & info ["i"; "inplace"] ~doc)


let inputs =
  let docv = "SRC" in
  let doc =
    "Input files. At least one is required, and exactly one without --inplace."
  in
  let default = [] in
  mk ~default Arg.(value & pos_all file default & info [] ~doc ~docv)


let name =
  let docv = "NAME" in
  let doc =
    "Name of input file for use in error reporting. Defaults to the input file name. Some options can be specified in configuration files named '.ocamlformat' in the same or a parent directory of $(docv), see documentation of other options for details. Mutually exclusive with --inplace."
  in
  let default = None in
  mk ~default
    Arg.(value & opt (some string) default & info ["name"] ~doc ~docv)


let output =
  let docv = "DST" in
  let doc =
    "Output file. Mutually exclusive with --inplace. Write to stdout if omitted."
  in
  let default = None in
  mk ~default
    Arg.(
      value & opt (some string) default & info ["o"; "output"] ~doc ~docv)


let sparse =
  let doc =
    "Generate more sparsely formatted code. Can be set in a config file with a `sparse true` line."
  in
  let env = Arg.env_var "OCAMLFORMAT_SPARSE" in
  let default = false in
  mk ~default Arg.(value & flag & info ["sparse"] ~doc ~env)


let no_version_check =
  let doc =
    "Do no check version matches the one specified in .ocamlformat."
  in
  let default = false in
  mk ~default Arg.(value & flag & info ["no-version-check"] ~doc)


let no_warn_error =
  let doc =
    "Do no treat warnings detected by the parser as errors. These warnings almost always indicate an unrecoverable situation, so expect an unhandled exception."
  in
  let default = false in
  mk ~default Arg.(value & flag & info ["no-warn-error"] ~doc)


let validate () =
  if List.is_empty !inputs then
    `Error (false, "Must specify at least one input file.")
  else if !inplace && Option.is_some !name then
    `Error (false, "Cannot specify --name with --inplace")
  else if !inplace && Option.is_some !output then
    `Error (false, "Cannot specify --output with --inplace")
  else if not !inplace && List.length !inputs > 1 then
    `Error (false, "Must specify only one input file without --inplace")
  else `Ok ()


;; parse info validate

type t = {margin: int; sparse: bool; max_iters: int}

let update conf name value =
  match name with
  | "margin" -> {conf with margin= Int.of_string value}
  | "max-iters" -> {conf with max_iters= Int.of_string value}
  | "sparse" -> {conf with sparse= Bool.of_string value}
  | "version" when not !no_version_check ->
      if String.equal Version.version value then conf
      else
        user_error
          ( "version mismatch: .ocamlformat requested " ^ value
          ^ " but version is " ^ Version.version )
          []
  | _ -> conf


let rec read_conf_files conf dir =
  let dir' = Filename.dirname dir in
  if not (String.equal dir dir') && Caml.Sys.file_exists dir then
    let conf = read_conf_files conf dir' in
    try
      In_channel.with_file (Filename.concat dir ".ocamlformat") ~f:
        (fun ic ->
          In_channel.fold_lines ic ~init:conf ~f:(fun conf line ->
              try Scanf.sscanf line "%s %s" (update conf) with
              | Scanf.Scan_failure _ | End_of_file ->
                  user_error "malformed .ocamlformat file"
                    [("line", Sexp.Atom line)] ) )
    with Sys_error _ -> conf
  else conf


let to_absolute file =
  Filename.(if is_relative file then concat (Unix.getcwd ()) file else file)


let conf name =
  read_conf_files {margin= !margin; sparse= !sparse; max_iters= !max_iters}
    (Filename.dirname (to_absolute name))


type 'a input = {kind: 'a; name: string; file: string; conf: t}

type action =
  | In_out of [`Impl | `Intf] input * string option
  | Inplace of [`Impl | `Intf] input list

let kind_of fname =
  match Filename.extension fname with
  | ".ml" -> `Impl
  | ".mli" -> `Intf
  | _ -> !kind


let action =
  if !inplace then
    Inplace
      (List.map !inputs ~f:(fun file ->
           {kind= kind_of file; name= file; file; conf= conf file} ))
  else
    match !inputs with
    | [input_file] ->
        let name = Option.value !name ~default:input_file in
        In_out
          ( { kind= kind_of input_file
            ; name
            ; file= input_file
            ; conf= conf name }
          , !output )
    | _ -> impossible "checked by validate"

and debug = !debug

and warn_error = not !no_warn_error

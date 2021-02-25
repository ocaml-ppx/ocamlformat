open Dune_engine
open Import

type t =
  | Lib of Lib_name.t
  | Exes of string list

let for_lib l = Lib l

let for_exes ~names = Exes names

(* For debug purposes we use the name of one library or executable and the hash
   of the others if there are multiple executables to name the merlin file *)
let to_string = function
  | Lib name -> sprintf "lib-%s" (Lib_name.to_string name)
  | Exes [ name ] -> sprintf "exe-%s" name
  | Exes (name :: names) ->
    sprintf "exe-%s-%s" name Digest.(generic names |> to_string)
  | Exes [] -> assert false

let merlin_exist_name = ".merlin-exist"

let merlin_folder_name = ".merlin-conf"

let merlin_exists_path path ident =
  String.concat ~sep:"-" [ merlin_exist_name; to_string ident ]
  |> Path.Build.relative path

let merlin_file_path path ident =
  Filename.concat merlin_folder_name (to_string ident)
  |> Path.Build.relative path

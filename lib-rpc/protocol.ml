(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

type format_args =
  {path: string option; config: (string * string) list option}

let empty_args = {path= None; config= None}

module Version = struct
  type t = V1 | V2

  let to_string = function V1 -> "v1" | V2 -> "v2"

  let of_string = function
    | "v1" | "V1" -> Some V1
    | "v2" | "V2" -> Some V2
    | _ -> None
end

module Make (IO : IO.S) = struct
  module type Command_S = sig
    type t

    val read_input : IO.ic -> t IO.t

    val output : IO.oc -> t -> unit IO.t
  end

  module Init = struct
    type t = [`Halt | `Unknown | `Version of string]

    let read_input ic =
      let open IO in
      read ic
      >>= function
      | None -> return `Halt
      | Some (Atom "Halt") -> return `Halt
      | Some (List [Atom "Version"; Atom v]) -> return (`Version v)
      | Some _ -> return `Unknown

    let to_sexp =
      let open Csexp in
      function
      | `Version v -> List [Atom "Version"; Atom v]
      | _ -> assert false

    let output oc t = IO.write oc [to_sexp t]
  end

  module V1 = struct
    type t =
      [ `Halt
      | `Unknown
      | `Error of string
      | `Config of (string * string) list
      | `Format of string ]

    let read_input ic =
      let open Csexp in
      let open IO in
      read ic
      >>= function
      | None -> return `Halt
      | Some (List [Atom "Format"; Atom x]) -> return (`Format x)
      | Some (List [Atom "Config"; List l]) ->
          let c =
            List.fold_left
              (fun acc -> function
                | List [Atom name; Atom value] -> (name, value) :: acc
                | _ -> acc )
              [] l
            |> List.rev
          in
          return (`Config c)
      | Some (List [Atom "Error"; Atom x]) -> return (`Error x)
      | Some (Atom "Halt") -> return `Halt
      | Some _ -> return `Unknown

    let to_sexp =
      let open Csexp in
      function
      | `Format x -> List [Atom "Format"; Atom x]
      | `Config c ->
          let l =
            List.map (fun (name, value) -> List [Atom name; Atom value]) c
          in
          List [Atom "Config"; List l]
      | `Error x -> List [Atom "Error"; Atom x]
      | `Halt -> Atom "Halt"
      | _ -> assert false

    let output oc t = IO.write oc [to_sexp t]
  end

  module V2 = struct
    type t =
      [`Halt | `Unknown | `Error of string | `Format of string * format_args]

    let read_input ic =
      let open Csexp in
      let open IO in
      let csexp_to_config csexpl =
        List.filter_map
          (function
            | List [Atom name; Atom value] -> Some (name, value) | _ -> None )
          csexpl
      in
      read ic
      >>= function
      | None -> return `Halt
      | Some (List (Atom "Format" :: Atom x :: l)) ->
          let extract args csexp =
            match csexp with
            | List [Atom "Config"; List l] ->
                {args with config= Some (csexp_to_config l)}
            | List [Atom "Path"; Atom path] -> {args with path= Some path}
            | _ -> args
          in
          let args = List.fold_left extract empty_args l in
          return (`Format (x, args))
      | Some (List [Atom "Error"; Atom x]) -> return (`Error x)
      | Some (Atom "Halt") -> return `Halt
      | Some _ -> return `Unknown

    let to_sexp =
      let open Csexp in
      function
      | `Format (x, {path; config}) ->
          let map_config name config =
            let c =
              List.map
                (fun (name, value) -> List [Atom name; Atom value])
                config
            in
            List [Atom name; List c]
          in
          let ofp =
            Option.map (fun path -> List [Atom "Path"; Atom path]) path
          and oconfig = Option.map (map_config "Config") config in
          List
            (List.filter_map
               (fun i -> i)
               [Some (Atom "Format"); Some (Atom x); ofp; oconfig] )
      | `Error x -> List [Atom "Error"; Atom x]
      | `Halt -> Atom "Halt"
      | _ -> assert false

    let output oc t = IO.write oc [to_sexp t]
  end
end

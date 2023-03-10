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

open Extended_ast

type 'a item =
  | Structure : Extended_ast.structure item
  | Signature : Extended_ast.signature item
  | Use_file : Extended_ast.use_file item

type 'a t =
  { attr_loc: Location.t
  ; chunk_loc: Location.t
  ; state: [`Enable | `Disable]
  ; items: 'a list }

let init_loc =
  let pos =
    Lexing.
      {pos_cnum= 0; pos_bol= 0; pos_lnum= 0; pos_fname= !Location.input_name}
  in
  Location.{loc_ghost= false; loc_start= pos; loc_end= pos}

let is_attr (type a) (fg : a list item) (x : a) =
  match (fg, x) with
  | Structure, {pstr_desc= Pstr_attribute x; pstr_loc} -> Some (x, pstr_loc)
  | Signature, {psig_desc= Psig_attribute x; psig_loc} -> Some (x, psig_loc)
  | Use_file, Ptop_def ({pstr_desc= Pstr_attribute x; pstr_loc} :: _) ->
      Some (x, pstr_loc)
  | _ -> None

let is_state_attr fg ~state x =
  let open Option.Monad_infix in
  is_attr fg x
  >>= fun (attr, loc) ->
  Conf.parse_state_attr attr
  >>= fun new_state ->
  match (state, new_state) with
  | `Enable, `Disable -> Some (`Disable, loc)
  | `Disable, `Enable -> Some (`Enable, loc)
  | _ -> None

let last_loc (type a) (fg : a list item) (l : a list) =
  let open Option.Monad_infix in
  match fg with
  | Structure -> List.last l >>| fun x -> x.pstr_loc
  | Signature -> List.last l >>| fun x -> x.psig_loc
  | Use_file -> (
      List.last l
      >>= function
      | Ptop_def x -> List.last x >>| fun x -> x.pstr_loc
      | Ptop_dir x -> Some x.pdir_loc )

let mk ~attr_loc ~chunk_loc state items = {attr_loc; chunk_loc; state; items}

let mk_tmp ~loc state items = mk ~attr_loc:loc ~chunk_loc:loc state items

(* Build chunks of each disabled/enabled code spans. The [chunk_loc] of each
   chunk has an unprecise ending position that needs to be set after looking
   at the following chunk. *)
let split_with_imprecise_locs fg ~state l =
  let init = ([], state) in
  let chunks, _ =
    List.fold_left l ~init ~f:(fun (acc, state) x ->
        match is_state_attr fg ~state x with
        | Some (state, loc) -> (mk_tmp ~loc state [x] :: acc, state)
        | None -> (
          match acc with
          (* first chunk *)
          | [] -> (mk_tmp ~loc:init_loc state [x] :: acc, state)
          | chunk :: t -> ({chunk with items= x :: chunk.items} :: t, state)
          ) )
  in
  List.rev_map chunks ~f:(fun x -> {x with items= List.rev x.items})

(* Extend the [chunk_loc] to make it span until the start of [last_loc]. *)
let extend_end_loc ~last_loc chunk =
  let loc_end = last_loc.Location.loc_start in
  let chunk_loc = {chunk.chunk_loc with loc_end} in
  {chunk with chunk_loc}

(* Update the [chunk_loc] of each chunk by using the loc of the following
   chunk. *)
let extend_end_locs fg l =
  match List.rev l with
  | [] -> []
  | h :: t ->
      (* last chunk *)
      let init =
        let last_loc =
          Option.value (last_loc fg h.items) ~default:h.chunk_loc
        in
        let chunk_loc = {h.chunk_loc with loc_end= last_loc.loc_end} in
        let h = {h with chunk_loc} in
        (h.attr_loc, [h])
      in
      let _, chunks =
        List.fold_left t ~init ~f:(fun (last_loc, acc) chunk ->
            let chunk = extend_end_loc ~last_loc chunk in
            (chunk.attr_loc, chunk :: acc) )
      in
      chunks

let split ~state fg l =
  extend_end_locs fg @@ split_with_imprecise_locs fg ~state l

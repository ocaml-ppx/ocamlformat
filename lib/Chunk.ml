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

let update_conf ?quiet c l = List.fold ~init:c l ~f:(Conf.update ?quiet)

let disabling (c : Conf.t) attr =
  (not c.opr_opts.disable.v)
  && (update_conf ~quiet:true c [attr]).opr_opts.disable.v

let enabling (c : Conf.t) attr =
  c.opr_opts.disable.v
  && not (update_conf ~quiet:true c [attr]).opr_opts.disable.v

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

let is_state_attr fg ~f c x =
  match is_attr fg x with
  | Some (attr, loc) when f c attr -> Some loc
  | _ -> None

let last_loc (type a) (fg : a list item) (l : a list) =
  let open Option.Monad_infix in
  match fg with
  | Structure -> List.last l >>| fun x -> x.pstr_loc
  | Signature -> List.last l >>| fun x -> x.psig_loc
  | Use_file ->
      List.last l
      >>= (function
            | Ptop_def x -> List.last x >>| fun x -> `Item x
            | Ptop_dir x -> Some (`Directive x) )
      >>| fun item -> Ast.location (Tli item)

let mk ~attr_loc ~chunk_loc state items = {attr_loc; chunk_loc; state; items}

let mk_tmp ~loc state items = mk ~attr_loc:loc ~chunk_loc:loc state items

let split (type a) (fg : a list item) c (l : a list) : a t list =
  List.fold_left l ~init:([], c) ~f:(fun (acc, c) x ->
      match is_state_attr fg ~f:disabling c x with
      | Some loc ->
          (mk_tmp ~loc `Disable [x] :: acc, Conf.update_state c `Disable)
      | None -> (
        match is_state_attr fg ~f:enabling c x with
        | Some loc ->
            (mk_tmp ~loc `Enable [x] :: acc, Conf.update_state c `Enable)
        | None -> (
          match acc with
          | [] ->
              let state =
                if c.opr_opts.disable.v then `Disable else `Enable
              in
              (mk_tmp ~loc:init_loc state [x] :: acc, c)
          | chunk :: t -> ({chunk with items= x :: chunk.items} :: t, c) ) ) )
  |> fun (chunks, (_ : Conf.t)) ->
  chunks
  |> function
  | [] -> []
  | h :: t ->
      let init =
        let items = List.rev h.items in
        let last_loc =
          Option.value (last_loc fg items) ~default:h.chunk_loc
        in
        let chunk_loc = {h.chunk_loc with loc_end= last_loc.loc_end} in
        (h.attr_loc, [{h with items; chunk_loc}])
      in
      List.fold_left t ~init ~f:(fun (last_loc, acc) chunk ->
          let chunk =
            { chunk with
              chunk_loc= {chunk.attr_loc with loc_end= last_loc.loc_start}
            ; items= List.rev chunk.items }
          in
          (chunk.attr_loc, chunk :: acc) )
      |> fun ((_ : Location.t), chunks) -> chunks

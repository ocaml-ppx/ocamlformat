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

type state = Enable | Disable of Location.t

type 'a t =
  | Structure : structure t
  | Signature : signature t
  | Use_file : use_file t

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

let is_attr (type a) (fg : a list t) (x : a) =
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

let split fg c l =
  List.fold_left l ~init:([], c) ~f:(fun (acc, c) x ->
      match is_state_attr fg ~f:disabling c x with
      | Some loc -> ((Disable loc, [x]) :: acc, Conf.update_state c `Disable)
      | None -> (
        match is_state_attr fg ~f:enabling c x with
        | Some _ -> ((Enable, [x]) :: acc, Conf.update_state c `Enable)
        | None -> (
          match acc with
          | [] ->
              let chunk =
                if c.opr_opts.disable.v then (Disable init_loc, [x])
                else (Enable, [x])
              in
              (chunk :: acc, c)
          | (st, h) :: t -> ((st, x :: h) :: t, c) ) ) )
  |> fst
  |> List.rev_map ~f:(function state, lx -> (state, List.rev lx))

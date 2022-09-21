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
  (not c.opr_opts.disable)
  && (update_conf ~quiet:true c [attr]).opr_opts.disable

let enabling (c : Conf.t) attr =
  c.opr_opts.disable
  && not (update_conf ~quiet:true c [attr]).opr_opts.disable

let init_loc =
  let pos =
    Lexing.
      {pos_cnum= 0; pos_bol= 0; pos_lnum= 0; pos_fname= !Location.input_name}
  in
  Location.{loc_ghost= false; loc_start= pos; loc_end= pos}

let is_attr (type a) (x : a list t) : a -> _ =
  match x with
  | Structure -> (
      function
      | {pstr_desc= Pstr_attribute attr; pstr_loc} -> Some (attr, pstr_loc)
      | _ -> None )
  | Signature -> (
      function
      | {psig_desc= Psig_attribute attr; psig_loc} -> Some (attr, psig_loc)
      | _ -> None )
  | Use_file -> (
      function
      | Ptop_def ({pstr_desc= Pstr_attribute attr; pstr_loc} :: _) ->
          Some (attr, pstr_loc)
      | _ -> None )

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
                if c.opr_opts.disable then (Disable init_loc, [x])
                else (Enable, [x])
              in
              (chunk :: acc, c)
          | (st, h) :: t -> ((st, x :: h) :: t, c) ) ) )
  |> fst
  |> List.rev_map ~f:(function
       | Enable, lx -> (Enable, List.rev lx)
       | Disable loc, lx -> (Disable loc, List.rev lx) )

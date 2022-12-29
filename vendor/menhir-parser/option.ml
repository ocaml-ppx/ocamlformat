(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let defined = function
  | None ->
      false
  | Some _ ->
      true

let map f = function
  | None ->
      None
  | Some x ->
      Some (f x)

let iter f o =
  match o with
  | None ->
      ()
  | Some x ->
      f x

let fold f o accu =
  match o with
  | None ->
      accu
  | Some x ->
      f x accu

let force = function
  | Some x ->
      x
  | None ->
      assert false

let project = function
  | Some x ->
      x
  | None ->
      (* Presumably, an error message has already been printed. *)
      exit 1

let equal equal o1 o2 =
  match o1, o2 with
  | None, None ->
      true
  | Some x1, Some x2 ->
      equal x1 x2
  | None, Some _
  | Some _, None ->
      false

let sub sub o1 o2 =
  match o1, o2 with
  | None, _ ->
      true
  | Some _, None ->
      false
  | Some x1, Some x2 ->
      sub x1 x2

let hash hash = function
  | Some x ->
      hash x
  | None ->
      Hashtbl.hash None

let value o ~default =
  match o with
  | Some x ->
      x
  | None ->
      default

let split o =
  match o with
  | None ->
      None, None
  | Some (x, x') ->
      Some x, Some x'

let elements o =
  match o with
  | None ->
      []
  | Some x ->
      [x]

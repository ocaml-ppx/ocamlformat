(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**Attributes allow the user to annotate the grammar with information that is
   ignored by Menhir, but can be exploited by other tools, via the SDK. *)

(**An attribute consists of a key, a payload, and a position in the source
   code. The payload string is an uninterpreted piece of text. The position is
   used only as part of error messages. *)
type attribute = {
  key:     key;
  payload: payload;
  origin:  Positions.t;
}

and key =
  string

and payload =
  string

type attributes =
  attribute list

(**[attribute_has_key key attr] determines whether the attribute [attr] has
   key [key]. *)
val attribute_has_key : key -> attribute -> bool

(**[find_attribute key attrs] looks for an attribute that carries the key
   [key] in the list [attrs] and returns its payload. If no such attribute
   exists then [Not_found] is raised. *)
val find_attribute : key -> attributes -> payload

(**[extract_attribute key attrs] looks for an attribute that carries the key
   [key] in the list [attrs] and returns a pair of this attribute (if it
   exists) and the list deprived of this attribute. *)
val extract_attribute : key -> attributes -> attribute option * attributes

(**[transform_attribute key f attrs] transforms the payload of the attribute
   that carries the key [key], if there is one, by applying the function [f]
   to this payload. *)
val transform_attribute : key -> (payload -> payload) -> attributes -> attributes

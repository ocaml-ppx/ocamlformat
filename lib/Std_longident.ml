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

open Longident

module Indexing_op = struct
  type raw =
    { opchars: string
    ; brackets: Asttypes.paren_kind
    ; extended: bool  (** eg. [.*{;..}] *)
    ; has_rhs: bool  (** eg. [.*{}<-] *) }

  let parse ident =
    match String.chop_prefix ~prefix:"." ident with
    | None -> None
    | Some ident ->
        let ident, has_rhs =
          match String.chop_suffix ident ~suffix:"<-" with
          | Some ident -> (ident, true)
          | None -> (ident, false)
        in
        let find_suffix (suffix, brackets, extended) =
          match String.chop_suffix ident ~suffix with
          | None -> None
          | Some opchars -> Some {opchars; brackets; extended; has_rhs}
        in
        List.find_map ~f:find_suffix
          [ ("{}", Asttypes.Brace, false)
          ; ("[]", Bracket, false)
          ; ("()", Paren, false)
          ; ("{;..}", Brace, true)
          ; ("[;..]", Bracket, true)
          ; ("(;..)", Paren, true) ]
end

module Char_id = struct
  let is_kwdop = function
    | '$' | '&' | '*' | '+' | '-' | '/' | '<' | '=' | '>' | '@' | '^' | '|'
     |'!' | '%' | ':' | '?' ->
        true
    | _ -> false

  let is_infixop = function
    | '$' | '%' | '*' | '+' | '-' | '/' | '<' | '=' | '>' | '|' | '&' | '@'
     |'^' | '#' ->
        true
    | _ -> false
end

module String_id = struct
  let is_prefix i =
    match i with
    | "!=" -> false
    | _ -> ( match i.[0] with '!' | '?' | '~' -> true | _ -> false )

  let is_monadic_binding s =
    String.length s > 3
    && (String.is_prefix s ~prefix:"let" || String.is_prefix s ~prefix:"and")
    && Option.is_none
         (String.lfindi s ~pos:3 ~f:(fun _ c -> not (Char_id.is_kwdop c)))

  let is_infix i =
    if Char_id.is_infixop i.[0] then true
    else
      match i with
      | "!=" | "land" | "lor" | "lxor" | "mod" | "::" | ":=" | "asr"
       |"lsl" | "lsr" | "or" | "||" ->
          true
      | _ -> is_monadic_binding i

  let is_hash_getter i =
    let is_infix_char c = Char.equal c '.' || Char_id.is_infixop c in
    match (i.[0], i.[String.length i - 1]) with
    | '#', ('#' | '.') when String.for_all i ~f:is_infix_char -> true
    | _ -> false

  let is_index_op ident = Option.is_some (Indexing_op.parse ident)

  let is_symbol i = is_prefix i || is_infix i || is_index_op i
end

let test ~f = function Longident.Lident i -> f i | _ -> false

let is_prefix = test ~f:String_id.is_prefix

let is_monadic_binding = test ~f:String_id.is_monadic_binding

let is_infix = test ~f:String_id.is_infix

let is_hash_getter = test ~f:String_id.is_hash_getter

let is_index_op i = Longident.last i |> String_id.is_index_op

let is_symbol i = is_prefix i || is_infix i || is_index_op i

let field_alias_str ~field y =
  match field with
  | Ldot (_, x) | Lident x -> String.equal x y
  | Lapply _ -> false

let field_alias ~field = function
  | Lident x -> field_alias_str ~field x
  | Ldot _ | Lapply _ -> false

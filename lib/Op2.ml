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

type infix_kind = Hash_get | Monad | Other

type kind = Monad_bind | Infix of infix_kind

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
      | _ -> false

  let is_hash_getter i =
    let is_infix_char c = Char.equal c '.' || Char_id.is_infixop c in
    match (i.[0], i.[String.length i - 1]) with
    | '#', ('#' | '.') when String.for_all i ~f:is_infix_char -> true
    | _ -> false

  let is_monadic s =
    let is_monadop s =
      match String.sub s ~pos:0 ~len:(min 2 (String.length s)) with
      | ">>" | ">|" | "@@" | "@>" -> true
      | _ -> false
    in
    is_monadop s
    (* "*>>=", "+>>>", "/>>|", etc. *)
    || (String.length s > 3 && is_monadop (String.sub s ~pos:1 ~len:2))

  let kind x =
    if is_monadic_binding x then Some Monad_bind
    else if is_hash_getter x then Some (Infix Hash_get)
    else if is_monadic x then Some (Infix Monad)
    else if is_infix x then Some (Infix Other)
    else None

  let prec ~child x =
    match (x.[0], x) with
    | _, ":=" -> (Prec.ColonEqual, child)
    | _, ("or" | "||") -> (BarBar, child)
    | _, ("&" | "&&") -> (AmperAmper, child)
    | ('=' | '<' | '>' | '|' | '&' | '$'), _ | _, "!=" -> (InfixOp0, child)
    | ('@' | '^'), _ -> (InfixOp1, child)
    | ('+' | '-'), _ -> (InfixOp2, child)
    | '*', _ when String.(x <> "*") && Char.(x.[1] = '*') -> (InfixOp4, child)
    | ('*' | '/' | '%'), _ | _, ("lor" | "lxor" | "mod" | "land") ->
        (InfixOp3, child)
    | _, ("lsl" | "lsr" | "asr") -> (InfixOp4, child)
    | '#', _ -> (HashOp, child)
    | _ -> (Apply, if is_infix x then child else Assoc.Non)
end

module Longident = struct
  let kind = function Longident.Lident i -> String_id.kind i | _ -> None
end

module Exp = struct
  open Extended_ast

  let kind = function
    | {pexp_desc= Pexp_ident {txt= i; _}; pexp_attributes= []; _} ->
        Longident.kind i
    | _ -> None
end

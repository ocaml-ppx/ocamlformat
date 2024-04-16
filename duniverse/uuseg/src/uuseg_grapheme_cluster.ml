(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* These are the rules as found in [1], with property values aliases [2]
   substituted.

   GB1.               sot ÷ Any
   GB2.               Any ÷ eot
   GB3.                CR × LF
   GB4.        (CN|CR|LF) ÷
   GB5.                   ÷ (CN|CR|LF)
   GB6.                 L × (L|V|LV|LVT)
   GB7.            (LV|V) × (V|T)
   GB8.           (LVT|T) × T
   GB9.                   × (EX|ZWJ)
   GB9a.                  × SM
   GB9b.               PP ×
   GB9c. \p{InCB=Consonant} [\p{InCB=Extend}\p{InCB=Linker}]*
         \p{InCB=Linker} [\p{InCB=Extend}\p{InCB=Linker}]*
         ×
         \p{InCB=Consonant}
   GB11. \p{Extended_Pictographic} EX* ZWJ x \p{Extended_Pictographic}
   GB12.  sot (RI RI)* RI × RI
   GB13.   [^RI] (RI RI)* × RI
   GB999.             Any ÷ Any

   [1]: http://www.unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries
   [2]: http://www.unicode.org/Public/7.0.0/ucd/PropertyValueAliases.txt
   [3]: http://www.unicode.org/Public/7.0.0/ucd/auxiliary/GraphemeBreakTest.html

   By the structure of the rules we see that grapheme clusters
   boundaries can *mostly* be determined by simply looking at the
   grapheme cluster break property value of the character on the left
   and on the right of a boundary. The exceptions are GB9c, GB10 and GB12-13
   which are handled specially by enriching the segmenter state in
   a horribly ad-hoc fashion. *)

type gcb =
  | CN | CR | EX | EB | EBG | EM | GAZ | L | LF | LV | LVT | PP | RI
  | SM | T | V | XX | ZWJ | Sot

type incb = Consonant | Extend | Linker | None'

(* WARNING. The indexes used here need to be synchronized with those
   assigned by uucp for Uucp.Break.Low.{grapheme_cluster,indic_conjunct_break}
*)

let byte_to_gcb =
  [| CN; CR; EX; EB; EBG; EM; GAZ; L; LF; LV; LVT; PP; RI;
     SM; T; V; XX; ZWJ; |]

let gcb u = byte_to_gcb.(Uucp.Break.Low.grapheme_cluster u)

let byte_to_incb = [| Consonant; Extend; Linker; None' |]
let incb u = byte_to_incb.(Uucp.Break.Low.indic_conjunct_break u)

type left_gb9c_state = (* Ad-hoc state for matching GB9c *)
| Reset | Has_consonant | Has_linker

type state =
| Fill  (* get next uchar to decide boundary. *)
| Flush (* an uchar is buffered, client needs to get it out with `Await. *)
| End   (* `End was added. *)

type t =
  { mutable state : state;                                 (* current state. *)
    mutable left_gb9c : left_gb9c_state;         (* state for matching gb9c. *)
    mutable left : gcb;            (* break property value left of boundary. *)
    mutable left_odd_ri : bool;             (* odd number of RI on the left. *)
    mutable left_emoji_seq : bool;                 (* emoji seq on the left. *)
    mutable buf : [ `Uchar of Uchar.t ] }                 (* bufferized add. *)

let nul_buf = `Uchar (Uchar.unsafe_of_int 0x0000)

let create () =
  { state = Fill;
    left_gb9c = Reset;
    left = Sot; left_odd_ri = false; left_emoji_seq = false;
    buf = nul_buf (* overwritten *); }

let copy s = { s with state = s.state; }

let gb9c_match s right_incb = match s.left_gb9c, right_incb with
| Has_linker, Consonant -> true
| _, _ -> false

let break s right right_incb right_u = match s.left, right with
| (* GB1 *)   Sot, _ -> true
  (* GB2 is handled by `End *)
| (* GB3 *)   CR, LF -> false
| (* GB4 *)   (CN|CR|LF), _ -> true
| (* GB5 *)   _, (CN|CR|LF) -> true
| (* GB6 *)   L, (L|V|LV|LVT) -> false
| (* GB7 *)   (LV|V), (V|T) -> false
| (* GB8 *)   (LVT|T), T -> false
| (* GB9+a *) _, (EX|ZWJ|SM) -> false
| (* GB9b *)  PP, _ -> false
| (* GB9c *)  _, _ when gb9c_match s right_incb -> false
| (* GB11 *)  ZWJ, _ when s.left_emoji_seq &&
                          Uucp.Emoji.is_extended_pictographic right_u -> false
| (* GB12+13 *) RI, RI when s.left_odd_ri -> false
| (* GB999 *) _, _ -> true

let update_left s right right_incb right_u =
  s.left <- right;
  begin match s.left with
  | EX | ZWJ ->
      s.left_odd_ri <- false
      (* keep s.left_emoji_seq as is *)
  | RI ->
      s.left_odd_ri <- not s.left_odd_ri;
      s.left_emoji_seq <- false;
  | _ when Uucp.Emoji.is_extended_pictographic right_u ->
      s.left_odd_ri <- false;
      s.left_emoji_seq <- true;
  | _ ->
      s.left_odd_ri <- false;
      s.left_emoji_seq <- false
  end;
  s.left_gb9c <- begin match right_incb with
  | None' -> Reset
  | Consonant -> Has_consonant
  | Linker when s.left_gb9c = Has_consonant -> Has_linker
  | Extend | Linker -> s.left_gb9c
  end

let add s = function
| `Uchar u as add ->
    begin match s.state with
    | Fill ->
        let right = gcb u in
        let right_incb = incb u in
        let break = break s right right_incb u in
        update_left s right right_incb u;
        if not break then add else
        (s.state <- Flush; s.buf <- add; `Boundary)
    | Flush -> Uuseg_base.err_exp_await add
    | End -> Uuseg_base.err_ended add
    end
| `Await ->
    begin match s.state with
    | Flush -> s.state <- Fill; (s.buf :> Uuseg_base.ret)
    | End -> `End
    | Fill -> `Await
    end
| `End ->
    begin match s.state with
    | Fill -> s.state <- End; if s.left = Sot then `End else `Boundary
    | Flush -> Uuseg_base.err_exp_await `End
    | End -> Uuseg_base.err_ended `End
    end

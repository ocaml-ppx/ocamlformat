(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* These are the rules as found in [1], with property values aliases [2]
   substituted.

   WB1.                 sot ÷ Any
   WB2.                 Any ÷ eot
   WB3.                  CR × LF
   WB3a.         (NL|CR|LF) ÷
   WB3b.                    ÷ (NL|CR|LF)
   WB3c.                ZWJ × \p{Extended_Pictographic}
   WB3d.          WSegSpace × WSegSpace
   WB4.  X (Extend|FO|ZWJ)* → X
   WB5.             (LE|HL) × (LE|HL)
   WB6.             (LE|HL) × (ML|MB|SQ) (LE|HL)
   WB7.  (LE|HL) (ML|MB|SQ) × (LE|HL)
   WB7a.                 HL × SQ
   WB7b.                 HL × DQ HL
   WB7c.              HL DQ × HL
   WB8.                  NU × NU
   WB9.             (LE|HL) × NU
   WB10.                 NU × (LE|HL)
   WB11.      NU (MN|MB|SQ) × NU
   WB12.                 NU × (MN|MB|SQ) NU
   WB13.                 KA × KA
   WB13a.  (LE|HL|NU|KA|EX) × EX
   WB13b.                EX × (LE|HL|NU|KA)
   WB15     sot (RI RI)* RI × RI
   WB15   [^RI] (RI RI)* RI × RI
   WB13c.                RI × RI
   WB999.               Any ÷ Any

   [1]: http://www.unicode.org/reports/tr29/#Word_boundaries
   [2]: http://www.unicode.org/Public/7.0.0/ucd/PropertyValueAliases.txt

   Given the structure of the rules we keep a window of four word
   break property value slots, two on the left, two on the right of a
   boundary and pattern match these slots to find the rule that
   applies. Because of WB4 these slots may actually correspond to more
   than one character and we need to bufferize the data for the slot r0.

   Besides we maintain two views of the window slots, one which has the
   word break property of concrete characters and another one that has
   the word break property as seen by the WB4 rewrite rule (for the
   right slots this coincides), see the *_wb4 fields in the state.

                            ---??--->
                     +----+----++----+----+
                  ...| l1 | l0 || r0 | r1 |
                     +----+----++----+----+
   already returned to client /  \ buffered in segmenter *)

type word =
| CR | DQ | EX | EB | EBG | EM | Extend | FO | GAZ | HL | KA | LE | LF
| MB | ML | MN | NL | NU | RI | SQ | WSegSpace | XX | ZWJ | Invalid | Sot | Eot

(* WARNING. The indexes used here need to be synchronized with those
   assigned by uucp for Uucp.Break.Low.word. *)

let byte_to_word =
  [| CR; DQ; EX; EB; EBG; EM; Extend; FO; GAZ; HL; KA; LE; LF;
     MB; ML; MN; NL; NU; RI; SQ; WSegSpace; XX; ZWJ |]

let word u = byte_to_word.(Uucp.Break.Low.word u)

type state =
| Fill   (* fill slots on the right of boundary. *)
| Flush  (* flush the first element of slot r0 to get to next boundary. *)
| Decide (* decide boundary of slot r0 *)

type t =
  { mutable state : state;
    mutable l1 : word; mutable l1_wb4 : word; (* l1 according to wb4 *)
    mutable l0 : word; mutable l0_wb4 : word; (* l0 according to wb4 *)
    mutable l0_odd_ri : bool; (* odd number of RI on left of break point. *)
    mutable r0 : word; (* of first element in r0_data. *)
    mutable r0_data : Uuseg_buf.t; (* data in r0 *)
    mutable r1 : word;
    mutable r1_data : [`Uchar of Uchar.t ] option; (* data in r1 (if any) *)
    mutable ended : bool; (* [true] if [`End] was added. *) }

let create () =
  { state = Fill;
    l1 = Invalid; l1_wb4 = Invalid;
    l0 = Sot; l0_wb4 = Sot;
    l0_odd_ri = false;
    r0 = Invalid;
    r0_data = Uuseg_buf.create 13;
    r1 = Invalid;
    r1_data = None;
    ended = false; }

let copy s = { s with r0_data = Uuseg_buf.copy s.r0_data; }

let has_break s = match s.l1, s.l0 (**),(**) s.r0, s.r1 with
| (* WB1 *)  _, Sot, _, _ -> true
| (* WB2 *)  _, _, Eot, _ -> true
| (* WB3 *)  _, CR, LF, _ -> false
| (* WB3a *) _, (NL|CR|LF), _, _ -> true
| (* WB3b *) _, _, (NL|CR|LF), _ -> true
| (* WB3c *) _, ZWJ, _, _ when
    not (Uuseg_buf.empty s.r0_data) &&
    Uucp.Emoji.is_extended_pictographic (Uuseg_buf.get_first s.r0_data) ->
    false
| (* WB3d *)  _, WSegSpace, WSegSpace, _ -> false
| _ -> (* apply WB4 rewrite and match *)
    match s.l1_wb4, s.l0_wb4 (**),(**) s.r0, s.r1 with
    | (* WB4 *)   _, _, (Extend|FO|ZWJ), _ -> false
    | (* WB5 *)   _, (LE|HL), (LE|HL), _ -> false
    | (* WB6 *)   _, (LE|HL), (ML|MB|SQ), (LE|HL) -> false
    | (* WB7 *)   (LE|HL), (ML|MB|SQ), (LE|HL), _ -> false
    | (* WB7a *)  _, HL, SQ, _ -> false
    | (* WB7b *)  _, HL, DQ, HL -> false
    | (* WB7c *)  HL, DQ, HL, _ -> false
    | (* WB8 *)   _, NU, NU, _ -> false
    | (* WB9 *)   _, (LE|HL), NU, _ -> false
    | (* WB10 *)  _, NU, (LE|HL), _ -> false
    | (* WB11 *)  NU, (MN|MB|SQ), NU, _ -> false
    | (* WB12 *)  _, NU, (MN|MB|SQ), NU -> false
    | (* WB13 *)  _, KA, KA, _ -> false
    | (* WB13a *) _, (LE|HL|NU|KA|EX), EX, _ -> false
    | (* WB13b *) _, EX, (LE|HL|NU|KA), _ -> false
    | (* WB15-16 *) _, RI, RI, _ when s.l0_odd_ri -> false
    | (* WB999 *) _, _, _, _ -> true

let next s = (* moves to the next boundary and returns the char in r0 *)
  s.l1 <- s.l0;
  s.l0 <- s.r0;
  (* The lb4 window only moves when r0 is not one of the absorbed chars. *)
  begin match s.r0 with
  | Extend | FO | ZWJ -> () (* wb4 window doesn't move *)
  | _ ->
      s.l1_wb4 <- s.l0_wb4;
      s.l0_wb4 <- s.r0;
      s.l0_odd_ri <- (match s.l0_wb4 with RI -> not s.l0_odd_ri | _ -> false);
  end;
  let ret = Uuseg_buf.flush s.r0_data in
  match Uuseg_buf.empty s.r0_data with
  | false -> s.r0 <- word (Uuseg_buf.get_first s.r0_data); ret
  | true ->
      s.r0 <- s.r1;
      s.r1 <- Invalid;
      match s.r1_data with
      | None -> ret
      | Some u -> Uuseg_buf.add s.r0_data u; s.r1_data <- None; ret

let need_fill s = Uuseg_buf.empty s.r0_data || s.r1_data = None
let flush s = match s.ended with
| false ->
    let ret = next s in
    (if need_fill s then s.state <- Fill else s.state <- Decide);
    ret
| true ->
    match s.r0 with
    | Eot -> `End
    | _ -> s.state <- Decide; next s

let decide s = if has_break s then (s.state <- Flush; `Boundary) else flush s

let add s = function
| `Uchar u as add ->
    if s.ended then Uuseg_base.err_ended add else
    begin match s.state with
    | Fill when Uuseg_buf.empty s.r0_data ->
        Uuseg_buf.add s.r0_data add; s.r0 <- word u; `Await
    | Fill ->
        begin match word u with
        | Extend | FO | ZWJ -> (* WB4 *) Uuseg_buf.add s.r0_data add; `Await
        | word -> s.r1_data <- Some add; s.r1 <- word; decide s
        end
    | Flush | Decide -> Uuseg_base.err_exp_await add
    end
| `Await ->
    begin match s.state with
    | Flush -> flush s
    | Decide -> decide s
    | Fill -> `Await
    end
| `End ->
    if s.ended then Uuseg_base.err_ended `End else
    begin match s.state with
    | Fill ->
        s.ended <- true;
        (if Uuseg_buf.empty s.r0_data then s.r0 <- Eot else s.r1 <- Eot);
        if s.l0 = Sot && s.r0 = Eot then (* empty string *) `End else
        decide s
    | Flush | Decide -> Uuseg_base.err_exp_await `End
    end

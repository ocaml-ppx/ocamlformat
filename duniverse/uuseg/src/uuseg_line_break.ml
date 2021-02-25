(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* These are the rules as found in [1]
   LB1 (per suggestion)
                           (AI|SG|XX) → AL
                      SA when (Mn|Mc) → CM
                                   SA → AL
                                   CJ → NS
   LB2                            sot ×
   LB3                                ! eot
   LB4                             BK !
   LB5                             CR × LF
                           (CR|LF|NL) !
   LB6                                × (BK|CR|LF|NL)
   LB7                                × (SP|ZW)
   LB8                         ZW SP* ÷
   LB8a                           ZWJ ×
   LB9  ¬(BK|CR|LF|NL|SP|ZW as X) (CM|ZWJ) * → X
   LB10                      (CM|ZWJ) → AL
   LB11                               × WJ
                                   WJ ×
   LB12                            GL ×
   LB12a                  ¬(SP|BA|HY) × GL
   LB13                               × (CL|CP|CP30|EX|IS|SY)
   LB14                 (OP|OP30) SP* ×
   LB15                        QU SP* × (OP|OP30)
   LB16              (CL|CP|CP30) SP* × NS
   LB17                        B2 SP* × B2
   LB18                            SP ÷
   LB19                               × QU
                                   QU ×
   LB20                               ÷ CB
                                   CB ÷
   LB21                               × (BA|HY|NS)
                                   BB ×
   LB21a                   HL (HY|BA) ×
   LB21b                           SY × HL
   LB22                               × IN
   LB23                       (AL|HL) × NU
                                   NU × (AL|HL)
   LB23a                           PR × (ID|EB|EM)
                           (ID|EB|EM) × PO
   LB24                       (PR|PO) × (AL|HL)
                              (AL|HL) × (PR|PO)
   LB25               (CL|CP|CP30|NU) × (PO|PR)
                              (PO|PR) × (OP|OP30)
                  (PO|PR|HY|IS|NU|SY) × NU
   LB26                            JL × (JL|JV|H2|H3)
                              (JV|H2) × (JV|JT)
                              (JT|H3) × JT
   LB27              (JL|JV|JT|H2|H3) × (IN|PO)
                                   PR × (JL|JV|JT|H2|H3)
   LB28                       (AL|HL) × (AL|HL)
   LB29                            IS × (AL|HL)
   LB30                    (AL|HL|NU) × OP30
                                 CP30 × (AL|HL|NU)
   LB30a              sot (RI RI)* RI × RI
                    [^RI] (RI RI)* RI × RI
   LB30b                           EB × EM
   LB31                           ALL ÷
                                      ÷ ALL

   [1]: http://www.unicode.org/reports/tr14/#Algorithm
   [2]: http://www.unicode.org/Public/7.0.0/ucd/PropertyValueAliases.txt

   Given the structure of the rules we keep a window of three line
   break property value slots, two on the left, one on the right of a
   boundary and pattern match these slots to find the rule that
   applies. Because of LB9 these slots may actually correspond to more
   than one character and we need to bufferize the data for the slot
   on the right.

   Besides we maintain two views of the window slots, one which has
   the word berak property of concrete characters and another one
   that has the word break property as seen by the LB9 rule and
   those that have SP* elements.


                            ---??--->
                     +----+----++----+
                  ...| l1 | l0 || r0 |
                     +----+----++----+
   already returned to client /  \ buffered in segmenter *)

type line =
  | AI | AL | B2 | BA | BB | BK | CB | CJ | CL | CM | CP
  | CR | EX | EB | EM | GL | H2 | H3 | HL | HY | ID | IN
  | IS | JL | JT | JV | LF | NL | NS | NU | OP | PO | PR
  | QU | RI | SA | SG | SP | SY | WJ | XX | ZW | ZWJ | Invalid | Sot | Eot
  (* Added to handle LB30 *)
  | OP30 | CP30

(* WARNING. The indexes used here need to be synchronized with those
   assigned by uucp for Uucp.Break.Low.line_break. *)

let byte_to_line =
  [| AL (* LB1 AI → AL *); AL; B2; BA; BB; BK; CB; NS (* LB1 CJ → NS *); CL;
     CM; CP; CR; EX; EB; EM; GL; H2; H3; HL; HY; ID; IN; IS; JL; JT; JV; LF;
     NL; NS; NU; OP; PO; PR; QU; RI; SA; AL (* LB1 SG → AL *); SP; SY; WJ;
     AL (* LB1 XX → AL *); ZW; ZWJ |]

let line u = match byte_to_line.(Uucp.Break.Low.line u) with
| SA -> (* LB1 for SA *)
    begin match Uucp.Gc.general_category u with
    | `Mn | `Mc -> CM
    | _ -> AL
    end
| OP -> (* Decompose because of LB30 *)
    begin match Uucp.Break.east_asian_width u with
    | `F | `W | `H -> OP
    | _ -> OP30
    end;
| CP -> (* Decompose because of LB30 *)
    begin match Uucp.Break.east_asian_width u with
    | `F | `W | `H -> CP
    | _ -> CP30
    end;
| l -> l

type state =
| Fill (* fill slot on the right of boundary. *)
| Flush (* flush the first lement of slot r0 to get to next boundary. *)
| Decide (* decide boundary of slot r0. *)

type t =
  { mutable state : state;                                 (* current state. *)
    mutable l1 : line; mutable l1_rewrite : line; (* l1 according to lb9/lb10 *)
    mutable l0 : line; mutable l0_rewrite : line; (* l0 according to lb9/lb10 *)
    mutable l0_odd_ri : bool; (* odd number of RI on the left of break point. *)
    mutable r0 : line; (* of first element in r0_data *)
    mutable r0_data : [`Uchar of Uchar.t ]; (* data in r0 *)
    mutable mandatory : bool; (* [true] if break is mandatory. *) }

let nul_buf = `Uchar (Uchar.unsafe_of_int 0x0000)
let create () =
  { state = Fill;
    l1 = Invalid; l1_rewrite = Invalid;
    l0 = Sot; l0_rewrite = Sot;
    l0_odd_ri = false;
    r0 = Invalid;
    r0_data = nul_buf (* overwritten *);
    mandatory = false; }

let mandatory s = s.mandatory
let copy s = { s with state = s.state }

let lb10_rewrite = function CM | ZWJ -> AL | l -> l
let is_lb9_X = function  BK | CR | LF | NL | SP | ZW | Sot -> false | _ -> true
let is_lb12_l0 = function SP | BA | HY -> false | _ -> true

let has_break s = (* N.B. sets s.mandatory by side effect. *)
  let mandatory s = s.mandatory <- true; true in
  s.mandatory <- false;
  match s.l1, s.l0 (**),(**) s.r0 with
  (* LB1 is handled by [byte_to_line] and [line]. *)
  | (* LB2 *)  _, Sot, _ -> false
  | (* LB3 *)  _, _, Eot -> mandatory s
  | (* LB4 *)  _, BK, _ -> mandatory s
  | (* LB5 *)  _, CR, LF -> false
  |            _, (CR|LF|NL), _ -> mandatory s
  | (* LB6 *)   _, _, (BK|CR|LF|NL) -> false
  | (* LB7 *)   _, _, (SP|ZW) -> false
  | (* LB8 *)  _, ZW, _ -> true
  |            _(* ZW *), _(* SP* *), _ when s.l1_rewrite = ZW &&
                                             s.l0_rewrite = SP -> true
  | (* LB8a *) _, ZWJ, _ -> false
  | (* LB9 implicitely entails  ¬(BK|CR|LF|NL|SP|ZW as X) × (CM|ZWJ) *)
               _, _x, (CM|ZWJ) when is_lb9_X s.l0_rewrite -> false
  | _ -> (* apply LB9/LB10 rewrite and match *)
      let l1 = if is_lb9_X s.l1_rewrite then s.l1_rewrite else s.l1 in
      let l0 = if is_lb9_X s.l0_rewrite then s.l0_rewrite else s.l0 in
      match (lb10_rewrite l1), (lb10_rewrite l0), (lb10_rewrite s.r0) with
      | (* LB11 *)  _, _, WJ -> false
      |             _, WJ, _ -> false
      | (* LB12 *)  _, GL, _ -> false
      | (* LB12a *) _, l0, GL when is_lb12_l0 l0 -> false
      | (* LB13 *)  _, _, (CL|CP|CP30|EX|IS|SY) -> false
      | (* LB14 *)  _, (OP|OP30), _ -> false
      |             (OP|OP30), SP, _ -> false
      | (* LB15 *)  _, QU, (OP|OP30) -> false
      |             QU, SP, (OP|OP30) -> false
      | (* LB16 *)  _, (CL|CP|CP30), NS -> false
      |             (CL|CP|CP30), SP, NS -> false
      | (* LB17 *)  _, B2, B2 -> false
      |             B2, SP, B2 -> false
      | (* LB18 *)  _, SP, _ -> true
      | (* LB19 *)  _, _, QU -> false
      |             _, QU, _ -> false
      | (* LB20 *)  _, _, CB -> true
      |             _, CB, _ -> true
      | (* LB21 *)  _, _, (BA|HY|NS) -> false
      |             _, BB, _ -> false
      | (* LB21a *) HL, (HY|BA), _ -> false
      | (* LB21b *) _, SY, HL -> false
      | (* LB22 *)  _, _, IN -> false
      | (* LB23 *)  _, (AL|HL), NU -> false
      |             _, NU, (AL|HL) -> false
      | (* LB23a *) _, PR, (ID|EB|EM) -> false
      |             _, (ID|EB|EM), PO -> false
      | (* LB24 *)  _, (PR|PO), (AL|HL) -> false
      |             _, (AL|HL), (PR|PO) -> false
      | (* LB25 *)  _, (CL|CP|CP30|NU), (PO|PR) -> false
      |             _, (PO|PR), (OP|OP30) -> false
      |             _, (PO|PR|HY|IS|NU|SY), NU -> false
      | (* LB26 *)  _, JL, (JL|JV|H2|H3) -> false
      |             _, (JV|H2), (JV|JT) -> false
      |             _, (JT|H3), JT -> false
      | (* LB27 *)  _, (JL|JV|JT|H2|H3), PO -> false
      |             _, PR, (JL|JV|JT|H2|H3) -> false
      | (* LB28 *)  _, (AL|HL), (AL|HL) -> false
      | (* LB29 *)  _, IS, (AL|HL) -> false
      | (* LB30 *)  _, (AL|HL|NU), OP30 -> false
      |             _, CP30, (AL|HL|NU) -> false
      | (* LB30a *) _, RI, RI when s.l0_odd_ri -> false
      | (* LB30b *) _, EB, EM -> false
      | (* LB31 *)  _, _, _ -> true

let next s = (* moves to the next boundary *)
  s.l1 <- s.l0;
  s.l0 <- s.r0;
  (* Only move rewrite window if l0_rewrite doesn't absorb the char
     by rule LB9 or the various SP* *)
  begin match s.r0 with
  | CM | ZWJ when is_lb9_X s.l0_rewrite -> ()
  | SP when s.l0_rewrite = SP -> ()
  | _ ->
      s.l1_rewrite <- s.l0_rewrite;
      s.l0_rewrite <- s.r0;
      s.l0_odd_ri <-
        (match s.l0_rewrite with RI -> not s.l0_odd_ri | _ -> false);
  end;
  s.r0 <- Invalid

let ended s = s.r0 = Eot
let flush s =
  if ended s then `End else
  (next s; s.state <- Fill; (s.r0_data :> Uuseg_base.ret))

let decide s = if has_break s then (s.state <- Flush; `Boundary) else flush s

let add s = function
| `Uchar u as add ->
    if ended s then Uuseg_base.err_ended add else
    begin match s.state with
    | Fill -> s.r0_data <- add; s.r0 <- line u; decide s
    | Flush | Decide -> Uuseg_base.err_exp_await add
    end
| `Await ->
    begin match s.state with
    | Flush -> flush s
    | Decide -> decide s
    | Fill -> `Await
    end
| `End ->
    if ended s then Uuseg_base.err_ended `End else
    begin match s.state with
    | Fill -> s.r0 <- Eot; if s.l0 = Sot then (* eps *) `End else decide s
    | Flush | Decide -> Uuseg_base.err_exp_await `End
    end

(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

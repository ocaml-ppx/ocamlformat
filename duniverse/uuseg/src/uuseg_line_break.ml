(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
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
   LB15a  (sot|BK|CR|LF|NL|OP|QU|QU_Pi|QU_Pf|GL|SP|ZW) QU_Pi SP* ×
   LB15b  × QU_Pf (SP|GL|WJ|CL|QU|CP|EX|IS|SY|BK|CR|LF|NL|ZW|eot)
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
   LB23               (AL|AL_circ|HL) × NU
                                   NU × (AL|AL_circ|HL)
   LB23a                           PR × (ID|ID30b|EB|EM)
                     (ID|ID30b|EB|EM) × PO
   LB24                       (PR|PO) × (AL|AL_circ|HL)
                      (AL|AL_circ|HL) × (PR|PO)
   LB25               (CL|CP|CP30|NU) × (PO|PR)
                              (PO|PR) × (OP|OP30)
                  (PO|PR|HY|IS|NU|SY) × NU
   LB26                            JL × (JL|JV|H2|H3)
                              (JV|H2) × (JV|JT)
                              (JT|H3) × JT
   LB27              (JL|JV|JT|H2|H3) × (IN|PO)
                                   PR × (JL|JV|JT|H2|H3)
   LB28               (AL|AL_circ|HL) × (AL|AL_circ|HL)
   LB28a                           AP × (AK|AL_circ|AS)
                      (AK|AL_circ|AS) × (VF|VI)
                   (AK|AL_circ|AS) VI × (AK|AL_circ)
                      (AK|AL_circ|AS) × (AK|AL_circ|AS) VF
   LB29                            IS × (AL|HL)
   LB30            (AL|AL_circ|HL|NU) × OP30
                                 CP30 × (AL|AL_circ|HL|NU)
   LB30a              sot (RI RI)* RI × RI
                    [^RI] (RI RI)* RI × RI
   LB30b                           EB × EM
                                ID30b x EM
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
                +----+----+----++----+----+
            ... | l2 | l1 | l0 || r0 | r1 |
                +----+----+----++----+----+
   already returned to client /  \ buffered in segmenter *)

type line =
  | AI | AK | AL | AP | AS | B2 | BA | BB | BK | CB | CJ | CL | CM | CP
  | CR | EX | EB | EM | GL | H2 | H3 | HL | HY | ID | IN
  | IS | JL | JT | JV | LF | NL | NS | NU | OP | PO | PR
  | QU | RI | SA | SG | SP | SY | VF | VI | WJ | XX | ZW | ZWJ | Invalid | Sot
  | Eot
  (* Added to handle the U+255C constant in LB28a. We need to split AL (the
     class of U+255C), the full set is AL + AL_circ *)
  | AL_circ
  (* Added to handle LB15{a,b}. We need to split QU (the full set is
     QU + QU_Pf + QU_Pi *)
  | QU_Pf
  | QU_Pi
  (* Added to handle LB30 *)
  | OP30 | CP30
  (* Added to handle LB30b *)
  | ID30b

(* WARNING. The indexes used here need to be synchronized with those
   assigned by uucp for Uucp.Break.Low.line_break. *)

let byte_to_line =
  [| AL (* LB1 AI → AL *); AK; AL; AP; AS; B2; BA; BB; BK; CB;
     NS (* LB1 CJ → NS *); CL;
     CM; CP; CR; EX; EB; EM; GL; H2; H3; HL; HY; ID; IN; IS; JL; JT; JV; LF;
     NL; NS; NU; OP; PO; PR; QU; RI; SA; AL (* LB1 SG → AL *); SP; SY; VF; VI;
     WJ; AL (* LB1 XX → AL *); ZW; ZWJ |]

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
    end
| CP -> (* Decompose because of LB30 *)
    begin match Uucp.Break.east_asian_width u with
    | `F | `W | `H -> CP
    | _ -> CP30
    end
| ID -> (* Decompose because of LB30b, this assumption is tested in test.ml *)
    if Uucp.Emoji.is_extended_pictographic u &&
       Uucp.Gc.general_category u = `Cn then ID30b else ID
| AL -> (* Decompose because of LB28a *)
    if Uchar.to_int u = 0x25CC then AL_circ else AL
| QU -> (* Decompose because of LB15{a,b} *)
    begin match Uucp.Gc.general_category u with
    | `Pf -> QU_Pf
    | `Pi -> QU_Pi
    | _ -> QU
    end
| l -> l

type state =
| Fill (* fill slot on the right of boundary. *)
| Flush (* flush the first lement of slot r0 to get to next boundary. *)
| Decide (* decide boundary of slot r0. *)

type t =
  { mutable state : state;                                 (* current state. *)
    mutable l2 : line; mutable l2_rewrite : line; (* l2 according to LB9/LB10 *)
    mutable l1 : line; mutable l1_rewrite : line; (* l1 according to LB9/LB10 *)
    mutable l0 : line; mutable l0_rewrite : line; (* l0 according to LB9/LB10 *)
    mutable l0_odd_ri : bool; (* odd number of RI on the left of break point. *)
    mutable r0 : line; (* of element in r0_data *)
    mutable r0_data : [`Uchar of Uchar.t ]; (* data in r0 *)
    mutable r1 : line; (* of element in r1_data *)
    mutable r1_data : [`Uchar of Uchar.t ]; (* data in r1 *)
    mutable mandatory : bool; (* [true] if break is mandatory. *)
    mutable ended : bool; (* [true] if [`End was added]. *) }

let nul_buf = `Uchar (Uchar.unsafe_of_int 0x0000)
let create () =
  { state = Fill;
    l2 = Invalid; l2_rewrite = Invalid;
    l1 = Invalid; l1_rewrite = Invalid;
    l0 = Sot; l0_rewrite = Sot;
    l0_odd_ri = false;
    r0 = Invalid;
    r0_data = nul_buf (* overwritten *);
    r1 = Invalid;
    r1_data = nul_buf (* overwritten *);
    mandatory = false;
    ended = false }

let mandatory s = s.mandatory
let copy s = { s with state = s.state }

let lb10_rewrite = function CM | ZWJ -> AL | l -> l
let is_lb9_X = function  BK | CR | LF | NL | SP | ZW | Sot -> false | _ -> true
let is_lb12_l0 = function SP | BA | HY -> false | _ -> true

let has_break s = (* N.B. sets s.mandatory by side effect. *)
  let mandatory s = s.mandatory <- true; true in
  s.mandatory <- false;
  (* NB. s.l2 and s.r1 are not needed here. *)
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
               _, x, (CM|ZWJ) when is_lb9_X s.l0_rewrite -> false
  | _ -> (* apply LB9/LB10 rewrite and match *)
      let l2 = if is_lb9_X s.l2_rewrite then s.l2_rewrite else s.l2 in
      let l1 = if is_lb9_X s.l1_rewrite then s.l1_rewrite else s.l1 in
      let l0 = if is_lb9_X s.l0_rewrite then s.l0_rewrite else s.l0 in
      match (lb10_rewrite l2), (lb10_rewrite l1), (lb10_rewrite l0),
            (lb10_rewrite s.r0), (lb10_rewrite s.r1) with
      | (* LB11 *)  _, _, _, WJ, _ -> false
      |             _, _, WJ, _, _ -> false
      | (* LB12 *)  _, _, GL, _, _ -> false
      | (* LB12a *) _, _, l0, GL, _ when is_lb12_l0 l0 -> false
      | (* LB13 *)  _, _, _, (CL|CP|CP30|EX|IS|SY), _ -> false
      | (* LB14 *)  _, _, (OP|OP30), _, _ -> false
      |             _, (OP|OP30), SP, _, _ -> false
      (* LB15a *)
      |             _, (Sot|BK|CR|LF|NL|OP|OP30|QU|QU_Pi|QU_Pf|GL|SP|ZW), QU_Pi,
                    _, _ -> false
      |            (Sot|BK|CR|LF|NL|OP|OP30|QU|QU_Pi|QU_Pf|GL|SP|ZW), QU_Pi,
                   SP,
                   _, _ -> false
      | (* LB15b *) _, _, _,
                    QU_Pf, (SP|GL|WJ|CL|QU|QU_Pi|QU_Pf|CP|CP30|EX|IS|SY|BK|CR|
                            LF|NL|ZW|Eot) -> false
      | (* LB16 *)  _, _, (CL|CP|CP30), NS, _ -> false
      |             _, (CL|CP|CP30), SP, NS, _ -> false
      | (* LB17 *)  _, _, B2, B2, _ -> false
      |             _, B2, SP, B2, _ -> false
      | (* LB18 *)  _, _, SP, _, _ -> true
      | (* LB19 *)  _, _, _, (QU|QU_Pi|QU_Pf), _ -> false
      |             _, _, (QU|QU_Pi|QU_Pf), _, _ -> false
      | (* LB20 *)  _, _, _, CB, _ -> true
      |             _, _, CB, _, _ -> true
      | (* LB21 *)  _, _, _, (BA|HY|NS), _ -> false
      |             _, _, BB, _, _ -> false
      | (* LB21a *) _, HL, (HY|BA), _, _ -> false
      | (* LB21b *) _, _, SY, HL, _ -> false
      | (* LB22 *)  _, _, _, IN, _ -> false
      | (* LB23 *)  _, _, (AL|AL_circ|HL), NU, _ -> false
      |             _, _, NU, (AL|AL_circ|HL), _ -> false
      | (* LB23a *) _, _, PR, (ID|ID30b|EB|EM), _ -> false
      |             _, _, (ID|ID30b|EB|EM), PO, _ -> false
      | (* LB24 *)  _, _, (PR|PO), (AL|AL_circ|HL), _ -> false
      |             _, _, (AL|AL_circ|HL), (PR|PO), _ -> false
      | (* LB25 *)  _, _, (CL|CP|CP30|NU), (PO|PR), _ -> false
      |             _, _, (PO|PR), (OP|OP30), _ -> false
      |             _, _, (PO|PR|HY|IS|NU|SY), NU, _ -> false
      | (* LB26 *)  _, _, JL, (JL|JV|H2|H3), _ -> false
      |             _, _, (JV|H2), (JV|JT), _ -> false
      |             _, _, (JT|H3), JT, _ -> false
      | (* LB27 *)  _, _, (JL|JV|JT|H2|H3), PO, _ -> false
      |             _, _, PR, (JL|JV|JT|H2|H3), _ -> false
      | (* LB28 *)  _, _, (AL|AL_circ|HL), (AL|AL_circ|HL), _ -> false
      | (* LB28a *) _, _, AP, (AK|AL_circ|AS), _ -> false
      |             _, _, (AK|AL_circ|AS), (VF|VI), _ -> false
      |             _, (AK|AL_circ|AS), VI, (AK|AL_circ), _ -> false
      |             _, _, (AK|AL_circ|AS), (AK|AL_circ|AS), VF -> false
      | (* LB29 *)  _, _, IS, (AL|AL_circ|HL), _ -> false
      | (* LB30 *)  _, _, (AL|AL_circ|HL|NU), OP30, _ -> false
      |             _, _, CP30, (AL|AL_circ|HL|NU), _ -> false
      | (* LB30a *) _, _, RI, RI, _ when s.l0_odd_ri -> false
      | (* LB30b *) _, _, EB, EM, _ -> false
      |             _, _, ID30b, EM, _ -> false
      | (* LB31 *)  _, _, _, _, _ -> true

let next s = (* moves to the next boundary *)
  s.l2 <- s.l1;
  s.l1 <- s.l0;
  s.l0 <- s.r0;
  (* Only move rewrite window if l0_rewrite doesn't absorb the char
     by rule LB9 or the various SP* *)
  begin match s.r0 with
  | CM | ZWJ when is_lb9_X s.l0_rewrite -> ()
  | SP when s.l0_rewrite = SP -> ()
  | _ ->
      s.l2_rewrite <- s.l1_rewrite;
      s.l1_rewrite <- s.l0_rewrite;
      s.l0_rewrite <- s.r0;
      s.l0_odd_ri <-
        (match s.l0_rewrite with RI -> not s.l0_odd_ri | _ -> false);
  end;
  let data = s.r0_data in
  s.r0 <- s.r1;
  s.r0_data <- s.r1_data;
  s.r1 <- Invalid;
  s.r1_data <- nul_buf;
  (data :> Uuseg_base.ret)

let need_fill s = s.r0 = Invalid || s.r1 = Invalid

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
    | Fill when s.r0 = Invalid -> s.r0_data <- add; s.r0 <- line u; `Await
    | Fill -> s.r1_data <- add; s.r1 <- line u; decide s
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
        (if s.r0 = Invalid then s.r0 <- Eot else s.r1 <- Eot);
        if s.l0 = Sot && s.r0 = Eot then (* empty string *) `End else
        decide s
    | Flush | Decide -> Uuseg_base.err_exp_await `End
    end

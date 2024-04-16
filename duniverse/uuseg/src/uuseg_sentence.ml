(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* These are the rules as found in [1], with property values aliases [2]
   substituted.

   SB1.                          sot ÷ Any
   SB2.                          Any ÷ eot
   SB3.                           CR × LF
   SB4.                   (SE|CR|LF) ÷
   SB5.                   X (EX|FO)* → X
   SB6.                           AT × NU
   SB7.                   (UP|LO) AT × UP

   SB8.                   AT CL* SP* × (¬(LE|UP|LO|SE|CR|LF|ST|AT))* LO
    rewrite w.o. ¬        AT CL* SP* × (CL|NU|SC|SP|XX)* LO
    factorize             AT CL* SP* × (NU|XX)? (CL|NU|SC|SP|XX)* LO
   SB8a.             (ST|AT) CL* SP* × (SC|ST|AT)
   SB9.                  (ST|AT) CL* × (CL|SP|SE|CR|LF)
   SB10.             (ST|AT) CL* SP* × (SP|SE|CR|LF)
   SB11. (ST|AT) CL* SP* (SE|CR|LF)? ÷
   SB12.                         Any × Any


   [1]: http://www.unicode.org/reports/tr29/#Sentence_boundaries
   [2]: http://www.unicode.org/Public/7.0.0/ucd/PropertyValueAliases.txt

   Given the structure of the rules we keep a window of three break
   property value slots, two on the left, one on the right of a
   boundary and pattern match these slots to find the rule that
   applies. the rules SB8 to SB11 are handled separately, and we keep
   a buffer to handle the lookahead needed for SB8.
                             ---??--->
                      +----+----++----+-------...
                      | l1 | l0 || r0 | SB8_buffer
                      +----+----++----+-------...
    already returned to client /  \ buffered in segmenter *)

type sentence =
  | AT | CL | CR | EX | FO | LE | LF | LO | NU | SC | SE | SP | ST | UP | XX
  | Invalid | Sot | Eot

(* WARNING. The indexes used here need to be synchronized with those
   assigned by uucp for Uucp.Break.Low.sentence. *)

let byte_to_sentence =
  [| AT; CL; CR; EX; FO; LE; LF; LO; NU; SC; SE; SP; ST; UP; XX|]

let sentence u = byte_to_sentence.(Uucp.Break.Low.sentence u)

type state =
  | Fill       (* fill slots on the right of boundary *)
  | Fill_CL_SP (* ad-hoc state to handle SB8 to SB11 *)
  | Fill_SB8   (* ad-hoc state to handle SB8 *)
  | Flush      (* flush slot r0 to get to next boundary. *)
  | End        (* `End was added. *)

type t =
  { mutable state : state;                                 (* current state. *)
    window : sentence array;                                (* break window. *)
    mutable l0 : int;                          (* index in [window] of [l0]. *)
    r0_buf : Uuseg_buf.t;                                  (* buffer for r0. *)
    sb8_buf : Uuseg_buf.t; }                    (* buffer for resolving SB8. *)

let create () =
  { state = Fill;
    window = [|Invalid; Sot; Invalid;|];
    l0 = 1;
    r0_buf = Uuseg_buf.create 13;
    sb8_buf = Uuseg_buf.create 13; }

let copy s =
  { s with window = Array.copy s.window;
           r0_buf = Uuseg_buf.copy s.r0_buf;
           sb8_buf = Uuseg_buf.copy s.sb8_buf; }

let l0_sentence s = s.window.(s.l0)
let r0_sentence s = s.window.((s.l0 + 1) mod Array.length s.window)
let r0_sentence_set s l = s.window.((s.l0 + 1) mod Array.length s.window) <- l
let r0_add s add = Uuseg_buf.add s.r0_buf add
let r0_empty s = Uuseg_buf.empty s.r0_buf
let r0_len s = Uuseg_buf.len s.r0_buf
let r0_flush s = Uuseg_buf.flush s.r0_buf
let window_move s =
  s.l0 <- (s.l0 + 1) mod Array.length s.window;
  r0_sentence_set s Invalid

(* WARNING. The code that follows is truly horrible, with more time a
   better way can certainly be found. *)

let decide_sb8_sb11 s sentence (`Uchar _ as add) =
  (* AT or ST is in l0 and we have (AT|ST) CL* SP* and
     sentence <> CL | SP | EX | FO *)
  match sentence with
  | SC | ST | AT (* SB8a *)
  | SE | CR | LF (* SB9 SB10 *) ->
      s.state <- Flush; r0_sentence_set s sentence; add
  | LO when l0_sentence s = AT (* SB8 *) ->
      s.state <- Flush; r0_sentence_set s sentence; add
  | NU | XX when l0_sentence s = AT (* check SB8 *) ->
      s.state <- Fill_SB8; Uuseg_buf.add s.sb8_buf add; `Await
  | NU | XX | LE | UP | LO (* SB11 *) ->
      s.state <- Flush; r0_sentence_set s sentence; r0_add s add; `Boundary
  | CL | SP | EX | FO | Invalid | Sot | Eot ->
      assert false

let decide s =
  let no_boundary s = r0_flush s in
  let wlen = Array.length s.window in
  let l0 = s.l0 in
  let r0 = (l0 + 1) mod wlen in
  let l1 = (l0 + 2) mod wlen in
  let w = s.window in
  match w.(l1), w.(l0) (**),(**) w.(r0) with
  | (* SB1 *) _, Sot, _ -> `Boundary
  (* SB2 is handled in [add]. *)
  | (* SB3 *) _, CR, LF -> no_boundary s
  | (* SB4 *) _, (SE|CR|LF), _ -> `Boundary
  (* SB5 is handled in [add]. *)
  | (* SB6 *)  _, AT, NU -> no_boundary s
  | (* SB7 *) (UP|LO), AT, UP -> no_boundary s
  | (* SB8-SB11 is also handled in [add]. *)
               _, (AT|ST), sentence -> decide_sb8_sb11 s sentence (r0_flush s)
  | (* SB12 *) _, _, _ -> no_boundary s

let rec add s = function
| `Uchar u as addv ->
    begin match s.state with
    | Fill ->
        begin match sentence u with
        | EX | FO as sentence ->
            begin match l0_sentence s with
            | SE | CR | LF | Sot ->
                s.state <- Flush; r0_sentence_set s sentence; r0_add s addv;
                decide s
            | _ -> addv (* SB5 *)
            end
        | CL | SP as sentence ->
            begin match l0_sentence s with
            | AT | ST ->
                s.state <- Fill_CL_SP; r0_sentence_set s sentence;
                addv (* SB9, SB10 *)
            | _ ->
                s.state <- Flush; r0_sentence_set s sentence; r0_add s addv;
                decide s
            end
        | sentence ->
            s.state <- Flush; r0_sentence_set s sentence; r0_add s addv;
            decide s
        end
    | Fill_CL_SP (* we have AT or ST in l0 *) ->
        begin match sentence u with
        | EX | FO -> addv (* SB5 *)
        | CL ->
            begin match r0_sentence s with
            | CL -> addv (* SB9 (or eventually SB8) *)
            | SP ->
                begin match l0_sentence s with
                | ST -> (* SB11 *)
                    s.state <- Flush; r0_sentence_set s CL; r0_add s addv;
                    `Boundary
                | AT -> (* check SB8 *)
                    s.state <- Fill_SB8; Uuseg_buf.add s.sb8_buf addv; `Await
                | _ -> assert false
                end
            | _ -> assert false
            end
        | SP ->
            begin match r0_sentence s with
            | CL -> r0_sentence_set s SP; addv (* SB10 (or eventually SB8 *)
            | SP -> addv
            | _ -> assert false
            end
        | sentence ->
            begin match r0_sentence s with
            | CL | SP -> decide_sb8_sb11 s sentence addv
            |_ -> assert false
            end
        end
    | Fill_SB8 ->
        begin match sentence u with
        | EX | FO (* SB5 *)
        | CL | NU | SC | SP | XX -> Uuseg_buf.add s.sb8_buf addv; `Await
        | LO -> (* SB8 *)
            Uuseg_buf.add s.sb8_buf addv; s.state <- Flush; add s `Await
        | _ -> (* SB11 *)
            Uuseg_buf.add s.sb8_buf addv; s.state <- Flush; `Boundary
        end
    | Flush -> Uuseg_base.err_exp_await addv
    | End -> Uuseg_base.err_ended addv
    end
| `Await ->
    begin match s.state with
    | Flush ->
        if not (r0_empty s) then r0_flush s else
        begin
          match Uuseg_buf.len s.sb8_buf with
          | 0 ->  s.state <- Fill; window_move s; `Await
          | 1 ->
              let u = Uuseg_buf.flush s.sb8_buf in
              s.state <- Fill; window_move s; add s u
          | _ ->
              let `Uchar uc as u = Uuseg_buf.flush s.sb8_buf in
              r0_sentence_set s (sentence uc); u
        end
    | End ->
        if not (Uuseg_buf.empty s.sb8_buf)
        then
          (* According to SB8 we bufferized only CL|NU|SC|SP|XX
             given the rules no new boundary will occur except the one
             due to SB2. So we just flush the sb8_buf. *)
          Uuseg_buf.flush s.sb8_buf
        else
        if r0_sentence s = Eot
        then (r0_sentence_set s Invalid; `Boundary (* SB2 *))
        else `End
    | Fill | Fill_CL_SP | Fill_SB8 -> `Await
    end
| `End ->
    begin match s.state with
    | Fill ->
        s.state <- End;
        if l0_sentence s = Sot then `End (* No boundary on empty seq *) else
        `Boundary (* SB1 and SB2 *)
    | Fill_CL_SP -> s.state <- End; `Boundary (* SB2 *)
    | Fill_SB8 -> s.state <- End; r0_sentence_set s Eot; `Boundary (* SB11 *)
    | Flush -> Uuseg_base.err_exp_await `End
    | End -> Uuseg_base.err_ended `End
    end
